{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Algorithm.Diff (Diff(..))
import Data.Attoparsec.Text (IResult(..))
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Filesystem.Path (FilePath)
import Nix.Derivation (Derivation, DerivationOutput)
import Options.Generic (Generic, ParseRecord)
import Prelude hiding (FilePath)

import qualified Data.Algorithm.Diff
import qualified Data.Attoparsec.Text
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.IO
import qualified Filesystem.Path.CurrentOS
import qualified Nix.Derivation
import qualified Options.Generic
import qualified System.Posix.IO
import qualified System.Posix.Terminal

data Options w = Options FilePath FilePath
    deriving (Generic, ParseRecord)

pathToText :: FilePath -> Text
pathToText path =
    case Filesystem.Path.CurrentOS.toText path of
        Left  text -> text
        Right text -> text

{-| Extract the name of a derivation (i.e. the part after the hash)

    This is used to guess which derivations are related to one another, even
    though their hash might differ

    Note that this assumes that the path name is:

    > /nix/store/${32_CHARACTER_HASH}-${NAME}.drv

    Nix technically does not require that the Nix store is actually stored
    underneath `/nix/store`, but this is the overwhelmingly common use case
-}
derivationName :: FilePath -> Text
derivationName = Data.Text.dropEnd 4 . Data.Text.drop 44 . pathToText

-- | Group input derivations by their name
groupByName :: Map FilePath (Set Text) -> Map Text (Map FilePath (Set Text))
groupByName m = Data.Map.fromList assocs
  where
    toAssoc key = (derivationName key, Data.Map.filterWithKey predicate m)
      where
        predicate key' _ = derivationName key == derivationName key'

    assocs = fmap toAssoc (Data.Map.keys m)

-- | Read and parse a derivation from a file
readDerivation :: FilePath -> IO Derivation
readDerivation path = do
    let string = Filesystem.Path.CurrentOS.encodeString path
    text <- Data.Text.IO.readFile string
    case Data.Attoparsec.Text.parse Nix.Derivation.parseDerivation text of
        Done _ derivation -> do
            return derivation
        _ -> do
            fail ("Could not parse a derivation from this file: " ++ string)

{-| Join two `Map`s on shared keys, discarding keys which are not present in
    both `Map`s
-}
innerJoin :: Ord k => Map k a -> Map k b -> Map k (a, b)
innerJoin = Data.Map.mergeWithKey both left right
  where
    both _ a b = Just (a, b)

    left _ = Data.Map.empty

    right _ = Data.Map.empty

data TTY = IsTTY | NotTTY

-- | Color text red
red :: TTY -> Text -> Text
red  IsTTY text = "\ESC[1;31m" <> text <> "\ESC[0m"
red NotTTY text = text

-- | Color text background red
redBackground  :: TTY -> Text -> Text
redBackground  IsTTY text = "\ESC[41m" <> text <> "\ESC[0m"
redBackground NotTTY text = "←" <> text <> "←"

-- | Color text green
green :: TTY -> Text -> Text
green  IsTTY text = "\ESC[1;32m" <> text <> "\ESC[0m"
green NotTTY text = text

-- | Color text background green
greenBackground :: TTY -> Text -> Text
greenBackground  IsTTY text = "\ESC[42m" <> text <> "\ESC[0m"
greenBackground NotTTY text = "→" <> text <> "→"

-- | Color text grey
grey :: TTY -> Text -> Text
grey  IsTTY text = "\ESC[1;2m" <> text <> "\ESC[0m"
grey NotTTY text = text

-- | Format the left half of a diff
minus :: TTY -> Text -> Text
minus tty text = red tty ("- " <> text)

-- | Format the right half of a diff
plus :: TTY -> Text -> Text
plus tty text = green tty ("+ " <> text)

-- | Format text explaining a diff
explain :: Text -> Text
explain text = "• " <> text

{-| Utility to automate a common pattern of printing the two halves of a diff.
    This passes the correct formatting function to each half
-}
diffWith :: Monad m => TTY -> a -> a -> ((Text -> Text, a) -> m ()) -> m ()
diffWith tty l r k = do
    k (minus tty, l)
    k (plus  tty, r)

-- | Format the derivation outputs
renderOutputs :: Set Text -> Text
renderOutputs outputs =
    ":{" <> Data.Text.intercalate "," (Data.Set.toList outputs) <> "}"

-- | Diff two outputs
diffOutput
    :: TTY
    -- ^ Whether or not we are writing to a terminal
    -> Int
    -- ^ Current indentation level
    -> Text
    -- ^ Output name
    -> DerivationOutput
    -- ^ Left derivation outputs
    -> DerivationOutput
    -- ^ Right derivation outputs
    -> IO ()
diffOutput tty indent outputName leftOutput rightOutput = do
    -- We deliberately do not include output paths or hashes in the diff since
    -- we already expect them to differ if the inputs differ.  Instead, we focus
    -- only displaying differing inputs.
    let leftHashAlgo  = Nix.Derivation.hashAlgo leftOutput
    let rightHashAlgo = Nix.Derivation.hashAlgo rightOutput
    if leftHashAlgo == rightHashAlgo
    then return ()
    else do
        echo (explain ("{" <> outputName <> "}:"))
        echo (explain "    Hash algorithm:")
        diffWith tty leftHashAlgo rightHashAlgo $ \(sign, hashAlgo) -> do
            echo ("        " <> sign hashAlgo)
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

-- | Diff two sets of outputs
diffOutputs
    :: TTY
    -- ^ Whether or not we are writing to a terminal
    -> Int
    -- ^ Current indentation level
    -> Map Text DerivationOutput
    -- ^ Left derivation outputs
    -> Map Text DerivationOutput
    -- ^ Right derivation outputs
    -> IO ()
diffOutputs tty indent leftOutputs rightOutputs = do
    let leftExtraOutputs  = Data.Map.difference leftOutputs  rightOutputs
    let rightExtraOutputs = Data.Map.difference rightOutputs leftOutputs

    let bothOutputs = innerJoin leftOutputs rightOutputs

    if Data.Map.null leftExtraOutputs && Data.Map.null rightExtraOutputs
        then return ()
        else do
            echo (explain "The set of outputs do not match:")
            diffWith tty leftExtraOutputs rightExtraOutputs $ \(sign, extraOutputs) -> do
                forM_ (Data.Map.toList extraOutputs) $ \(key, _value) -> do
                    echo ("    " <> sign ("{" <> key <> "}"))
    forM_ (Data.Map.toList bothOutputs) $ \(key, (leftOutput, rightOutput)) -> do
        if leftOutput == rightOutput
        then return ()
        else do
            diffOutput tty indent key leftOutput rightOutput
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

-- | Diff two `Text` values
diffText
    :: TTY
    -- ^ Whether or not we are writing to a terminal
    -> Int
    -- ^ Current indentation level
    -> Text
    -- ^ Left value to compare
    -> Text
    -- ^ Right value to compare
    -> Text
diffText tty indent left right = format (Data.Text.concat (fmap renderChunk chunks))
  where
    leftString  = Data.Text.unpack left
    rightString = Data.Text.unpack right

    chunks = Data.Algorithm.Diff.getGroupedDiff leftString rightString

    format text =
        if 80 <= indent + Data.Text.length text
        then "''\n" <> indentedText <> prefix <> "''"
        else text
      where
        indentedText =
            (Data.Text.unlines . fmap indentLine . Data.Text.lines) text
          where
            indentLine line = prefix <> "    " <> line

    prefix = Data.Text.replicate indent " "

    renderChunk (First  l) = redBackground   tty (Data.Text.pack l)
    renderChunk (Second r) = greenBackground tty (Data.Text.pack r)
    renderChunk (Both l _) = grey            tty (Data.Text.pack l)

-- | Diff two environments
diffEnv
    :: TTY
    -- ^ Whether or not we are writing to a terminal
    -> Int
    -- ^ Current indentation level
    -> Set Text
    -- ^ Left derivation outputs (used to exclude them from diff)
    -> Set Text
    -- ^ Right derivation outputs (used to exclude them from diff)
    -> Map Text Text
    -- ^ Left environment to compare
    -> Map Text Text
    -- ^ Right environment to compare
    -> IO ()
diffEnv tty indent leftOutputs rightOutputs leftEnv rightEnv = do
    let leftExtraEnv  = Data.Map.difference leftEnv  rightEnv
    let rightExtraEnv = Data.Map.difference rightEnv leftEnv

    let bothEnv = innerJoin leftEnv rightEnv

    diffWith tty leftExtraEnv rightExtraEnv $ \(sign, extraEnv) -> do
        forM_ (Data.Map.toList extraEnv) $ \(key, value) -> do
            echo (sign (key <> "=" <> value))
    forM_ (Data.Map.toList bothEnv) $ \(key, (leftValue, rightValue)) -> do
        if      leftValue == rightValue
            ||  (   Data.Set.member key leftOutputs
                &&  Data.Set.member key rightOutputs
                )
        then return ()
        else echo (key <> "=" <> diffText tty indent leftValue rightValue)
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

diff :: TTY -> Int -> FilePath -> Set Text -> FilePath -> Set Text -> IO ()
diff tty indent leftPath leftOutputs rightPath rightOutputs = do
    if leftPath == rightPath
    then return ()
    else do
        diffWith tty (leftPath, leftOutputs) (rightPath, rightOutputs) $ \(sign, (path, outputs)) -> do
            echo (sign (pathToText path <> renderOutputs outputs))

        if derivationName leftPath /= derivationName rightPath
        then do
            echo (explain "The derivation names do not match")
        else if leftOutputs /= rightOutputs
        then do
            echo (explain "The requested outputs do not match")
        else do
            leftDerivation  <- readDerivation leftPath
            rightDerivation <- readDerivation rightPath
            let leftInputs  = groupByName (Nix.Derivation.inputDrvs leftDerivation)
            let rightInputs = groupByName (Nix.Derivation.inputDrvs rightDerivation)
    
            let leftNames  = Data.Map.keysSet leftInputs
            let rightNames = Data.Map.keysSet rightInputs
            let leftExtraNames  = Data.Set.difference leftNames  rightNames
            let rightExtraNames = Data.Set.difference rightNames leftNames

            if leftNames /= rightNames
            then do
                echo (explain "The set of input names do not match:")
                diffWith tty leftExtraNames rightExtraNames $ \(sign, names) -> do
                    forM_ names $ \name -> do
                        echo ("    " <> sign name)
            else do 
                let assocs = Data.Map.toList (innerJoin leftInputs rightInputs)
                descended <- forM assocs $ \(inputName, (leftPaths, rightPaths)) -> do
                    let leftExtraPaths =
                            Data.Map.difference leftPaths  rightPaths
                    let rightExtraPaths =
                            Data.Map.difference rightPaths leftPaths
                    case (Data.Map.toList leftExtraPaths, Data.Map.toList rightExtraPaths) of
                        _   | leftPaths == rightPaths -> do
                            return False
                        ([(leftPath', leftOutputs')], [(rightPath', rightOutputs')])
                            | leftOutputs' == rightOutputs' -> do
                            echo (explain ("The input named `" <> inputName <> "` differs"))
                            diff tty (indent + 2) leftPath' leftOutputs' rightPath' rightOutputs'
                            return True
                        _ -> do
                            echo (explain ("The set of inputs named `" <> inputName <> "` do not match"))
                            diffWith tty leftExtraPaths rightExtraPaths $ \(sign, extraPaths) -> do
                                forM_ (Data.Map.toList extraPaths) $ \(extraPath, outputs) -> do
                                    echo ("    " <> sign (pathToText extraPath <> renderOutputs outputs))
                            return False
                if or descended
                then return ()
                else do
                    let leftOuts = Nix.Derivation.outputs leftDerivation
                    let rightOuts = Nix.Derivation.outputs rightDerivation
                    diffOutputs tty indent leftOuts rightOuts

                    let leftEnv  = Nix.Derivation.env leftDerivation
                    let rightEnv = Nix.Derivation.env rightDerivation
                    let leftOutNames  = Data.Map.keysSet leftOuts
                    let rightOutNames = Data.Map.keysSet rightOuts
                    diffEnv tty indent leftOutNames rightOutNames leftEnv rightEnv
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

main :: IO ()
main = do
    Options left right <- Options.Generic.unwrapRecord "Explain why two derivations differ"
    b <- System.Posix.Terminal.queryTerminal System.Posix.IO.stdOutput
    let tty = if b then IsTTY else NotTTY
    diff tty 0 left (Data.Set.singleton "out") right (Data.Set.singleton "out")
