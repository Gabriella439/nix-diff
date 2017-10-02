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
import Nix.Derivation (Derivation)
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

-- | Color text red
red :: Text -> Text
red text = "\ESC[1;31m" <> text <> "\ESC[0m"

-- | Color text green
green :: Text -> Text
green text = "\ESC[1;32m" <> text <> "\ESC[0m"

-- | Color text grey
grey :: Text -> Text
grey text = "\ESC[1;2m" <> text <> "\ESC[0m"

-- | Format the left half of a diff
minus :: Text -> Text
minus text = red ("- " <> text)

-- | Format the right half of a diff
plus :: Text -> Text
plus text = green ("+ " <> text)

-- | Format text explaining a diff
explain :: Text -> Text
explain text = "• " <> text

{-| Utility to automate a common pattern of printing the two halves of a diff.
    This passes the correct formatting function to each half
-}
diffWith :: Monad m => a -> a -> ((Text -> Text, a) -> m ()) -> m ()
diffWith l r k = do
    k (minus, l)
    k (plus , r)

-- | Format the derivation outputs
renderOutputs :: Set Text -> Text
renderOutputs outputs =
    ":{" <> Data.Text.intercalate "," (Data.Set.toList outputs) <> "}"

-- | Diff two `Text` values
diffText
    :: Int
    -- ^ Current indentation level (used to indent multi-line diffs)
    -> Text
    -- ^ Left value to compare
    -> Text
    -- ^ Right value to compare
    -> Text
diffText indent left right = format (Data.Text.concat (fmap renderChunk chunks))
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

    highlight = Data.Text.concatMap adapt
      where
        adapt ' '  = "█"
        adapt '\n' = "█\n"
        adapt '\t' = "█\t"
        adapt  c   =  Data.Text.singleton c

    prefix = Data.Text.replicate indent " "

    renderChunk (First  l) = green (highlight (Data.Text.pack l))
    renderChunk (Second r) = red   (highlight (Data.Text.pack r))
    renderChunk (Both l _) = grey             (Data.Text.pack l)

diffEnv :: Int -> Map Text Text -> Map Text Text -> IO ()
diffEnv indent leftEnv rightEnv = do
    let leftExtraEnv  = Data.Map.difference leftEnv rightEnv
    let rightExtraEnv = Data.Map.difference leftEnv rightEnv

    let bothEnv = innerJoin leftEnv rightEnv

    diffWith leftExtraEnv rightExtraEnv $ \(sign, extraEnv) -> do
        forM_ (Data.Map.toList extraEnv) $ \(key, value) -> do
            echo (sign (key <> "=" <> value))
    forM_ (Data.Map.toList bothEnv) $ \(key, (leftValue, rightValue)) -> do
        if leftValue == rightValue || key == "out"
        then return ()
        else echo (key <> "=" <> diffText indent leftValue rightValue)
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

diff :: Int -> FilePath -> Set Text -> FilePath -> Set Text -> IO ()
diff indent leftPath leftOutputs rightPath rightOutputs = do
    if leftPath == rightPath
    then return ()
    else do
        diffWith (leftPath, leftOutputs) (rightPath, rightOutputs) $ \(sign, (path, outputs)) -> do
            echo (sign (pathToText path <> renderOutputs outputs))

        if derivationName leftPath /= derivationName rightPath
        then do
            echo (explain "The derivation names do not match")

        else do
            leftDerivation  <- readDerivation leftPath
            rightDerivation <- readDerivation rightPath
            let rightInputs = groupByName (Nix.Derivation.inputDrvs leftDerivation)
            let leftInputs  = groupByName (Nix.Derivation.inputDrvs rightDerivation)
    
            let leftNames  = Data.Map.keysSet leftInputs
            let rightNames = Data.Map.keysSet rightInputs
            let leftExtraNames  = Data.Set.difference leftNames  rightNames
            let rightExtraNames = Data.Set.difference rightNames leftNames

            if leftNames /= rightNames
            then do
                echo (explain "The set of input names do not match:")
                diffWith leftExtraNames rightExtraNames $ \(sign, names) -> do
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
                            diff (indent + 2) leftPath' leftOutputs' rightPath' rightOutputs'
                            return True
                        _ -> do
                            echo (explain ("The set of inputs named `" <> inputName <> "` do not match"))
                            diffWith leftExtraPaths rightExtraPaths $ \(sign, extraPaths) -> do
                                forM_ (Data.Map.toList extraPaths) $ \(extraPath, outputs) -> do
                                    echo ("    " <> sign (pathToText extraPath <> renderOutputs outputs))
                            return False
                if or descended
                then return ()
                else do
                    let leftEnv  = Nix.Derivation.env leftDerivation
                    let rightEnv = Nix.Derivation.env rightDerivation
                    diffEnv indent leftEnv rightEnv
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

main :: IO ()
main = do
    Options left right <- Options.Generic.unwrapRecord "Explain why two derivations differ"
    diff 0 left (Data.Set.singleton "out") right (Data.Set.singleton "out")
