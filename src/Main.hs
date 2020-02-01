{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local)
import Control.Monad.State (MonadState, StateT, get, put)
import Data.Attoparsec.Text (IResult(..))
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Filesystem.Path (FilePath)
import Nix.Derivation (Derivation, DerivationOutput)
import Numeric.Natural (Natural)
import Options.Applicative (Parser, ParserInfo)
import Prelude hiding (FilePath)

import qualified Control.Monad.Reader
import qualified Control.Monad.State
import qualified Data.Algorithm.Diff       as Diff
import qualified Data.Attoparsec.Text
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Vector
import qualified Filesystem.Path.CurrentOS
import qualified GHC.IO.Encoding
import qualified Nix.Derivation
import qualified Options.Applicative
import qualified System.Posix.IO
import qualified System.Posix.Terminal

data Color = Always | Auto | Never

parseColor :: Parser Color
parseColor =
    Options.Applicative.option
        reader
        (   Options.Applicative.long "color"
        <>  Options.Applicative.value Auto
        <>  Options.Applicative.metavar "(always|auto|never)"
        )
  where
    reader = do
        string <- Options.Applicative.str
        case string of
            "always" -> return Always
            "auto"   -> return Auto
            "never"  -> return Never
            _        -> fail "Invalid color"

parseLineOriented :: Parser Bool
parseLineOriented =
    Options.Applicative.switch
        (   Options.Applicative.long "line-oriented"
        <>  Options.Applicative.help "Display textual differences on a per-line basis instead of a per-character basis"
        )

parseEnvironment :: Parser Bool
parseEnvironment =
    Options.Applicative.switch
        (   Options.Applicative.long "environment"
        <>  Options.Applicative.help "Force display of environment differences"
        )

data Options = Options
    { left         :: FilePath
    , right        :: FilePath
    , color        :: Color
    , lineOriented :: Bool
    , environment  :: Bool
    }

parseOptions :: Parser Options
parseOptions = do
    left         <- parseLeft
    right        <- parseRight
    color        <- parseColor
    lineOriented <- parseLineOriented
    environment  <- parseEnvironment

    return (Options { left, right, color, lineOriented, environment })
  where
    parseFilePath metavar = do
        Options.Applicative.strArgument
            (Options.Applicative.metavar metavar)

    parseLeft = parseFilePath "LEFT"

    parseRight = parseFilePath "RIGHT"

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        parseOptions
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.header "Explain why two derivations differ"
        )

data Context = Context
    { tty          :: TTY
    , indent       :: Natural
    , lineOriented :: Bool
    , environment  :: Bool
    }

newtype Status = Status { visited :: Set Diffed }

data Diffed = Diffed
    { leftDerivation  :: FilePath
    , leftOutput      :: Set Text
    , rightDerivation :: FilePath
    , rightOutput     :: Set Text
    } deriving (Eq, Ord)

newtype Diff a = Diff { unDiff :: ReaderT Context (StateT Status IO) a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Context
    , MonadState Status
    , MonadIO
    )

echo :: Text -> Diff ()
echo text = do
    Context { indent } <- ask
    let n = fromIntegral indent
    liftIO (Data.Text.IO.putStrLn (Data.Text.replicate n " " <> text))

indented :: Natural -> Diff a -> Diff a
indented n = local adapt
  where
    adapt context = context { indent = indent context + n }

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
readDerivation :: FilePath -> Diff Derivation
readDerivation path = do
    let string = Filesystem.Path.CurrentOS.encodeString path
    text <- liftIO (Data.Text.IO.readFile string)
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
redBackground  :: Bool -> TTY -> Text -> Text
redBackground False IsTTY  text = "\ESC[41m" <> text <> "\ESC[0m"
redBackground True  IsTTY  text = "\ESC[41m" <> text <> "\ESC[0m\n"
redBackground False NotTTY text = "←" <> text <> "←"
redBackground True  NotTTY text = "- " <> text <> "\n"

-- | Color text green
green :: TTY -> Text -> Text
green  IsTTY text = "\ESC[1;32m" <> text <> "\ESC[0m"
green NotTTY text = text

-- | Color text background green
greenBackground :: Bool -> TTY -> Text -> Text
greenBackground False IsTTY  text = "\ESC[42m" <> text <> "\ESC[0m"
greenBackground True  IsTTY  text = "\ESC[42m" <> text <> "\ESC[0m\n"
greenBackground False NotTTY text = "→" <> text <> "→"
greenBackground True  NotTTY text = "+ " <> text <> "\n"

-- | Color text grey
grey :: Bool -> TTY -> Text -> Text
grey False  IsTTY text = "\ESC[1;2m" <> text <> "\ESC[0m"
grey True   IsTTY text = "\ESC[1;2m" <> text <> "\ESC[0m\n"
grey False NotTTY text = text
grey True  NotTTY text = "  " <> text <> "\n"

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
diffWith :: a -> a -> ((Text -> Text, a) -> Diff ()) -> Diff ()
diffWith l r k = do
    Context { tty } <- ask
    k (minus tty, l)
    k (plus  tty, r)

-- | Format the derivation outputs
renderOutputs :: Set Text -> Text
renderOutputs outputs =
    ":{" <> Data.Text.intercalate "," (Data.Set.toList outputs) <> "}"

-- | Diff two outputs
diffOutput
    :: Text
    -- ^ Output name
    -> DerivationOutput
    -- ^ Left derivation outputs
    -> DerivationOutput
    -- ^ Right derivation outputs
    -> Diff ()
diffOutput outputName leftOutput rightOutput = do
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
        diffWith leftHashAlgo rightHashAlgo $ \(sign, hashAlgo) -> do
            echo ("        " <> sign hashAlgo)

-- | Diff two sets of outputs
diffOutputs
    :: Map Text DerivationOutput
    -- ^ Left derivation outputs
    -> Map Text DerivationOutput
    -- ^ Right derivation outputs
    -> Diff ()
diffOutputs leftOutputs rightOutputs = do
    let leftExtraOutputs  = Data.Map.difference leftOutputs  rightOutputs
    let rightExtraOutputs = Data.Map.difference rightOutputs leftOutputs

    let bothOutputs = innerJoin leftOutputs rightOutputs

    if Data.Map.null leftExtraOutputs && Data.Map.null rightExtraOutputs
        then return ()
        else do
            echo (explain "The set of outputs do not match:")
            diffWith leftExtraOutputs rightExtraOutputs $ \(sign, extraOutputs) -> do
                forM_ (Data.Map.toList extraOutputs) $ \(key, _value) -> do
                    echo ("    " <> sign ("{" <> key <> "}"))
    forM_ (Data.Map.toList bothOutputs) $ \(key, (leftOutput, rightOutput)) -> do
        if leftOutput == rightOutput
        then return ()
        else diffOutput key leftOutput rightOutput

mapDiff :: (a -> b) -> Diff.Diff a -> Diff.Diff b
mapDiff f (Diff.First  l) = Diff.First (f l)
mapDiff f (Diff.Second r) = Diff.Second (f r)
mapDiff f (Diff.Both l r) = Diff.Both (f l) (f r)

-- | Diff two `Text` values
diffText
    :: Text
    -- ^ Left value to compare
    -> Text
    -- ^ Right value to compare
    -> Diff Text
diffText left right = do
    Context { indent, lineOriented, tty } <- ask
    let n = fromIntegral indent

    let leftString  = Data.Text.unpack left
    let rightString = Data.Text.unpack right

    let leftLines  = Data.Text.lines left
    let rightLines = Data.Text.lines right

    let chunks =
            if lineOriented
                then
                    Diff.getDiff leftLines rightLines
                else
                    fmap (mapDiff Data.Text.pack) (Diff.getGroupedDiff leftString rightString)

    let prefix = Data.Text.replicate n " "

    let format text =
            if 80 <= n + Data.Text.length text
            then "''\n" <> indentedText <> prefix <> "''"
            else text
          where
            indentedText =
                (Data.Text.unlines . fmap indentLine . Data.Text.lines) text
              where
                indentLine line = prefix <> "    " <> line

    let renderChunk (Diff.First  l) =
            redBackground   lineOriented tty l
        renderChunk (Diff.Second r) =
            greenBackground lineOriented tty r
        renderChunk (Diff.Both l _) =
            grey            lineOriented tty l

    return (format (Data.Text.concat (fmap renderChunk chunks)))

-- | Diff two environments
diffEnv
    :: Set Text
    -- ^ Left derivation outputs
    -> Set Text
    -- ^ Right derivation outputs
    -> Map Text Text
    -- ^ Left environment to compare
    -> Map Text Text
    -- ^ Right environment to compare
    -> Diff ()
diffEnv leftOutputs rightOutputs leftEnv rightEnv = do
    let leftExtraEnv  = Data.Map.difference leftEnv  rightEnv
    let rightExtraEnv = Data.Map.difference rightEnv leftEnv

    let bothEnv = innerJoin leftEnv rightEnv

    let predicate key (left, right) =
                left == right
            ||  (   Data.Set.member key leftOutputs
                &&  Data.Set.member key rightOutputs
                )
            ||  key == "builder"
            ||  key == "system"

    if     Data.Map.null leftExtraEnv
        && Data.Map.null rightExtraEnv
        && Data.Map.null
               (Data.Map.filterWithKey (\k v -> not (predicate k v)) bothEnv)
    then return ()
    else do
        echo (explain "The environments do not match:")
        diffWith leftExtraEnv rightExtraEnv $ \(sign, extraEnv) -> do
            forM_ (Data.Map.toList extraEnv) $ \(key, value) -> do
                echo ("    " <> sign (key <> "=" <> value))
        forM_ (Data.Map.toList bothEnv) $ \(key, (leftValue, rightValue)) -> do
            if      predicate key (leftValue, rightValue)
            then return ()
            else do
                text <- indented 4 (diffText leftValue rightValue)
                echo ("    " <> key <> "=" <> text)

-- | Diff input sources
diffSrcs
    :: Set FilePath
    -- ^ Left input sources
    -> Set FilePath
    -- ^ Right inputSources
    -> Diff ()
diffSrcs leftSrcs rightSrcs = do
    let leftExtraSrcs  = Data.Set.difference leftSrcs  rightSrcs
    let rightExtraSrcs = Data.Set.difference rightSrcs leftSrcs

    if Data.Set.null leftExtraSrcs && Data.Set.null rightExtraSrcs
        then return ()
        else do
            echo (explain "The set of input sources do not match:")
            diffWith leftExtraSrcs rightExtraSrcs $ \(sign, extraSrcs) -> do
                forM_ extraSrcs $ \extraSrc -> do
                    echo ("    " <> sign (pathToText extraSrc))

diffPlatform :: Text -> Text -> Diff ()
diffPlatform leftPlatform rightPlatform = do
    if leftPlatform == rightPlatform
    then return ()
    else do
        echo (explain "The platforms do not match")
        diffWith leftPlatform rightPlatform $ \(sign, platform) -> do
            echo ("    " <> sign platform)

diffBuilder :: Text -> Text -> Diff ()
diffBuilder leftBuilder rightBuilder = do
    if leftBuilder == rightBuilder
    then return ()
    else do
        echo (explain "The builders do not match")
        diffWith leftBuilder rightBuilder $ \(sign, builder) -> do
            echo ("    " <> sign builder)

diffArgs :: Vector Text -> Vector Text -> Diff ()
diffArgs leftArgs rightArgs = do
    Context { tty } <- ask
    if leftArgs == rightArgs
    then return ()
    else do
        echo (explain "The arguments do not match")
        let leftList  = Data.Vector.toList leftArgs
        let rightList = Data.Vector.toList rightArgs
        let diffs = Diff.getDiff leftList rightList
        let renderDiff (Diff.First arg) =
                echo ("    " <> minus tty arg)
            renderDiff (Diff.Second arg) =
                echo ("    " <> plus tty arg)
            renderDiff (Diff.Both arg _) =
                echo ("    " <> explain arg)
        mapM_ renderDiff diffs

diff :: Bool -> FilePath -> Set Text -> FilePath -> Set Text -> Diff ()
diff topLevel leftPath leftOutputs rightPath rightOutputs = do
    Status { visited } <- get
    let diffed = Diffed leftPath leftOutputs rightPath rightOutputs
    if leftPath == rightPath
    then return ()
    else if Data.Set.member diffed visited
    then do
        echo (explain "These two derivations have already been compared")
    else do
        put (Status (Data.Set.insert diffed visited))
        diffWith (leftPath, leftOutputs) (rightPath, rightOutputs) $ \(sign, (path, outputs)) -> do
            echo (sign (pathToText path <> renderOutputs outputs))

        if derivationName leftPath /= derivationName rightPath && not topLevel
        then do
            echo (explain "The derivation names do not match")
        else if leftOutputs /= rightOutputs
        then do
            echo (explain "The requested outputs do not match")
        else do
            leftDerivation  <- readDerivation leftPath
            rightDerivation <- readDerivation rightPath

            let leftOuts = Nix.Derivation.outputs leftDerivation
            let rightOuts = Nix.Derivation.outputs rightDerivation
            diffOutputs leftOuts rightOuts

            let leftPlatform  = Nix.Derivation.platform leftDerivation
            let rightPlatform = Nix.Derivation.platform rightDerivation
            diffPlatform leftPlatform rightPlatform

            let leftBuilder  = Nix.Derivation.builder leftDerivation
            let rightBuilder = Nix.Derivation.builder rightDerivation
            diffBuilder leftBuilder rightBuilder

            let leftArgs  = Nix.Derivation.args leftDerivation
            let rightArgs = Nix.Derivation.args rightDerivation
            diffArgs leftArgs rightArgs

            let leftSrcs  = Nix.Derivation.inputSrcs leftDerivation
            let rightSrcs = Nix.Derivation.inputSrcs rightDerivation
            diffSrcs leftSrcs rightSrcs

            let leftInputs  = groupByName (Nix.Derivation.inputDrvs leftDerivation)
            let rightInputs = groupByName (Nix.Derivation.inputDrvs rightDerivation)

            let leftNames  = Data.Map.keysSet leftInputs
            let rightNames = Data.Map.keysSet rightInputs
            let leftExtraNames  = Data.Set.difference leftNames  rightNames
            let rightExtraNames = Data.Set.difference rightNames leftNames

            Control.Monad.State.when (leftNames /= rightNames) $ do
                echo (explain "The set of input names do not match:")
                diffWith leftExtraNames rightExtraNames $ \(sign, names) -> do
                    forM_ names $ \name -> do
                        echo ("    " <> sign name)

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
                        indented 2 (diff False leftPath' leftOutputs' rightPath' rightOutputs')
                        return True
                    _ -> do
                        echo (explain ("The set of inputs named `" <> inputName <> "` do not match"))
                        diffWith leftExtraPaths rightExtraPaths $ \(sign, extraPaths) -> do
                            forM_ (Data.Map.toList extraPaths) $ \(extraPath, outputs) -> do
                                echo ("    " <> sign (pathToText extraPath <> renderOutputs outputs))
                        return False

            Context { environment } <- ask

            if or descended && not environment
            then do
                echo (explain "Skipping environment comparison")
            else do
                let leftEnv  = Nix.Derivation.env leftDerivation
                let rightEnv = Nix.Derivation.env rightDerivation
                let leftOutNames  = Data.Map.keysSet leftOuts
                let rightOutNames = Data.Map.keysSet rightOuts
                diffEnv leftOutNames rightOutNames leftEnv rightEnv

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Options { left, right, color, lineOriented, environment } <- Options.Applicative.execParser parserInfo

    tty <- case color of
        Never -> do
            return NotTTY
        Always -> do
            return IsTTY
        Auto -> do
            b <- System.Posix.Terminal.queryTerminal System.Posix.IO.stdOutput
            return (if b then IsTTY else NotTTY)

    let indent = 0
    let context = Context { tty, indent, lineOriented, environment }
    let status = Status Data.Set.empty
    let action = diff True left (Data.Set.singleton "out") right (Data.Set.singleton "out")
    Control.Monad.State.evalStateT (Control.Monad.Reader.runReaderT (unDiff action) context) status
