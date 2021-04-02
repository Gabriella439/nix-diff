{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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
import Nix.Derivation (Derivation, DerivationOutput)
import Numeric.Natural (Natural)
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Monad        as Monad
import qualified Control.Monad.Reader
import qualified Control.Monad.State
import qualified Data.Attoparsec.Text
import qualified Data.Char            as Char
import qualified Data.Map
import qualified Data.List            as List
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Vector
import qualified GHC.IO.Encoding
import qualified Nix.Derivation
import qualified Options.Applicative
import qualified Patience
import qualified System.Directory     as Directory
import qualified System.Posix.IO
import qualified System.Posix.Terminal

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
#endif

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

parseLineOriented :: Parser Orientation
parseLineOriented =
        per "line" Line
    <|> per "character" Character
    <|> per "word" Word
    <|> pure Word
  where
    per x orientation =
        Options.Applicative.flag' orientation
            (   Options.Applicative.long (x <> "-oriented")
            <>  Options.Applicative.help ("Display textual differences on a per-" <> x <> " basis")
            )

parseEnvironment :: Parser Bool
parseEnvironment =
    Options.Applicative.switch
        (   Options.Applicative.long "environment"
        <>  Options.Applicative.help "Force display of environment differences"
        )

data Options = Options
    { left        :: FilePath
    , right       :: FilePath
    , color       :: Color
    , orientation :: Orientation
    , environment :: Bool
    }

data Orientation = Character | Word | Line

parseOptions :: Parser Options
parseOptions = do
    left        <- parseLeft
    right       <- parseRight
    color       <- parseColor
    orientation <- parseLineOriented
    environment <- parseEnvironment

    return (Options { left, right, color, orientation, environment })
  where
    parseFilePath metavar = do
        Options.Applicative.strArgument
            (Options.Applicative.metavar metavar)

    parseLeft = parseFilePath "LEFT"

    parseRight = parseFilePath "RIGHT"

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.header "Explain why two derivations differ"
        )

data Context = Context
    { tty         :: TTY
    , indent      :: Natural
    , orientation :: Orientation
    , environment :: Bool
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
#if MIN_VERSION_base(4,9,0)
    , MonadFail
#endif
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
pathToText = Data.Text.pack

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

-- | Group paths by their name
groupByName :: Map FilePath a -> Map Text (Map FilePath a)
groupByName m = Data.Map.fromList assocs
  where
    toAssoc key = (derivationName key, Data.Map.filterWithKey predicate m)
      where
        predicate key' _ = derivationName key == derivationName key'

    assocs = fmap toAssoc (Data.Map.keys m)

{-| Extract the name of a build product

    Similar to `derivationName`, this assumes that the path name is:

    > /nix/store/${32_CHARACTER_HASH}-${NAME}.drv
-}
buildProductName :: FilePath -> Text
buildProductName = Data.Text.drop 44 . pathToText

-- | Like `groupByName`, but for `Set`s
groupSetsByName :: Set FilePath -> Map Text (Set FilePath)
groupSetsByName s = Data.Map.fromList (fmap toAssoc (Data.Set.toList s))
  where
    toAssoc key = (buildProductName key, Data.Set.filter predicate s)
      where
        predicate key' = buildProductName key == buildProductName key'

-- | Read and parse a derivation from a file
readDerivation :: FilePath -> Diff (Derivation FilePath Text)
readDerivation path = do
    let string = path
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
redBackground  :: Orientation -> TTY -> Text -> Text
redBackground Line IsTTY text = "\ESC[41m" <> prefix <> "\ESC[0m" <> suffix
  where
    (prefix, suffix) = Data.Text.break lineBoundary text
redBackground Word IsTTY text = "\ESC[41m" <> prefix <> "\ESC[0m" <> suffix
  where
    (prefix, suffix) = Data.Text.break wordBoundary text
redBackground Character IsTTY text = "\ESC[41m" <> text <> "\ESC[0m"
redBackground Line NotTTY text = "- " <> text
redBackground _    NotTTY text = "←" <> text <> "←"

-- | Color text green
green :: TTY -> Text -> Text
green IsTTY  text = "\ESC[1;32m" <> text <> "\ESC[0m"
green NotTTY text = text

-- | Color text background green
greenBackground :: Orientation -> TTY -> Text -> Text
greenBackground Line IsTTY text = "\ESC[42m" <> prefix <> "\ESC[0m" <> suffix
  where
    (prefix, suffix) = Data.Text.break lineBoundary text
greenBackground Word IsTTY text = "\ESC[42m" <> prefix <> "\ESC[0m" <> suffix
  where
    (prefix, suffix) = Data.Text.break wordBoundary text
greenBackground Character IsTTY  text = "\ESC[42m" <> text <> "\ESC[0m"
greenBackground Line NotTTY text = "+ " <> text
greenBackground _    NotTTY text = "→" <> text <> "→"

-- | Color text grey
grey :: Orientation -> TTY -> Text -> Text
grey _    IsTTY  text = "\ESC[1;2m" <> text <> "\ESC[0m"
grey Line NotTTY text = "  " <> text
grey _    NotTTY text = text

-- | Format the left half of a diff
minus :: TTY -> Text -> Text
minus tty text = red tty ("- " <> text)

-- | Format the right half of a diff
plus :: TTY -> Text -> Text
plus tty text = green tty ("+ " <> text)

-- | Format text explaining a diff
explain :: Text -> Text
explain text = "• " <> text

-- `getGroupedDiff` from `Diff` library, adapted for `patience`
getGroupedDiff :: Ord a => [a] -> [a] -> [Patience.Item [a]]
getGroupedDiff oldList newList = go $ Patience.diff oldList newList
  where
    go = \case
      Patience.Old x : xs ->
        let (fs, rest) = goOlds xs
         in Patience.Old (x : fs) : go rest
      Patience.New x : xs ->
        let (fs, rest) = goNews xs
         in Patience.New (x : fs) : go rest
      Patience.Both x y : xs ->
        let (fs, rest) = goBoth xs
            (fxs, fys) = unzip fs
         in Patience.Both (x : fxs) (y : fys) : go rest
      [] -> []

    goOlds = \case
      Patience.Old x : xs ->
        let (fs, rest) = goOlds xs
         in (x : fs, rest)
      xs -> ([], xs)

    goNews = \case
      Patience.New x : xs ->
        let (fs, rest) = goNews xs
         in (x : fs, rest)
      xs -> ([], xs)

    goBoth = \case
      Patience.Both x y : xs ->
        let (fs, rest) = goBoth xs
         in ((x, y) : fs, rest)
      xs -> ([], xs)

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
    -> (DerivationOutput FilePath Text)
    -- ^ Left derivation outputs
    -> (DerivationOutput FilePath Text)
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
        diffWith leftHashAlgo rightHashAlgo \(sign, hashAlgo) -> do
            echo ("        " <> sign hashAlgo)

-- | Diff two sets of outputs
diffOutputs
    :: Map Text (DerivationOutput FilePath Text)
    -- ^ Left derivation outputs
    -> Map Text (DerivationOutput FilePath Text)
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
            diffWith leftExtraOutputs rightExtraOutputs \(sign, extraOutputs) -> do
                forM_ (Data.Map.toList extraOutputs) \(key, _value) -> do
                    echo ("    " <> sign ("{" <> key <> "}"))
    forM_ (Data.Map.toList bothOutputs) \(key, (leftOutput, rightOutput)) -> do
        if leftOutput == rightOutput
        then return ()
        else diffOutput key leftOutput rightOutput

mapDiff :: (a -> b) -> Patience.Item a -> Patience.Item b
mapDiff f (Patience.Old  l  ) = Patience.Old (f l)
mapDiff f (Patience.New    r) = Patience.New (f r)
mapDiff f (Patience.Both l r) = Patience.Both (f l) (f r)

{-| Split `Text` into spans of `Text` that alternatively fail and satisfy the
    given predicate

    The first span (if present) does not satisfy the predicate (even if the
    span is empty)

    >>> decomposeOn (== 'b') "aabbaa"
    ["aa","bb","aa"]
    >>> decomposeOn (== 'b') "bbaa"
    ["","bb","aa"]
    >>> decomposeOn (== 'b') ""
    []
-}
decomposeOn :: (Char -> Bool) -> Text -> [Text]
decomposeOn predicate = unsatisfy
  where
    unsatisfy text
        | Data.Text.null text = []
        | otherwise           = prefix : satisfy suffix
      where
        (prefix, suffix) = Data.Text.break predicate text

    satisfy text
        | Data.Text.null text = []
        | otherwise           = prefix : unsatisfy suffix
      where
        (prefix, suffix) = Data.Text.span predicate text

lineBoundary :: Char -> Bool
lineBoundary = ('\n' ==)

wordBoundary :: Char -> Bool
wordBoundary = Char.isSpace

-- | Diff two `Text` values
diffText
    :: Text
    -- ^ Left value to compare
    -> Text
    -- ^ Right value to compare
    -> Diff Text
diffText left right = do
    Context{ indent, orientation, tty } <- ask

    let n = fromIntegral indent

    let leftString  = Data.Text.unpack left
    let rightString = Data.Text.unpack right

    let decomposeWords = decomposeOn wordBoundary

    let decomposeLines text = loop (decomposeOn lineBoundary text)
          where
            -- Groups each newline character with the preceding line
            loop (x : y : zs) = (x <> y) : loop zs
            loop          zs  = zs

    let leftWords  = decomposeWords left
    let rightWords = decomposeWords right

    let leftLines  = decomposeLines left
    let rightLines = decomposeLines right

    let chunks =
            case orientation of
                Character ->
                    fmap (mapDiff Data.Text.pack) (getGroupedDiff leftString rightString)
                Word ->
                    Patience.diff leftWords rightWords
                Line ->
                    Patience.diff leftLines rightLines
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

    let renderChunk (Patience.Old  l  ) =
            redBackground   orientation tty l
        renderChunk (Patience.New    r) =
            greenBackground orientation tty r
        renderChunk (Patience.Both l _) =
            grey            orientation tty l

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
        diffWith leftExtraEnv rightExtraEnv \(sign, extraEnv) -> do
            forM_ (Data.Map.toList extraEnv) \(key, value) -> do
                echo ("    " <> sign (key <> "=" <> value))
        forM_ (Data.Map.toList bothEnv) \(key, (leftValue, rightValue)) -> do
            if      predicate key (leftValue, rightValue)
            then return ()
            else do
                text <- diffText leftValue rightValue
                echo ("    " <> key <> "=" <> text)

-- | Diff input sources
diffSrcs
    :: Set FilePath
    -- ^ Left input sources
    -> Set FilePath
    -- ^ Right inputSources
    -> Diff ()
diffSrcs leftSrcs rightSrcs = do
    let groupedLeftSrcs  = groupSetsByName leftSrcs
    let groupedRightSrcs = groupSetsByName rightSrcs

    let leftNames  = Data.Map.keysSet groupedLeftSrcs
    let rightNames = Data.Map.keysSet groupedRightSrcs

    let leftExtraNames  = Data.Set.difference leftNames  rightNames
    let rightExtraNames = Data.Set.difference rightNames leftNames

    let leftExtraSrcs  = Data.Set.difference leftSrcs  rightSrcs
    let rightExtraSrcs = Data.Set.difference rightSrcs leftSrcs

    Monad.when (leftNames /= rightNames) do
        echo (explain "The set of input source names do not match:")
        diffWith leftExtraNames rightExtraNames \(sign, names) -> do
            forM_ names \name -> do
                echo ("    " <> sign name)

    let assocs = Data.Map.toList (innerJoin groupedLeftSrcs groupedRightSrcs)

    forM_ assocs \(inputName, (leftPaths, rightPaths)) -> do
        let leftExtraPaths  = Data.Set.difference leftPaths  rightPaths
        let rightExtraPaths = Data.Set.difference rightPaths leftPaths
        case (Data.Set.toList leftExtraPaths, Data.Set.toList rightExtraPaths) of
            ([], []) -> return ()
            ([leftPath], [rightPath]) ->  do
                echo (explain ("The input source named `" <> inputName <> "` differs"))
                leftExists  <- liftIO (Directory.doesFileExist leftPath)
                rightExists <- liftIO (Directory.doesFileExist rightPath)
                if leftExists && rightExists
                    then do
                        leftText  <- liftIO (Data.Text.IO.readFile leftPath)
                        rightText <- liftIO (Data.Text.IO.readFile rightPath)

                        text <- diffText leftText rightText
                        echo ("    " <> text)
                    else do
                        return ()
                return ()

diffPlatform :: Text -> Text -> Diff ()
diffPlatform leftPlatform rightPlatform = do
    if leftPlatform == rightPlatform
    then return ()
    else do
        echo (explain "The platforms do not match")
        diffWith leftPlatform rightPlatform \(sign, platform) -> do
            echo ("    " <> sign platform)

diffBuilder :: Text -> Text -> Diff ()
diffBuilder leftBuilder rightBuilder = do
    if leftBuilder == rightBuilder
    then return ()
    else do
        echo (explain "The builders do not match")
        diffWith leftBuilder rightBuilder \(sign, builder) -> do
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
        let diffs = Patience.diff leftList rightList
        let renderDiff (Patience.Old arg) =
                echo ("    " <> minus tty arg)
            renderDiff (Patience.New arg) =
                echo ("    " <> plus tty arg)
            renderDiff (Patience.Both arg _) =
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
        diffWith (leftPath, leftOutputs) (rightPath, rightOutputs) \(sign, (path, outputs)) -> do
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

            Monad.when (leftNames /= rightNames) do
                echo (explain "The set of input derivation names do not match:")
                diffWith leftExtraNames rightExtraNames \(sign, names) -> do
                    forM_ names \name -> do
                        echo ("    " <> sign name)

            let assocs = Data.Map.toList (innerJoin leftInputs rightInputs)
            descended <- forM assocs \(inputName, (leftPaths, rightPaths)) -> do
                let leftExtraPaths =
                        Data.Map.difference leftPaths  rightPaths
                let rightExtraPaths =
                        Data.Map.difference rightPaths leftPaths
                case (Data.Map.toList leftExtraPaths, Data.Map.toList rightExtraPaths) of
                    _   | leftPaths == rightPaths -> do
                        return False
                    ([(leftPath', leftOutputs')], [(rightPath', rightOutputs')])
                        | leftOutputs' == rightOutputs' -> do
                        echo (explain ("The input derivation named `" <> inputName <> "` differs"))
                        indented 2 (diff False leftPath' leftOutputs' rightPath' rightOutputs')
                        return True
                    _ -> do
                        echo (explain ("The set of input derivations named `" <> inputName <> "` do not match"))
                        diffWith leftExtraPaths rightExtraPaths \(sign, extraPaths) -> do
                            forM_ (Data.Map.toList extraPaths) \(extraPath, outputs) -> do
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

    Options { left, right, color, orientation, environment } <- Options.Applicative.execParser parserInfo

    tty <- case color of
        Never -> do
            return NotTTY
        Always -> do
            return IsTTY
        Auto -> do
            b <- System.Posix.Terminal.queryTerminal System.Posix.IO.stdOutput
            return (if b then IsTTY else NotTTY)

    let indent = 0
    let context = Context { tty, indent, orientation, environment }
    let status = Status Data.Set.empty
    let action = diff True left (Data.Set.singleton "out") right (Data.Set.singleton "out")
    Control.Monad.State.evalStateT (Control.Monad.Reader.runReaderT (unDiff action) context) status
