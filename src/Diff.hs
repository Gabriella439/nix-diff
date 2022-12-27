{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Diff where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.State (MonadState, StateT, get, put)
import Data.Attoparsec.Text (IResult(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Nix.Derivation (Derivation, DerivationOutput)

import qualified Control.Monad.Reader
import qualified Data.Attoparsec.Text
import qualified Data.Char            as Char
import qualified Data.List            as List
import qualified Data.List.NonEmpty
import qualified Data.Map
import qualified Data.Set
import qualified Data.String          as String
import qualified Data.Text            as Text
import qualified Data.Vector
import qualified Nix.Derivation
import qualified Patience
import qualified System.Directory     as Directory
import qualified System.FilePath      as FilePath
import qualified System.Process       as Process

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
import qualified Data.ByteString
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
#endif


newtype Status = Status { visited :: Set Diffed }

data Diffed = Diffed
    { leftDerivation  :: FilePath
    , leftOutput      :: Set Text
    , rightDerivation :: FilePath
    , rightOutput     :: Set Text
    } deriving (Eq, Ord)

newtype Diff a = Diff { unDiff :: ReaderT DiffContext (StateT Status IO) a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader DiffContext
    , MonadState Status
    , MonadIO
#if MIN_VERSION_base(4,9,0)
    , MonadFail
#endif
    )

data DiffContext = DiffContext
  { orientation :: Orientation
  , environment :: Bool
  }

data Orientation = Character | Word | Line

data Changed a = Changed { before :: a, now :: a }

type OutputHash = Text

type Platform = Text

type Builder = Text

type Argument = Text

data DerivationDiff
  = DerivationsAreTheSame
  | AlreadyCompared
  | NamesDontMatch   { outputStructure :: Changed (FilePath, Set Text)}
  | OutputsDontMatch { outputStructure :: Changed (FilePath, Set Text)}
  | DerivationDiff
      { outputStructure :: Changed (FilePath, Set Text)
      , outputsDiff     :: OutputsDiff
      , platformDiff    :: Maybe (Changed Platform)
        -- ^ Will be Nothing, if Platform does not change
      , builderDiff     :: Maybe (Changed Builder)
        -- ^ Will be Nothing, if Builder does not change
      , argumentsDiff   :: Maybe (NonEmpty (Patience.Item Argument))
        -- ^ Will be Nothing, if arguments are equal
      , sourcesDiff     :: SourcesDiff
      , inputsDiff      :: InputsDiff
      , envDiff         :: Maybe EnvironmentDiff
        -- ^ Will be Nothing, if environment comparison is skipped
      }


data OutputsDiff = OutputsDiff
  { extraOutputs :: Maybe (Changed (Map Text (DerivationOutput FilePath Text)))
    -- ^ Map from derivation name to it's outputs.
    --   Will be Nothing, if `Data.Map.difference` gives
    --   empty Maps for both new and old outputs
  , outputHashDiff :: [OutputDiff]
    -- ^ Difference of outputs with the same name.
    --   Will be empty, if all outputs are equal.
  }

data OutputDiff = OutputDiff
  { outputName :: Text
  , hashDifference :: Changed OutputHash
  }

data SourcesDiff = SourcesDiff
  { extraSrcNames :: Maybe (Changed (Set Text))
    -- ^ Will be Nothing, if there is no extra source names
  , srcFilesDiff :: [SourceFileDiff]
  }

data SourceFileDiff
  = OneSourceFileDiff
      { srcName :: Text
      , srcContentDiff :: Maybe [Patience.Item Text]
      -- ^ Will be Nothing, if any of source files not exists
      }
  | SomeSourceFileDiff
      { srcName :: Text
      , srcFilesDiff :: Changed [FilePath]
      }

data InputsDiff = InputsDiff
  { inputExtraNames :: Maybe (Changed (Set Text))
    -- ^ Will be Nothing, if there is no extra input names
  , inputDerivationDiffs :: [InputDerivationsDiff]
  }

data InputDerivationsDiff
  = OneDerivationDiff
      { drvName :: Text
      , drvDiff :: DerivationDiff
      }
  | SomeDerivationsDiff
      { drvName :: Text
      , extraPartsDiff :: Changed (Map FilePath (Set Text))
      }

data EnvVarDiff = EnvVarDiff
  { envKey :: Text
  , envValueDiff :: [Patience.Item Text]
  }

data EnvironmentDiff
  = EnvironmentsAreEqual
  | EnvironmentDiff
      { extraEnvDiff :: Changed (Map Text Text)
      , envContentDiff :: [EnvVarDiff]
      }

{-| Extract the name of a derivation (i.e. the part after the hash)

    This is used to guess which derivations are related to one another, even
    though their hash might differ

    Note that this assumes that the path name is:

    > /nix/store/${32_CHARACTER_HASH}-${NAME}.drv

    Nix technically does not require that the Nix store is actually stored
    underneath `/nix/store`, but this is the overwhelmingly common use case
-}
derivationName :: FilePath -> Text
derivationName = Text.dropEnd 4 . Text.drop 44 . Text.pack

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
buildProductName = Text.drop 44 . Text.pack

-- | Like `groupByName`, but for `Set`s
groupSetsByName :: Set FilePath -> Map Text (Set FilePath)
groupSetsByName s = Data.Map.fromList (fmap toAssoc (Data.Set.toList s))
  where
    toAssoc key = (buildProductName key, Data.Set.filter predicate s)
      where
        predicate key' = buildProductName key == buildProductName key'

-- | Read a file as utf-8 encoded string, replacing non-utf-8 characters
-- with the unicode replacement character.
-- This is necessary since derivations (and nix source code!) can in principle
-- contain arbitrary bytes, but `nix-derivation` can only parse from 'Text'.
readFileUtf8Lenient :: FilePath -> IO Text
readFileUtf8Lenient file =
    Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
        <$> Data.ByteString.readFile file

-- | Read and parse a derivation from a file
readDerivation :: FilePath -> Diff (Derivation FilePath Text)
readDerivation path = do
    let string = path
    text <- liftIO (readFileUtf8Lenient string)
    case Data.Attoparsec.Text.parse Nix.Derivation.parseDerivation text of
        Done _ derivation -> do
            return derivation
        _ -> do
            fail ("Could not parse a derivation from this file: " ++ string)

-- | Read and parse a derivation from a store path that can be a derivation
-- (.drv) or a realized path, in which case the corresponding derivation is
-- queried.
readInput :: FilePath -> Diff (Derivation FilePath Text)
readInput pathAndMaybeOutput = do
    let (path, _) = List.break (== '!') pathAndMaybeOutput
    if FilePath.isExtensionOf ".drv" path
    then readDerivation path
    else do
        let string = path
        result <- liftIO (Process.readProcess "nix-store" [ "--query", "--deriver", string ] [])
        case String.lines result of
            [] -> fail ("Could not obtain the derivation of " ++ string)
            l : ls -> do
                let drv_path = Data.List.NonEmpty.last (l :| ls)
                readDerivation drv_path

{-| Join two `Map`s on shared keys, discarding keys which are not present in
    both `Map`s
-}
innerJoin :: Ord k => Map k a -> Map k b -> Map k (a, b)
innerJoin = Data.Map.mergeWithKey both left right
  where
    both _ a b = Just (a, b)

    left _ = Data.Map.empty

    right _ = Data.Map.empty

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

-- | Diff two outputs
diffOutput
    :: Text
    -- ^ Output name
    -> (DerivationOutput FilePath Text)
    -- ^ Left derivation outputs
    -> (DerivationOutput FilePath Text)
    -- ^ Right derivation outputs
    -> (Maybe OutputDiff)
diffOutput outputName leftOutput rightOutput = do
    -- We deliberately do not include output paths or hashes in the diff since
    -- we already expect them to differ if the inputs differ.  Instead, we focus
    -- only displaying differing inputs.
    let leftHashAlgo  = Nix.Derivation.hashAlgo leftOutput
    let rightHashAlgo = Nix.Derivation.hashAlgo rightOutput
    if leftHashAlgo == rightHashAlgo
      then Nothing
      else Just (OutputDiff outputName (Changed leftHashAlgo rightHashAlgo))

-- | Diff two sets of outputs
diffOutputs
    :: Map Text (DerivationOutput FilePath Text)
    -- ^ Left derivation outputs
    -> Map Text (DerivationOutput FilePath Text)
    -- ^ Right derivation outputs
    -> OutputsDiff
diffOutputs leftOutputs rightOutputs = do
    let leftExtraOutputs  = Data.Map.difference leftOutputs  rightOutputs
    let rightExtraOutputs = Data.Map.difference rightOutputs leftOutputs

    let bothOutputs = innerJoin leftOutputs rightOutputs

    let
      extraOutputs =
        if Data.Map.null leftExtraOutputs && Data.Map.null rightExtraOutputs
          then Nothing
          else Just (Changed leftExtraOutputs rightExtraOutputs)
    let
      outputDifference = flip map (Data.Map.toList bothOutputs) \(key, (leftOutput, rightOutput)) -> do
        if leftOutput == rightOutput
        then Nothing
        else Just (diffOutput key leftOutput rightOutput)

    OutputsDiff extraOutputs (catMaybes . catMaybes $ outputDifference)

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
        | Text.null text = []
        | otherwise      = prefix : satisfy suffix
      where
        (prefix, suffix) = Text.break predicate text

    satisfy text
        | Text.null text = []
        | otherwise      = prefix : unsatisfy suffix
      where
        (prefix, suffix) = Text.span predicate text

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
    -> Diff [Patience.Item Text]
    -- ^ List of blocks of diffed text
diffText left right = do
    DiffContext{ orientation } <- ask

    let leftString  = Text.unpack left
    let rightString = Text.unpack right

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
                    fmap (fmap Text.pack) (getGroupedDiff leftString rightString)
                Word ->
                    Patience.diff leftWords rightWords
                Line ->
                    Patience.diff leftLines rightLines

    return chunks

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
    -> Diff EnvironmentDiff
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
    then return EnvironmentsAreEqual
    else do
        let extraEnvDiff = Changed leftExtraEnv rightExtraEnv
        envDiff <- forM (Data.Map.toList bothEnv) \(key, (leftValue, rightValue)) -> do
            if      predicate key (leftValue, rightValue)
            then return Nothing
            else do
                valueDiff <- diffText leftValue rightValue
                pure (Just (EnvVarDiff key valueDiff))
        pure (EnvironmentDiff extraEnvDiff (catMaybes envDiff))


-- | Diff input sources
diffSrcs
    :: Set FilePath
    -- ^ Left input sources
    -> Set FilePath
    -- ^ Right inputSources
    -> Diff SourcesDiff
diffSrcs leftSrcs rightSrcs = do
    let groupedLeftSrcs  = groupSetsByName leftSrcs
    let groupedRightSrcs = groupSetsByName rightSrcs

    let leftNames  = Data.Map.keysSet groupedLeftSrcs
    let rightNames = Data.Map.keysSet groupedRightSrcs

    let leftExtraNames  = Data.Set.difference leftNames  rightNames
    let rightExtraNames = Data.Set.difference rightNames leftNames

    let extraSrcNames = if (leftNames /= rightNames)
        then Just (Changed leftExtraNames rightExtraNames)
        else Nothing

    let assocs = Data.Map.toList (innerJoin groupedLeftSrcs groupedRightSrcs)

    srcFilesDiff <- forM assocs \(inputName, (leftPaths, rightPaths)) -> do
        let leftExtraPaths  = Data.Set.difference leftPaths  rightPaths
        let rightExtraPaths = Data.Set.difference rightPaths leftPaths
        case (Data.Set.toList leftExtraPaths, Data.Set.toList rightExtraPaths) of
            ([], []) -> return Nothing
            ([leftPath], [rightPath]) ->  do
                leftExists  <- liftIO (Directory.doesFileExist leftPath)
                rightExists <- liftIO (Directory.doesFileExist rightPath)
                srcContentDiff <- if leftExists && rightExists
                    then do
                        leftText  <- liftIO (readFileUtf8Lenient leftPath)
                        rightText <- liftIO (readFileUtf8Lenient rightPath)

                        text <- diffText leftText rightText
                        return (Just text)
                    else do
                        return Nothing
                return (Just (OneSourceFileDiff inputName srcContentDiff))
            (leftExtraPathsList, rightExtraPathsList) -> do
                return (Just (SomeSourceFileDiff inputName (Changed leftExtraPathsList rightExtraPathsList)))
    return (SourcesDiff extraSrcNames (catMaybes srcFilesDiff))

diffPlatform :: Text -> Text -> Maybe (Changed Platform)
diffPlatform leftPlatform rightPlatform = do
    if leftPlatform == rightPlatform
    then Nothing
    else Just (Changed leftPlatform rightPlatform)

diffBuilder :: Text -> Text -> Maybe (Changed Builder)
diffBuilder leftBuilder rightBuilder = do
    if leftBuilder == rightBuilder
    then Nothing
    else Just (Changed leftBuilder rightBuilder)

diffArgs :: Vector Text -> Vector Text -> Maybe (NonEmpty (Patience.Item Argument))
diffArgs leftArgs rightArgs = do
    if leftArgs == rightArgs
    then Nothing
    else do
        let leftList  = Data.Vector.toList leftArgs
        let rightList = Data.Vector.toList rightArgs
        Data.List.NonEmpty.nonEmpty (Patience.diff leftList rightList)

diff :: Bool -> FilePath -> Set Text -> FilePath -> Set Text -> Diff DerivationDiff
diff topLevel leftPath leftOutputs rightPath rightOutputs = do
    Status { visited } <- get
    let diffed = Diffed leftPath leftOutputs rightPath rightOutputs
    if leftPath == rightPath
    then return DerivationsAreTheSame
    else if Data.Set.member diffed visited
    then do
        pure AlreadyCompared
    else do
        put (Status (Data.Set.insert diffed visited))
        let
          outputStructure = Changed (leftPath, leftOutputs) (rightPath, rightOutputs)

        if derivationName leftPath /= derivationName rightPath && not topLevel
        then do
            pure (NamesDontMatch outputStructure)
        else if leftOutputs /= rightOutputs
        then do
            pure (OutputsDontMatch outputStructure)
        else do
            leftDerivation  <- readInput leftPath
            rightDerivation <- readInput rightPath

            let leftOuts = Nix.Derivation.outputs leftDerivation
            let rightOuts = Nix.Derivation.outputs rightDerivation
            let outputsDiff = diffOutputs leftOuts rightOuts

            let leftPlatform  = Nix.Derivation.platform leftDerivation
            let rightPlatform = Nix.Derivation.platform rightDerivation
            let platformDiff = diffPlatform leftPlatform rightPlatform

            let leftBuilder  = Nix.Derivation.builder leftDerivation
            let rightBuilder = Nix.Derivation.builder rightDerivation
            let builderDiff = diffBuilder leftBuilder rightBuilder

            let leftArgs  = Nix.Derivation.args leftDerivation
            let rightArgs = Nix.Derivation.args rightDerivation
            let argumentsDiff = diffArgs leftArgs rightArgs

            let leftSrcs  = Nix.Derivation.inputSrcs leftDerivation
            let rightSrcs = Nix.Derivation.inputSrcs rightDerivation
            sourcesDiff <- diffSrcs leftSrcs rightSrcs

            let leftInputs  = groupByName (Nix.Derivation.inputDrvs leftDerivation)
            let rightInputs = groupByName (Nix.Derivation.inputDrvs rightDerivation)

            let leftNames  = Data.Map.keysSet leftInputs
            let rightNames = Data.Map.keysSet rightInputs
            let leftExtraNames  = Data.Set.difference leftNames  rightNames
            let rightExtraNames = Data.Set.difference rightNames leftNames

            let inputExtraNames = if (leftNames /= rightNames)
                then Just (Changed leftExtraNames rightExtraNames)
                else Nothing

            let assocs = Data.Map.toList (innerJoin leftInputs rightInputs)
            (descended, mInputsDiff) <- unzip <$> forM assocs \(inputName, (leftPaths, rightPaths)) -> do
                let leftExtraPaths =
                        Data.Map.difference leftPaths  rightPaths
                let rightExtraPaths =
                        Data.Map.difference rightPaths leftPaths
                case (Data.Map.toList leftExtraPaths, Data.Map.toList rightExtraPaths) of
                    _   | leftPaths == rightPaths -> do
                        return (False, Nothing)
                    ([(leftPath', leftOutputs')], [(rightPath', rightOutputs')])
                        | leftOutputs' == rightOutputs' -> do
                        drvDiff <- diff False leftPath' leftOutputs' rightPath' rightOutputs'
                        return (True, Just (OneDerivationDiff inputName drvDiff))
                    _ -> do
                        let extraPartsDiff = Changed leftExtraPaths rightExtraPaths
                        return (False, Just (SomeDerivationsDiff inputName extraPartsDiff))

            let inputDerivationDiffs = catMaybes mInputsDiff
            let inputsDiff = InputsDiff {..}

            DiffContext { environment } <- ask

            envDiff <- if or descended && not environment
                then return Nothing
                else do
                  let leftEnv  = Nix.Derivation.env leftDerivation
                  let rightEnv = Nix.Derivation.env rightDerivation
                  let leftOutNames  = Data.Map.keysSet leftOuts
                  let rightOutNames = Data.Map.keysSet rightOuts
                  Just <$> diffEnv leftOutNames rightOutNames leftEnv rightEnv
            pure DerivationDiff{..}
