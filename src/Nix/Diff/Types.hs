{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingVia           #-}

module Nix.Diff.Types where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import Nix.Derivation (DerivationOutput (..))

import qualified Patience
import GHC.Generics (Generic)
import Data.Foldable
import Data.Aeson
import Data.Aeson.Types
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

{- ** NOTE: Lawless instances
   All the Arbitrary instances here are written to
   check if `decode . encode == id` rule was broken,
   so they don't respect to internal laws of these types,
   such as "Maps from `extraOutputs` must not have
   intersecting keys" and so on.
   If you want to test these invariants, you have to rewrite
   instances manually.
-}

data Changed a = Changed { before :: a, now :: a }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (Arbitrary a) => Arbitrary (Changed a) where
  arbitrary = Changed <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance ToJSON a => ToJSON (Changed a) where
  toJSON = changedToJSON toJSON

instance FromJSON a => FromJSON (Changed a) where
  parseJSON = changedFromJSON parseJSON

newtype TextDiff = TextDiff {unTextDiff :: [Patience.Item Text]}
  deriving stock (Eq, Show)

instance Arbitrary TextDiff where
  arbitrary = TextDiff <$> listOf arbitraryItem

instance ToJSON TextDiff where
  toJSON = listValue itemToJSON . squashDiff . unTextDiff
    where
      -- TODO: think about longest difference spans,
      -- instead of printing each line/word/character as one item.
      -- Perhaps, this squashing should be in Diff module,
      -- Because trees before and after squashing would be printed
      -- in a different way by Render.HumanReadable.
      --
      -- Don't forget to update Arbitrary instance for that type.

      -- squashDiff (Patience.Old a : Patience.Old b : xs) =
      --   squashDiff (Patience.Old (a <> b) : xs)
      -- squashDiff (Patience.New a : Patience.New b : xs) =
      --   squashDiff (Patience.New (a <> b) : xs)
      -- squashDiff (Patience.Both a _ : Patience.Both b _ : xs) =
      --   let ab = a <> b in squashDiff (Patience.Both ab ab : xs)
      -- squashDiff (x: xs) = x : squashDiff xs
      -- squashDiff [] = []
      squashDiff = id

instance FromJSON TextDiff where
  parseJSON v = TextDiff <$> (traverse itemFromJSON =<< parseJSON v)

-- Helpfull aliases

type OutputHash = Text

type Platform = Text

type Builder = Text

type Argument = Text

-- Derivation diff

data DerivationDiff
  = DerivationsAreTheSame
  | AlreadyCompared
  | OnlyAlreadyComparedBelow { outputStructure :: Changed OutputStructure}
  | NamesDontMatch           { outputStructure :: Changed OutputStructure}
  | OutputsDontMatch         { outputStructure :: Changed OutputStructure}
  | DerivationDiff
      { outputStructure :: Changed OutputStructure
      , outputsDiff     :: OutputsDiff
      , platformDiff    :: Maybe (Changed Platform)
        -- ^ Will be Nothing, if Platform does not change
      , builderDiff     :: Maybe (Changed Builder)
        -- ^ Will be Nothing, if Builder does not change
      , argumentsDiff   :: Maybe ArgumentsDiff
        -- ^ Will be Nothing, if arguments are equal
      , sourcesDiff     :: SourcesDiff
      , inputsDiff      :: InputsDiff
      , envDiff         :: Maybe EnvironmentDiff
        -- ^ Will be Nothing, if environment comparison is skipped
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary DerivationDiff

-- Output structure

data OutputStructure = OutputStructure
  { derivationPath :: FilePath
  , derivationOutputs :: Set Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary OutputStructure

-- ** Outputs diff

data OutputsDiff = OutputsDiff
  { extraOutputs :: Maybe (Changed (Map Text (DerivationOutput FilePath Text)))
    -- ^ Map from derivation name to its outputs.
    --   Will be Nothing, if `Data.Map.difference` gives
    --   empty Maps for both new and old outputs
  , outputHashDiff :: [OutputDiff]
    -- ^ Difference of outputs with the same name.
    --   Will be empty, if all outputs are equal.
  }
  deriving stock (Eq, Show)

instance Arbitrary OutputsDiff where
  arbitrary = OutputsDiff <$> arbitraryExtraOutputs  <*> arbitrary
    where
      arbitraryExtraOutputs =
        oneof [pure Nothing, Just <$> arbitraryChangedMap]

      arbitraryChangedMap =
        Changed <$> arbitraryMap <*> arbitraryMap

      arbitraryMap = Map.fromList <$>
        listOf ((,) <$> arbitrary <*> arbitraryDerivationOutput)

instance ToJSON OutputsDiff where
  toJSON OutputsDiff{..} = object
    [ "extraOutputs" .= fmap (changedToJSON extraOutputsToJSON) extraOutputs
    , "outputHashDiff" .= outputHashDiff
    ]
    where
    extraOutputsToJSON :: Map Text (DerivationOutput FilePath Text) -> Value
    extraOutputsToJSON = toJSON . fmap derivationOutputToJSON

    derivationOutputToJSON :: DerivationOutput FilePath Text -> Value
    derivationOutputToJSON DerivationOutput{..} = object
      [ "path" .= path
      , "hashAlgo" .= hashAlgo
      , "hash" .= hash
      ]

instance FromJSON OutputsDiff where
  parseJSON = withObject "OutputsDiff" \o -> do
    extraOutputsWithoutParsingDerivationOutputs <- o .: "extraOutputs"
    ohd <- o .: "outputHashDiff"
    eo <- (traverse . traverse . traverse)
      derivationOutputFromJSON
      extraOutputsWithoutParsingDerivationOutputs
    pure $ OutputsDiff eo ohd
    where

      derivationOutputFromJSON :: Value -> Parser (DerivationOutput FilePath Text)
      derivationOutputFromJSON = withObject "DerivationOutput" \o ->
        DerivationOutput <$> o .: "path" <*> o .: "hashAlgo" <*> o .: "hash"

data OutputDiff = OutputDiff
  { outputName :: Text
  , hashDifference :: Changed OutputHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary OutputDiff

-- ** Arguments diff

newtype ArgumentsDiff = ArgumentsDiff
  { unArgumetsDiff :: NonEmpty (Patience.Item Argument)
  }
  deriving stock (Eq, Show)

instance Arbitrary ArgumentsDiff where
  arbitrary = ArgumentsDiff . NonEmpty.fromList <$> listOf1 arbitraryItem

instance ToJSON ArgumentsDiff where
  toJSON = listValue itemToJSON . toList . unArgumetsDiff

instance FromJSON ArgumentsDiff where
  parseJSON v = ArgumentsDiff <$> (traverse itemFromJSON =<< parseJSON v)

-- ** Sources diff

data SourcesDiff = SourcesDiff
  { extraSrcNames :: Maybe (Changed (Set Text))
    -- ^ Will be Nothing, if there is no extra source names
  , srcFilesDiff :: [SourceFileDiff]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary SourcesDiff

data SourceFileDiff
  = OneSourceFileDiff
      { srcName :: Text
      , srcContentDiff :: Maybe TextDiff
      -- ^ Will be Nothing, if any of source files not exists
      }
  | SomeSourceFileDiff
      { srcName :: Text
      , srcFileDiff :: Changed [FilePath]
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary SourceFileDiff

-- ** Inputs diff

data InputsDiff = InputsDiff
  { inputExtraNames :: Maybe (Changed (Set Text))
    -- ^ Will be Nothing, if there is no extra input names
  , inputDerivationDiffs :: [InputDerivationsDiff]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary InputsDiff

data InputDerivationsDiff
  = OneDerivationDiff
      { drvName :: Text
      , drvDiff :: DerivationDiff
      }
  | SomeDerivationsDiff
      { drvName :: Text
      , extraPartsDiff :: Changed (Map FilePath (Set Text))
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary InputDerivationsDiff

-- ** Environment diff

data EnvironmentDiff
  = EnvironmentsAreEqual
  | EnvironmentDiff
      { extraEnvDiff :: Changed (Map Text Text)
      , envContentDiff :: [EnvVarDiff]
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary EnvironmentDiff

data EnvVarDiff = EnvVarDiff
  { envKey :: Text
  , envValueDiff :: TextDiff
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary EnvVarDiff

-- ** Helpers

changedToJSON :: (a -> Value) -> Changed a -> Value
changedToJSON converter Changed{..} = object
  [ "before" .= converter before
  , "now" .= converter now
  ]

changedFromJSON :: (Value -> Parser a) -> Value -> Parser (Changed a)
changedFromJSON innerParser = withObject "Changed" \o -> do
  b <- o .: "before"
  n <- o .: "now"
  Changed <$> innerParser b <*> innerParser n

itemToJSON :: ToJSON v => Patience.Item v -> Value
itemToJSON (Patience.Old a)    = object
  [ "type" .= ("Old" :: Text), "content" .= a]
itemToJSON (Patience.New a)    = object
  [ "type" .= ("New" :: Text), "content" .= a]
itemToJSON (Patience.Both a _) = object
  [ "type" .= ("Both" :: Text), "content" .= a]

itemFromJSON :: FromJSON v => Value -> Parser (Patience.Item v)
itemFromJSON = withObject "Item" \o -> do
  t <- o .: "type"
  c <- o .: "content"
  case t :: Text of
    "Old" -> pure $ Patience.Old c
    "New" -> pure $ Patience.New c
    "Both" -> pure $ Patience.Both c c
    _ -> fail "Item: unexpected type"

arbitraryItem :: Arbitrary a => Gen (Patience.Item a)
arbitraryItem =  do
  t <- arbitrary
  ctr <- elements [old, new, both]
  pure (ctr t)
  where
    old = Patience.Old
    new = Patience.New
    both x = Patience.Both x x

arbitraryDerivationOutput :: (Arbitrary fp, Arbitrary txt) => Gen (DerivationOutput fp txt)
arbitraryDerivationOutput = DerivationOutput <$> arbitrary <*> arbitrary <*> arbitrary
