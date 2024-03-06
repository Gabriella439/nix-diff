{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImportQualifiedPost #-}

-- | A crude implementation of the Nix store concept.
--
-- For anything fancier than this, it would be best to use FFI bindings instead,
-- such as hercules-ci-cnix-store.
module Nix.Diff.Store
  ( StorePath (..),
    toPhysicalPath,
    toText,
    doesFileExist,
    readFileUtf8Lenient,
  )
where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.ByteString
import Data.Data (Data)
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import Test.QuickCheck (Arbitrary)
import qualified Data.Text as T

-- | A file path that may not exist on the true file system;
-- needs to be looked up in a store, which may be relocated.
--
-- Unlike the (C++) Nix StorePath type, subpaths are allowed.
newtype StorePath = StorePath
  { -- | If the store is relocated, its physical location is elsewhere, and this 'FilePath' won't resolve.
    -- Use 'toPhysicalPath'.
    unsafeStorePathFile :: FilePath
  }
  deriving (Data)
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Arbitrary)

doesFileExist :: StorePath -> IO Bool
doesFileExist =
  Directory.doesFileExist <=< toPhysicalPath

readFileUtf8Lenient :: StorePath -> IO Text
readFileUtf8Lenient sp = do
  file <- toPhysicalPath sp
  Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
    <$> Data.ByteString.readFile file

toPhysicalPath :: StorePath -> IO FilePath
toPhysicalPath (StorePath p) = do
  nixStoreDir <- lookupEnv "NIX_STORE_DIR" <&> maybe "/nix/store" stripSlash
  nixRemoteMaybe <- lookupEnv "NIX_REMOTE" <&> fmap stripSlash
  case nixRemoteMaybe of
    Just nixRemote | nixStoreDir `L.isPrefixOf` p -> do
      pure $ nixRemote <> "/" <> L.dropWhile (== '/') p
    _ -> pure p

-- | Convert a 'StorePath' to a 'Text' for display purposes. The path may not exist at this physical location.
toText :: StorePath -> Text
toText (StorePath p) = T.pack p

stripSlash :: FilePath -> FilePath
stripSlash = T.unpack . T.reverse . T.dropWhile (== '/') . T.reverse . T.pack
