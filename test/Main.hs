{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Data.Aeson
import Nix.Diff.Types
import Test.Tasty
import Test.Tasty.Silver
import Test.Tasty.QuickCheck
import System.Process.Typed
import qualified Data.ByteString.Lazy.Char8 as BS
import Nix.Diff
import qualified Control.Monad.Reader
import qualified Control.Monad.State
import qualified Data.Set
import Nix.Diff.Render.HumanReadable (runRender', renderDiffHumanReadable, TTY (NotTTY), RenderContext (RenderContext))
import Data.Text.Encoding (decodeUtf8)

main :: IO ()
main = do
  td <- initDerivations
  print td
  defaultMain $
     testGroup "Tests" [goldenTests td, properties]

humanReadableExpected :: FilePath
humanReadableExpected = "./golden-tests/expected-outputs/human-readable"

jsonExpected :: FilePath
jsonExpected = "./golden-tests/expected-outputs/json"

goldenTests :: TestableDerivations -> TestTree
goldenTests td = testGroup "Golden tests"
  [ goldenVsAction "Human readable, environment"
      humanReadableExpected (makeDiffTree' (diffContextEnv Word)) (toHumanReadable (renderContext Word))
  , goldenVsAction "JSON, environment"
      jsonExpected (makeDiffTree' (diffContextEnv Word)) toJson
  ]
  where
    makeDiffTree' = makeDiffTree td

    toHumanReadable ctx drv = runRender' (renderDiffHumanReadable drv) ctx
    toJson drv = (<> "\n") . decodeUtf8 . BS.toStrict $ (Data.Aeson.encode drv)

    indent = 0
    diffContextEnv orient = DiffContext orient True
    renderContext orient = RenderContext orient NotTTY indent

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "decode . encode == id" prop_DecodeEncodeID
  ]
  where
    prop_DecodeEncodeID :: DerivationDiff -> Bool
    prop_DecodeEncodeID dd = case decode (encode dd) of
      Nothing -> False
      Just dd' -> dd == dd'


data TestableDerivations = TestableDerivations
  { oldDerivation :: FilePath
  , newDerivation :: FilePath
  }
  deriving Show

initDerivations :: IO TestableDerivations
initDerivations = do
  (_, o) <- readProcessStdout "nix-instantiate ./golden-tests/old-derivation/drv.nix"
  (_, n) <- readProcessStdout "nix-instantiate ./golden-tests/new-derivation/drv.nix"
  pure (TestableDerivations (toFilePath o) (toFilePath n))
  where
    toFilePath = BS.unpack . BS.init -- drop new-line char

makeDiffTree :: TestableDerivations -> DiffContext -> IO DerivationDiff
makeDiffTree TestableDerivations{..} diffContext = do
  let status = Status Data.Set.empty
  let action = diff True oldDerivation (Data.Set.singleton "out") newDerivation (Data.Set.singleton "out")
  Control.Monad.State.evalStateT (Control.Monad.Reader.runReaderT (unDiff action) diffContext) status
