{-# LANGUAGE OverloadedStrings #-}
module Golden.Tests where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text.Encoding
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.Silver (goldenVsAction)

import Nix.Diff
import Nix.Diff.Transformations
import Nix.Diff.Render.HumanReadable

import Golden.Utils ( TestableDerivations, makeDiffTree )

goldenTests :: TestableDerivations -> TestableDerivations -> TestTree
goldenTests simpleTd complexTd = testGroup "Golden tests"
  [ goldenVsAction "Human readable, environment"
      humanReadableExpected
      simple_env_words
      humanReadable_words

  , goldenVsAction "JSON, environment"
      jsonExpected (mkDTSimple (diffContextEnv Word)) toJson

  , goldenVsAction "Human readable, complex, skip already compared"
      skipAlreadyComparedExpected
      complex_words
      (humanReadable_words . foldAlreadyComparedSubTrees)
  ]
  where
    toJson drv = (<> "\n") . decodeUtf8 . BS.toStrict $ (Data.Aeson.encode drv)

    humanReadable_words = toHumanReadable (renderContext Word)
    toHumanReadable ctx drv = runRender' (renderDiffHumanReadable drv) ctx

    simple_env_words = mkDTSimple (diffContextEnv Word)
    complex_words = mkDTComplex (diffContext Word)

    mkDTSimple = makeDiffTree simpleTd
    mkDTComplex = makeDiffTree complexTd

    indent' = 0
    diffContextEnv orient = DiffContext orient True
    diffContext orient = DiffContext orient False
    renderContext orient = RenderContext orient NotTTY indent'

humanReadableExpected :: FilePath
humanReadableExpected = "./golden-tests/expected-outputs/human-readable"

jsonExpected :: FilePath
jsonExpected = "./golden-tests/expected-outputs/json"

skipAlreadyComparedExpected :: FilePath
skipAlreadyComparedExpected = "./golden-tests/expected-outputs/skip-already-compared"
