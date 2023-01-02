module Properties where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Aeson

import Nix.Diff.Types
import Nix.Diff.Transformations

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "decode . encode == id" prop_DecodeEncodeID
  , testProperty "squash . squash == squash" prop_SqashTextDiffs
  ]
  where
    prop_DecodeEncodeID :: DerivationDiff -> Bool
    prop_DecodeEncodeID dd = case decode (encode dd) of
      Nothing -> False
      Just dd' -> dd == dd'

    prop_SqashTextDiffs :: DerivationDiff -> Bool
    prop_SqashTextDiffs dd = (op . op $ dd) == (op dd)
      where
        op = squashSourcesAndEnvsDiff
