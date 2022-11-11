module Properties where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Aeson

import Nix.Diff.Types

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "decode . encode == id" prop_DecodeEncodeID
  ]
  where
    prop_DecodeEncodeID :: DerivationDiff -> Bool
    prop_DecodeEncodeID dd = case decode (encode dd) of
      Nothing -> False
      Just dd' -> dd == dd'
