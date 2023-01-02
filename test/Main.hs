module Main where

import Test.Tasty

import Golden.Utils (initSimpleDerivations, initComplexDerivations)
import Golden.Tests (goldenTests)
import Properties (properties)

main :: IO ()
main = do
  simpleTd  <- initSimpleDerivations
  complexTd <- initComplexDerivations
  defaultMain $
     testGroup "Tests" [goldenTests simpleTd complexTd, properties]
