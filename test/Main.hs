module Main where

import Test.Tasty
import Test.Tasty.Silver.Interactive (interactiveTests)

import Golden.Utils (initSimpleDerivations, initComplexDerivations)
import Golden.Tests (goldenTests)
import Properties (properties)

main :: IO ()
main = do
  simpleTd  <- initSimpleDerivations
  complexTd <- initComplexDerivations
  defaultMainWithIngredients
    [ interactiveTests (const False)
    ]
    $ testGroup "Tests" [goldenTests simpleTd complexTd, properties]
