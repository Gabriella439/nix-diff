let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/pending/haskell-ci.dhall

let Prelude = https://prelude.dhall-lang.org/package.dhall

let matrixStepsNoHaddoc =
      let steps = haskellCi.matrixSteps

      in  Prelude.List.take
            ( Prelude.Natural.subtract
                1
                (Prelude.List.length haskellCi.BuildStep steps)
            )
            haskellCi.BuildStep
            steps

in        haskellCi.generalCi
            matrixStepsNoHaddoc
            ( Some
                { ghc =
                  [ haskellCi.GHC.GHC902
                  , haskellCi.GHC.GHC8107
                  , haskellCi.GHC.GHC884
                  , haskellCi.GHC.GHC865
                  ]
                , cabal = [ haskellCi.Cabal.Cabal34 ]
                }
            )
      //  { on = [ haskellCi.Event.push, haskellCi.Event.pull_request ] }
    : haskellCi.CI.Type
