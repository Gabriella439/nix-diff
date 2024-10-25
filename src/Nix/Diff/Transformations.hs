module Nix.Diff.Transformations where

import qualified Patience
import Data.Generics.Uniplate.Data ( transformBi )

import Nix.Diff.Types
import qualified Data.Set as Set
import Data.Text (Text)

{-| In large diffs there may be a lot of derivations
    that doesn't change at all, but changed some of
    its nested inputs, that was already compared.
    This case will produce "stairs" of useless reports:
    ```
    • The input derivation named `a` differs
      - /nix/store/j1jmbxd74kzianaywml2nw1ja31a00r5-a.drv:{out}
      + /nix/store/ww51c2dha7m5l5qjzh2rblicsamkrh62-a.drv:{out}
      • The input derivation named `b` differs
        - /nix/store/j1jmbxd74kzianaywml2nw1ja31a00r5-b.drv:{out}
        + /nix/store/ww51c2dha7m5l5qjzh2rblicsamkrh62-b.drv:{out}
        • The input derivation named `c` differs
          • These two derivations have already been compared
    ```
    This transformation will fold all these subtrees of diff
    into one OnlyAlreadyComparedBelow.
-}
foldAlreadyComparedSubTrees :: DerivationDiff -> DerivationDiff
foldAlreadyComparedSubTrees dd = case dd of
  DerivationsAreTheSame -> dd
  AlreadyCompared -> dd
  OnlyAlreadyComparedBelow{} -> dd
  NamesDontMatch{} -> dd
  OutputsDontMatch{} -> dd
  DerivationDiff{..} -> if
      | OutputsDiff Nothing [] <- outputsDiff
      , Nothing <- platformDiff
      , Nothing <- builderDiff
      , Nothing <- argumentsDiff
      , SourcesDiff Nothing [] <- sourcesDiff
      , InputsDiff Nothing inputs <- inputsDiff'
      , all alreadyComparedBelow inputs
      , envSkippedOrUnchanged envDiff
          -> OnlyAlreadyComparedBelow outputStructure

      | otherwise -> DerivationDiff
          { outputStructure
          , outputsDiff
          , platformDiff
          , builderDiff
          , argumentsDiff
          , sourcesDiff
          , inputsDiff = inputsDiff'
          , envDiff
          }
    where
      inputsDiff' = transformNestedDerivationDiffs
            foldAlreadyComparedSubTrees
            inputsDiff

{-| If packages deep in the dependency graph have been changed, many other
   derivations will also change in an uninteresting manner. This can lead to
   hundreds or thousands of lines of output like this:

   ```
   • The input derivation named `bash-5.2p32` differs
     • These two derivations have already been compared
   • The input derivation named `ensure-newer-sources-hook` differs
     • These two derivations have already been compared
   • The input derivation named `pypa-install-hook` differs
     • These two derivations have already been compared
   ```

   This transformation will fold sequences of `OneDerivationDiff` like this
   into a single `ManyDerivationsAlreadyComparedDiff`.
-}
foldManyInputDerivationsAlreadyCompared :: DerivationDiff -> DerivationDiff
foldManyInputDerivationsAlreadyCompared dd = case dd of
  DerivationsAreTheSame -> dd
  AlreadyCompared -> dd
  OnlyAlreadyComparedBelow{} -> dd
  NamesDontMatch{} -> dd
  OutputsDontMatch{} -> dd
  DerivationDiff{..} ->
    let inputsDiff' = transformNestedDerivationDiffs
                        foldManyInputDerivationsAlreadyCompared
                        inputsDiff

        helper :: [Text] -> [InputDerivationsDiff] -> [InputDerivationsDiff]
        helper [] [] = []
        helper names [] =
          [ManyDerivationsAlreadyComparedDiff
            { drvNames = Set.fromList names
            }]
        helper names (input:inputs) =
          case input of
            OneDerivationDiff
              { drvName
              , drvDiff = AlreadyCompared
              } -> helper
                    (drvName:names)
                    inputs
            OneDerivationDiff{} -> input : helper names inputs
            SomeDerivationsDiff{} -> input : helper names inputs
            ManyDerivationsAlreadyComparedDiff{} -> error "unreachable"

     in DerivationDiff
       { inputsDiff =
           inputsDiff'
             { inputDerivationDiffs =
                 helper [] (inputDerivationDiffs inputsDiff')
             }
       , ..
       }

{-| This transformation is most useful for
    --json output, because it will sqash a lot of
    `{"content":"  ","type":"Both"},{"content":"When","type":"Both"},{"content":" ","type":"Both"},{"content":"in","type":"Both"},{"content":" ","type":"Both"}`
    into one
    `{"content":"  When in ","type":"Both"}`
    block.

    To understand this problem clearer, see `golden-tests/expected-outputs/json`
    and `golden-tests/expected-outputs/json-squashed`.

    _Warning_: this transformation can break some parts of printing in
    human readable mode.
-}
squashSourcesAndEnvsDiff :: DerivationDiff -> DerivationDiff
squashSourcesAndEnvsDiff = transformBi
    \(TextDiff x) -> TextDiff (squashDiff x)
  where
    squashDiff (Patience.Old a : Patience.Old b : xs) =
      squashDiff (Patience.Old (a <> b) : xs)
    squashDiff (Patience.New a : Patience.New b : xs) =
      squashDiff (Patience.New (a <> b) : xs)
    squashDiff (Patience.Both a _ : Patience.Both b _ : xs) =
      let ab = a <> b in squashDiff (Patience.Both ab ab : xs)
    squashDiff (x : xs) = x : squashDiff xs
    squashDiff [] = []

-- ** Helpers

transformNestedDerivationDiffs
  :: (DerivationDiff -> DerivationDiff)
  -> InputsDiff
  -> InputsDiff
transformNestedDerivationDiffs f InputsDiff{..} = InputsDiff
  { inputExtraNames
  , inputDerivationDiffs = map changeDerivation inputDerivationDiffs
  }
  where
    changeDerivation idd = case idd of
      OneDerivationDiff name dd ->
        OneDerivationDiff name (f dd)
      SomeDerivationsDiff {} -> idd
      ManyDerivationsAlreadyComparedDiff {} -> idd

envSkippedOrUnchanged :: Maybe EnvironmentDiff -> Bool
envSkippedOrUnchanged = \case
  Nothing -> True
  Just EnvironmentsAreEqual -> True
  _ -> False

alreadyComparedBelow :: InputDerivationsDiff -> Bool
alreadyComparedBelow = \case
  OneDerivationDiff _ AlreadyCompared -> True
  OneDerivationDiff _ OnlyAlreadyComparedBelow{} -> True
  _ -> False

transformIf :: Bool -> (DerivationDiff -> DerivationDiff) -> DerivationDiff -> DerivationDiff
transformIf False _ = id
transformIf True f = f
