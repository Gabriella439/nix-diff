{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Nix.Diff.Transformations where

import qualified Patience
import Data.Generics.Uniplate.Data ( transformBi )

import Nix.Diff.Types

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
    into one OnlyAlreadComparedBelow.
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

{-| This transfomration is most usefull for
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
