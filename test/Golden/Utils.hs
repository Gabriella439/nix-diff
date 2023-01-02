{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Golden.Utils where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Set
import qualified Control.Monad.Reader
import qualified Control.Monad.State
import Control.Monad (unless)
import Control.Exception ( throwIO, ErrorCall(ErrorCall) )
import System.Process.Typed

import Nix.Diff ( diff, Diff(unDiff), DiffContext, Status(Status) )
import Nix.Diff.Types ( DerivationDiff )

data TestableDerivations = TestableDerivations
  { oldDerivation :: FilePath
  , newDerivation :: FilePath
  }
  deriving Show

initSimpleDerivations :: IO TestableDerivations
initSimpleDerivations = do
  (oExit, o) <- readProcessStdout (nixInstantiate "./golden-tests/derivations/simple/old/drv.nix")
  (nExit, n) <- readProcessStdout (nixInstantiate "./golden-tests/derivations/simple/new/drv.nix")
  unless (oExit == ExitSuccess || nExit == ExitSuccess)
    (throwIO (ErrorCall  "Can't instantiate simple derivations"))
  pure (TestableDerivations (toFilePath o) (toFilePath n))
  where
    toFilePath = BS.unpack . BS.init -- drop new-line char

initComplexDerivations :: IO TestableDerivations
initComplexDerivations = do
  (oExit, o) <- readProcessStdout (nixPathInfo "./golden-tests/derivations/complex/old/")
  (nExit, n) <- readProcessStdout (nixPathInfo "./golden-tests/derivations/complex/new/")
  unless (oExit == ExitSuccess || nExit == ExitSuccess)
    (throwIO (ErrorCall  "Can't instantiate complex derivations"))
  pure (TestableDerivations (toFilePath o) (toFilePath n))
  where
    toFilePath = BS.unpack . BS.init -- drop new-line char

nixInstantiate :: FilePath -> ProcessConfig () () ()
nixInstantiate fp = shell ("nix-instantiate " <> fp)

nixPathInfo :: FilePath -> ProcessConfig () () ()
nixPathInfo fp = shell ("nix path-info --experimental-features 'nix-command flakes' --derivation " <> fp)

makeDiffTree :: TestableDerivations -> DiffContext -> IO DerivationDiff
makeDiffTree TestableDerivations{..} diffContext = do
  let status = Status Data.Set.empty
  let action = diff True oldDerivation (Data.Set.singleton "out") newDerivation (Data.Set.singleton "out")
  Control.Monad.State.evalStateT (Control.Monad.Reader.runReaderT (unDiff action) diffContext) status
