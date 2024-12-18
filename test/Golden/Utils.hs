module Golden.Utils where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Set
import qualified Control.Monad.Reader
import qualified Control.Monad.State
import Control.Monad (unless)
import Control.Exception ( throwIO, ErrorCall(ErrorCall) )
import System.Process.Typed

import Nix.Diff ( diff, Diff(unDiff), DiffContext, Status(Status) )
import Nix.Diff.Types ( DerivationDiff, OutputNames(OutputNames) )
import Nix.Diff.Store ( StorePath(StorePath) )

data TestableDerivations = TestableDerivations
  { oldDerivation :: StorePath
  , newDerivation :: StorePath
  }
  deriving Show

initSimpleDerivations :: IO TestableDerivations
initSimpleDerivations = do
  (oExit, o) <- readProcessStdout (nixInstantiate "./golden-tests/derivations/simple/old/drv.nix")
  (nExit, n) <- readProcessStdout (nixInstantiate "./golden-tests/derivations/simple/new/drv.nix")
  unless (oExit == ExitSuccess || nExit == ExitSuccess)
    (throwIO (ErrorCall  "Can't instantiate simple derivations"))
  pure (TestableDerivations (storePathFromCLI o) (storePathFromCLI n))

initComplexDerivations :: IO TestableDerivations
initComplexDerivations = do
  (oExit, o) <- readProcessStdout (nixPathInfo "./golden-tests/derivations/complex/old/")
  (nExit, n) <- readProcessStdout (nixPathInfo "./golden-tests/derivations/complex/new/")
  unless (oExit == ExitSuccess || nExit == ExitSuccess)
    (throwIO (ErrorCall  "Can't instantiate complex derivations"))
  pure (TestableDerivations (storePathFromCLI o) (storePathFromCLI n))

nixInstantiate :: FilePath -> ProcessConfig () () ()
nixInstantiate fp = shell ("nix-instantiate " <> fp)

nixPathInfo :: FilePath -> ProcessConfig () () ()
nixPathInfo fp = shell ("nix path-info --experimental-features 'nix-command flakes' --derivation " <> fp <> "#packages.x86_64-linux.default")

makeDiffTree :: TestableDerivations -> DiffContext -> IO DerivationDiff
makeDiffTree TestableDerivations{..} diffContext = do
  let status = Status Data.Set.empty
  let action = diff True oldDerivation (OutputNames (Data.Set.singleton "out")) newDerivation (OutputNames (Data.Set.singleton "out"))
  Control.Monad.State.evalStateT (Control.Monad.Reader.runReaderT action.unDiff diffContext) status

-- | Drop conventional stdout newline, convert to StorePath
storePathFromCLI :: BS.ByteString -> StorePath
storePathFromCLI = StorePath . BS.unpack . BS.init -- drop new-line char
