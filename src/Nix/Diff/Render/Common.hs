{-# LANGUAGE CPP #-}

module Nix.Diff.Render.Common where

import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask, local)
import Control.Monad.Writer(MonadWriter, Writer, runWriter, tell)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.Set
import qualified Control.Monad.Reader
import qualified Data.Text as Text

#if !MIN_VERSION_base(4,15,1)
import Control.Monad.Fail (MonadFail)
#endif

import Nix.Diff
import Nix.Diff.Types


data RenderContext = RenderContext
  { orientation :: Orientation
  , tty         :: TTY
  , indent      :: Natural
  , title       :: Text
  , context     :: Maybe Natural
  }

newtype Render a = Render { unRender :: ReaderT RenderContext (Writer Text) a}
    deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader RenderContext
    , MonadWriter Text
    )

runRender :: Render a -> RenderContext ->  (a, Text)
runRender render rc = runWriter $  runReaderT render.unRender rc

runRender' :: Render () -> RenderContext -> Text
runRender' render = snd . runRender render

data TTY = IsTTY | NotTTY

tellLn :: Text -> Render ()
tellLn line = tell (line <> "\n")

echo :: Text -> Render ()
echo text = do
    RenderContext { indent } <- ask
    let n = fromIntegral indent
    tellLn (Text.replicate n " " <> text)

indented :: Natural -> Render a -> Render a
indented n = local adapt
  where
    adapt context = context { indent = context.indent + n }

-- | Format the derivation outputs
renderOutputs :: OutputNames -> Text
renderOutputs (OutputNames outputs) =
    ":{" <> Text.intercalate "," (Data.Set.toList outputs) <> "}"

ifExist :: Applicative f => Maybe a -> (a -> f ()) -> f ()
ifExist m l = maybe (pure ()) l m

zippers :: [a] -> [([a], a, [a])]
zippers = go []
  where
    go _           []  = []
    go prefix (x : xs) = (prefix, x, xs) : go (x : prefix) xs
