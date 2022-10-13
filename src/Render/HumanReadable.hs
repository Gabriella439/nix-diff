{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Render.HumanReadable where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Control.Monad.Reader
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import qualified Patience

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
#endif

import Diff


data RenderContext = RenderContext
  { orientation :: Orientation
  , tty         :: TTY
  , indent      :: Natural
  }

newtype Render a = Render { unRender :: ReaderT RenderContext IO a}
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader RenderContext
    , MonadIO
#if MIN_VERSION_base(4,9,0)
    , MonadFail
#endif
    )

echo :: Text -> Render ()
echo text = do
    RenderContext { indent } <- ask
    let n = fromIntegral indent
    liftIO (Text.IO.putStrLn (Text.replicate n " " <> text))

indented :: Natural -> Render a -> Render a
indented n = local adapt
  where
    adapt context = context { indent = indent context + n }

data TTY = IsTTY | NotTTY

-- | Color text red
red :: TTY -> Text -> Text
red  IsTTY text = "\ESC[1;31m" <> text <> "\ESC[0m"
red NotTTY text = text

-- | Color text background red
redBackground  :: Orientation -> TTY -> Text -> Text
redBackground Line IsTTY text = "\ESC[41m" <> prefix <> "\ESC[0m" <> suffix
  where
    (prefix, suffix) = Text.break lineBoundary text
redBackground Word IsTTY text = "\ESC[41m" <> prefix <> "\ESC[0m" <> suffix
  where
    (prefix, suffix) = Text.break wordBoundary text
redBackground Character IsTTY text = "\ESC[41m" <> text <> "\ESC[0m"
redBackground Line NotTTY text = "- " <> text
redBackground _    NotTTY text = "←" <> text <> "←"

-- | Color text green
green :: TTY -> Text -> Text
green IsTTY  text = "\ESC[1;32m" <> text <> "\ESC[0m"
green NotTTY text = text

-- | Color text background green
greenBackground :: Orientation -> TTY -> Text -> Text
greenBackground Line IsTTY text = "\ESC[42m" <> prefix <> "\ESC[0m" <> suffix
  where
    (prefix, suffix) = Text.break lineBoundary text
greenBackground Word IsTTY text = "\ESC[42m" <> prefix <> "\ESC[0m" <> suffix
  where
    (prefix, suffix) = Text.break wordBoundary text
greenBackground Character IsTTY  text = "\ESC[42m" <> text <> "\ESC[0m"
greenBackground Line NotTTY text = "+ " <> text
greenBackground _    NotTTY text = "→" <> text <> "→"

-- | Color text grey
grey :: Orientation -> TTY -> Text -> Text
grey _    IsTTY  text = "\ESC[1;2m" <> text <> "\ESC[0m"
grey Line NotTTY text = "  " <> text
grey _    NotTTY text = text

-- | Format the left half of a diff
minus :: TTY -> Text -> Text
minus tty text = red tty ("- " <> text)

-- | Format the right half of a diff
plus :: TTY -> Text -> Text
plus tty text = green tty ("+ " <> text)

-- | Format text explaining a diff
explain :: Text -> Text
explain text = "• " <> text

{-| Utility to automate a common pattern of printing the two halves of a diff.
    This passes the correct formatting function to each half
-}
renderWith :: Changed a -> ((Text -> Text, a) -> Render ()) -> Render ()
renderWith Changed{..} k = do
    RenderContext { tty } <- ask
    k (minus tty, before)
    k (plus  tty, now)

-- | Format the derivation outputs
renderOutputs :: Set Text -> Text
renderOutputs outputs =
    ":{" <> Text.intercalate "," (Data.Set.toList outputs) <> "}"

renderDiffHumanReadable :: DerivationDiff -> Render ()
renderDiffHumanReadable = \case
    DerivationsAreTheSame -> pure ()
    AlreadyCompared ->  echo (explain "These two derivations have already been compared")
    NamesDontMatch {..} -> do
      renderOutputStructure outputStructure
      echo (explain "The derivation names do not match")
    OutputsDontMatch {..} -> do
      renderOutputStructure outputStructure
      echo (explain "The requested outputs do not match")
    DerivationDiff {..} -> do
      renderOutputStructure outputStructure
      renderOutputsDiff outputsDiff
      renderPlatformDiff platformDiff
      renderBuilderDiff builderDiff
      renderArgsDiff argumentsDiff
      renderSrcDiff sourcesDiff
      renderInputExtraNames inputExtraNames
      mapM_ renderInputDiff inputsDiff
      renderEnvDiff envDiff

  where
    renderOutputStructure os =
      renderWith os \(sign, (path, outputs)) -> do
        echo (sign (Text.pack path <> renderOutputs outputs))

    renderOutputsDiff OutputsDiff{..} = do
      ifExist extraOutputs \eo -> do
        echo (explain "The set of outputs do not match:")
        renderWith eo \(sign, extraOutputs') -> do
          forM_ (Data.Map.toList extraOutputs') \(key, _value) -> do
              echo ("    " <> sign ("{" <> key <> "}"))
      mapM_ renderOutputHashDiff outputHashDiff

    renderOutputHashDiff OutputDiff{..} = do
      echo (explain ("{" <> outputName <> "}:"))
      echo (explain "    Hash algorithm:")
      renderWith hashDifference \(sign, hashAlgo) -> do
          echo ("        " <> sign hashAlgo)

    renderPlatformDiff mpd =
      ifExist mpd \pd -> do
        echo (explain "The platforms do not match")
        renderWith pd \(sign, platform) -> do
           echo ("    " <> sign platform)

    renderBuilderDiff mbd =
      ifExist mbd \bd -> do
        echo (explain "The builders do not match")
        renderWith bd \(sign, builder) -> do
          echo ("    " <> sign builder)

    renderArgsDiff mad =
      ifExist mad \ad -> do
        RenderContext { tty } <- ask
        echo (explain "The arguments do not match")
        let renderDiff (Patience.Old arg) =
                echo ("    " <> minus tty arg)
            renderDiff (Patience.New arg) =
                echo ("    " <> plus tty arg)
            renderDiff (Patience.Both arg _) =
                echo ("    " <> explain arg)
        mapM_ renderDiff ad

    renderSrcDiff SourcesDiff{..} = do
      ifExist extraSrcNames \esn -> do
        echo (explain "The set of input source names do not match:")
        renderWith esn \(sign, names) -> do
          forM_ names \name -> do
              echo ("    " <> sign name)

      mapM_ renderSrcFileDiff srcFilesDiff

    renderSrcFileDiff OneSourceFileDiff{..} = do
      echo (explain ("The input source named `" <> srcName <> "` differs"))
      ifExist srcContentDiff \scd -> do
        text <- renderText scd
        echo ("    " <> text)
    renderSrcFileDiff SomeSourceFileDiff{..} = do
      echo (explain ("The input sources named `" <> srcName <> "` differ"))
      renderWith srcFilesDiff \(sign, paths) -> do
        forM_ paths \path -> do
            echo ("    " <>  sign (Text.pack path))

    renderInputExtraNames mien =
      ifExist mien \ien -> do
        echo (explain "The set of input derivation names do not match:")
        renderWith ien \(sign, names) -> do
          forM_ names \name -> do
              echo ("    " <> sign name)

    renderInputDiff OneDerivationDiff{..} = do
      echo (explain ("The input derivation named `" <> drvName <> "` differs"))
      indented 2 (renderDiffHumanReadable drvDiff)
    renderInputDiff SomeDerivationsDiff{..} = do
      echo (explain ("The set of input derivations named `" <> drvName <> "` do not match"))
      renderWith extraPartsDiff \(sign, extraPaths) -> do
        forM_ (Data.Map.toList extraPaths) \(extraPath, outputs) -> do
          echo ("    " <> sign (Text.pack extraPath <> renderOutputs outputs))

    renderEnvDiff Nothing =
      echo (explain "Skipping environment comparison")
    renderEnvDiff (Just EnvironmentsAreEqual) = pure ()
    renderEnvDiff (Just EnvironmentDiff{..}) = do
      echo (explain "The environments do not match:")
      renderWith extraEnvDiff \(sign, extraEnv) -> do
        forM_ (Data.Map.toList extraEnv) \(key, value) -> do
            echo ("    " <> sign (key <> "=" <> value))
      forM_ envContentDiff \EnvVarDiff{..} -> do
        text <- renderText envValueDiff
        echo ("    " <> envKey <> "=" <> text)

    renderText :: [Patience.Item Text] -> Render Text
    renderText chunks = do
      RenderContext{ indent, orientation, tty } <- ask

      let n = fromIntegral indent

      let prefix = Text.replicate n " "

      let format text =
              if 80 <= n + Text.length text
              then "''\n" <> indentedText <> prefix <> "''"
              else text
            where
              indentedText =
                  (Text.unlines . fmap indentLine . Text.lines) text
                where
                  indentLine line = prefix <> "    " <> line

      let renderChunk (Patience.Old  l  ) =
              redBackground   orientation tty l
          renderChunk (Patience.New    r) =
              greenBackground orientation tty r
          renderChunk (Patience.Both l _) =
              grey            orientation tty l

      return (format (Text.concat (fmap renderChunk chunks)))

    ifExist m l = maybe (pure ()) l m
