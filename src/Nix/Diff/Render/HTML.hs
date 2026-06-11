{-# LANGUAGE CPP #-}

module Nix.Diff.Render.HTML where

import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Patience

#if !MIN_VERSION_base(4,15,1)
import Control.Monad.Fail (MonadFail)
#endif

import Nix.Diff
import Nix.Diff.Types
import Nix.Diff.Render.Common
import qualified Nix.Diff.Store as Store


-- | Format deleted line
deletedLine :: Text -> Text
deletedLine = htmlTextWith "li" [ "class=\"del\"" ] . deletedText

-- | Format and render deleted line
renderDeletedLine :: Text -> Render ()
renderDeletedLine = echo . deletedLine

-- | Format deleted text
deletedText :: Text -> Text
deletedText = htmlText "del"

-- | Format deleted word
deletedWord  :: Orientation -> Text -> Text
deletedWord Line = deletedLine
deletedWord _    = deletedText

-- | Format inserted line
insertedLine :: Text -> Text
insertedLine = htmlTextWith "li" [ "class=\"ins\"" ] . insertedText

-- | Format and render inserted line
renderInsertedLine :: Text -> Render ()
renderInsertedLine = echo . insertedLine

-- | Format inserted text
insertedText :: Text -> Text
insertedText = htmlText "ins"

-- | Format inserted word
insertedWord :: Orientation -> Text -> Text
insertedWord Line = insertedLine
insertedWord _    = insertedText

-- | Format normal text
normalText :: Text -> Text
normalText text = text

-- | Format normal line
normalLine :: Text -> Text
normalLine = htmlTextWith "li" [ "class=\"normal\"" ] . normalText

-- | Format and render normal line
renderNormalLine :: Text -> Render ()
renderNormalLine = echo . normalLine

-- | Format normal text
normalWord :: Orientation -> Text -> Text
normalWord Line = normalLine
normalWord _    = normalText

-- | Format details with summary
details :: Text -> Render () -> Render ()
details summary body = do
  htmlBlockWith "li" [ "class=\"details\"" ] do
    htmlBlockWith "details" [ "open" ] do
      htmlLine "summary" summary
      body

-- | Format code
code :: Text -> Text
code = htmlText "code"

-- | Format HTML block
htmlBlock :: Text -> Render () -> Render ()
htmlBlock tag = htmlBlockWith tag []

htmlBlockWith :: Text -> [Text] -> Render () -> Render ()
htmlBlockWith tag attrs body = do
  let attrs' = (Text.concat . fmap (\attr -> " " <> attr)) attrs
  let opening = "<" <> tag <> attrs' <> ">"
  let closing = "</" <> tag <> ">"
  echo opening
  indented 2 body
  echo closing

-- | Format HTML line
htmlLine :: Text -> Text -> Render ()
htmlLine tag = echo . (htmlText tag)

-- | Format HTML text
htmlText :: Text -> Text -> Text
htmlText tag = htmlTextWith tag []

htmlTextWith :: Text -> [Text] -> Text -> Text
htmlTextWith tag attrs body = do
  let attrs' = (Text.concat . fmap (\attr -> " " <> attr)) attrs
  let opening = "<" <> tag <> attrs' <> ">"
  let closing = "</" <> tag <> ">"
  opening <> body <> closing

-- | Render (unsorted) list
renderList :: Render () -> Render ()
renderList = htmlBlock "ul"

{-| Utility to automate a common pattern of printing the two halves of a diff.
    This passes the correct formatting function to each half
-}
renderWith :: Changed a -> ((Text -> Render (), a) -> Render ()) -> Render ()
renderWith Changed{..} k = do
    RenderContext {} <- ask
    k (renderDeletedLine,   before)
    k (renderInsertedLine,  now)

renderWith' :: Changed a -> ((Text -> Render (), a) -> Render ()) -> Render ()
renderWith' changed = renderList . (renderWith changed)

renderOutputStructure :: Changed OutputStructure -> Render ()
renderOutputStructure os = do
  renderWith os \(render, OutputStructure path outputs) -> do
    let inner = Store.toText path <> renderOutputs outputs
    (render . code) inner

renderOutputsDiff :: OutputsDiff -> Render ()
renderOutputsDiff OutputsDiff{..} = do
  ifExist extraOutputs \eo -> do
    details "The set of outputs do not match:" do
      renderWith' eo \(render, extraOutputs') -> do
        forM_ (Map.toList extraOutputs') \(key, _value) -> do
          render ("{" <> (code key) <> "}")
  mapM_ renderOutputHashDiff outputHashDiff

renderOutputHashDiff :: OutputDiff -> Render ()
renderOutputHashDiff OutputDiff{..} = do
  renderNormalLine ("{" <> (code outputName) <> "}:")
  renderNormalLine "    Hash algorithm:"
  renderWith' hashDifference \(render, hashAlgo) -> render hashAlgo

renderPlatformDiff :: Maybe (Changed Text) -> Render ()
renderPlatformDiff mpd =
  ifExist mpd \pd -> do
    details "The platforms do not match:" do
      renderWith' pd \(render, platform) -> (render . code) platform

renderBuilderDiff :: Maybe (Changed Text) -> Render ()
renderBuilderDiff mbd =
  ifExist mbd \bd -> do
    details "The builders do not match:" do
      renderWith' bd \(render, builder) -> render builder

renderArgsDiff :: Maybe ArgumentsDiff -> Render ()
renderArgsDiff mad =
  ifExist mad \(ArgumentsDiff ad) -> do
    RenderContext {} <- ask
    details "The arguments do not match:" do
      let renderDiff (Patience.Old  lhs    ) = renderDeletedLine   lhs
          renderDiff (Patience.New      rhs) = renderInsertedLine  rhs
          renderDiff (Patience.Both lhs _  ) = renderNormalLine    lhs
      mapM_ renderDiff ad

renderSrcDiff :: SourcesDiff -> Render ()
renderSrcDiff SourcesDiff{..} = do
  ifExist extraSrcNames \esn -> do
    details "The set of input source names do not match:" do
      renderWith' esn \(render, srcNames) -> do
        forM_ srcNames (render . code)

  mapM_ renderSrcFileDiff srcFilesDiff

renderSrcFileDiff :: SourceFileDiff -> Render ()
renderSrcFileDiff OneSourceFileDiff{..} = do
  renderNormalLine ("The input source named " <> (code srcName) <> " differss")
  ifExist srcContentDiff \scd -> do
    text <- renderText scd
    echo text
renderSrcFileDiff SomeSourceFileDiff{..} = do
  details ("The input sources named " <> (code srcName) <> " differ:") do
    renderWith' srcFileDiff \(render, paths) -> do
      forM_ paths (render . code . Store.toText)

renderInputsDiff :: InputsDiff -> Render ()
renderInputsDiff InputsDiff{..} = do
  renderInputExtraNames inputExtraNames
  mapM_ renderInputDerivationsDiff inputDerivationDiffs

renderInputExtraNames :: Foldable t => Maybe (Changed (t Text)) -> Render ()
renderInputExtraNames mien =
  ifExist mien \ien -> do
    details "The set of input derivation names do not match:" do
      renderWith' ien \(render, drvNames) -> forM_ drvNames (render . code)

renderInputDerivationsDiff :: InputDerivationsDiff -> Render ()
renderInputDerivationsDiff OneDerivationDiff{..} = do
  let summary = "The input derivation named " <> (code drvName) <> " differs:"
  details summary do
    renderDiffInnerHTML' drvDiff
renderInputDerivationsDiff SomeDerivationsDiff{..} = do
  let summary = "The set of input derivations named " <> (code drvName) <> " do not match:"
  details summary do
    renderWith' extraPartsDiff \(render, extraPaths) -> do
      forM_ (Map.toList extraPaths) \(extraPath, outputs) -> do
        let inner = Store.toText extraPath <> renderOutputs outputs
        (render . code) inner
renderInputDerivationsDiff ManyDerivationsAlreadyComparedDiff{..} = do
  details "Input derivations differ but have already been compared:" do
    renderList (forM_ (Set.toList drvNames) (renderNormalLine . code))

renderEnvDiff :: Maybe EnvironmentDiff -> Render ()
renderEnvDiff Nothing = renderNormalLine "Skipping environment comparison"
renderEnvDiff (Just EnvironmentsAreEqual) = pure ()
renderEnvDiff (Just EnvironmentDiff{..}) = do
  details "The environments do not match:" do
    let Changed {..} = extraEnvDiff
    if Map.null before && Map.null now then pure ()
    else do
          renderWith' extraEnvDiff \(render, extraEnv) -> do
            forM_ (Map.toList extraEnv) \(key, value) -> do
              let text = key <> "=" <> value
              (render . code) text

    renderList do
      forM_ envContentDiff \EnvVarDiff{..} -> do
        text <- renderText envValueDiff
        let text' = envKey <> "=" <> text
        (renderNormalLine . code) text'

renderText :: TextDiff -> Render Text
renderText (TextDiff chunks) = do
  RenderContext{ orientation, context } <- ask

  let format text = do
        if Text.any (\c -> c == '\n') text
        then "''\n<pre>\n" <> text <> "''\n</pre>"
        else "\"" <> text <> "\""

  let renderChunk (Patience.Old  l  ) = deletedWord   orientation l
      renderChunk (Patience.New    r) = insertedWord  orientation r
      renderChunk (Patience.Both l _) = normalWord    orientation l
  let renderChunks = fmap renderChunk

  let windowedChunks = case context of
        Nothing -> chunks
        Just m  -> do
          let notBoth (Patience.Both _ _) = False
              notBoth  _                  = True

          let predicate (before, line, after) = do
                let nat = fromIntegral m
                let before' = take nat before
                let after' = take nat after
                any notBoth (line : before' ++ after')

          let middle (_, line, _) = line

          (fmap middle . filter predicate . zippers) chunks

  (return . format . Text.concat . renderChunks) windowedChunks

renderDiffHTML :: DerivationDiff -> Render ()
renderDiffHTML drv_diff = do
  RenderContext{ title } <- ask
  echo "<!DOCTYPE html>"
  htmlBlock "html" do
    htmlBlock "head" do
      htmlLine "title" title
      htmlBlock "style" do
        echo "del { color: red; }"
        echo "ins { color: green; }"
        echo "li { list-style-position: inside; }"
        echo "li.del { list-style-type: '− '; }"
        echo "li.ins { list-style-type: '+ '; }"
        echo "li.details { list-style-type: none; }"

    htmlBlock "body" do
      (renderDiffInnerHTML' drv_diff)

renderDiffInnerHTML :: DerivationDiff -> Render ()
renderDiffInnerHTML = \case
    DerivationsAreTheSame -> renderNormalLine "The derivations are the same."
    AlreadyCompared ->  renderNormalLine "These two derivations have already been compared."
    OnlyAlreadyComparedBelow {..} -> do
      renderOutputStructure outputStructure
      renderNormalLine "Skipping because only derivations that have already been compared and shown in the diff are below."
    NamesDontMatch {..} -> do
      renderOutputStructure outputStructure
      renderNormalLine "The derivation names do not match."
    OutputsDontMatch {..} -> do
      renderOutputStructure outputStructure
      renderNormalLine "The requested outputs do not match."
    DerivationDiff {..} -> do
      renderOutputStructure outputStructure
      renderOutputsDiff outputsDiff
      renderPlatformDiff platformDiff
      renderBuilderDiff builderDiff
      renderArgsDiff argumentsDiff
      renderSrcDiff sourcesDiff
      renderInputsDiff inputsDiff
      renderEnvDiff envDiff

renderDiffInnerHTML' :: DerivationDiff -> Render ()
renderDiffInnerHTML' = (renderList . renderDiffInnerHTML)
