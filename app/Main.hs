{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import Control.Applicative ((<|>), optional)
import Data.Text (Text)
import Options.Applicative (Parser, ParserInfo)
import Numeric.Natural (Natural)

import qualified Control.Monad.Reader
import qualified Control.Monad.State
import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Set
import qualified Data.Text.IO as Text.IO
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified System.Posix.IO
import qualified System.Posix.Terminal

import Nix.Diff
import Nix.Diff.Store (StorePath(StorePath))
import Nix.Diff.Types
import Nix.Diff.Render.HumanReadable
import Nix.Diff.Transformations
import Data.Foldable (Foldable(fold))

data Color = Always | Auto | Never

data RenderRunner = HumanReadable | JSON

data TransformOptions = TransformOptions
  { foldAlreadyCompared :: Bool
  , squashTextDiff      :: Bool
  }

parseColor :: Parser Color
parseColor =
    Options.Applicative.option
        reader
        (   Options.Applicative.long "color"
        <>  Options.Applicative.help ("display colors always, automatically (if terminal detected), or never")
        <>  Options.Applicative.value Auto
        <>  Options.Applicative.metavar "(always|auto|never)"
        )
  where
    reader = do
        string <- Options.Applicative.str
        case string :: Text of
            "always" -> return Always
            "auto"   -> return Auto
            "never"  -> return Never
            _        -> fail "Invalid color"

parseLineOriented :: Parser Orientation
parseLineOriented =
        per "line" Line
    <|> per "character" Character
    <|> per "word" Word
    <|> pure Word
  where
    per x orientation =
        Options.Applicative.flag' orientation
            (   Options.Applicative.long (x <> "-oriented")
            <>  Options.Applicative.help ("Display textual differences on a per-" <> x <> " basis")
            )

parseContext :: Parser Natural
parseContext =
    Options.Applicative.option
        Options.Applicative.auto
        (   Options.Applicative.long "context"
        <>  Options.Applicative.help "Limit context to N lines/words/characters (depending on mode)"
        )

parseEnvironment :: Parser Bool
parseEnvironment =
    Options.Applicative.switch
        (   Options.Applicative.long "environment"
        <>  Options.Applicative.help "Force display of environment differences"
        )

parseRenderRunner :: Parser RenderRunner
parseRenderRunner = json <|> pure HumanReadable
  where
    json = Options.Applicative.flag' JSON
      (  Options.Applicative.long "json"
      <> Options.Applicative.help "Print output in JSON format"
      )

parseTransformOptions :: Parser TransformOptions
parseTransformOptions = do
  foldAlreadyCompared <- parseReduceAlreadyCompared
  squashTextDiff      <- parseSquashTextDiff

  pure TransformOptions{..}
  where
    parseReduceAlreadyCompared =
      Options.Applicative.switch
          (   Options.Applicative.long "skip-already-compared"
          <>  Options.Applicative.help "Fold subtrees, that changed only by already compared input"
          )
    parseSquashTextDiff =
      Options.Applicative.switch
          (   Options.Applicative.long "squash-text-diff"
          <>  Options.Applicative.help (fold
                                      ["Squash text diffs into the lagest spans. It's most useful ",
                                       "with json output. ",
                                       "WARNING: can break some parts of human readable output."])
          )

data Options = Options
    { left             :: FilePath
    , right            :: FilePath
    , color            :: Color
    , orientation      :: Orientation
    , environment      :: Bool
    , renderRunner     :: RenderRunner
    , transformOptions :: TransformOptions
    , context          :: Maybe Natural
    }

parseOptions :: Parser Options
parseOptions = do
    left             <- parseLeft
    right            <- parseRight
    color            <- parseColor
    orientation      <- parseLineOriented
    environment      <- parseEnvironment
    renderRunner     <- parseRenderRunner
    transformOptions <- parseTransformOptions
    context          <- optional parseContext

    return (Options { .. })
  where
    parseFilePath metavar = do
        Options.Applicative.strArgument
            (Options.Applicative.metavar metavar)

    parseLeft = parseFilePath "LEFT"

    parseRight = parseFilePath "RIGHT"

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.header "Explain why two derivations differ"
        )

transformDiff :: TransformOptions -> DerivationDiff -> DerivationDiff
transformDiff TransformOptions{..}
  = transformIf foldAlreadyCompared foldAlreadyComparedSubTrees
  . transformIf squashTextDiff      squashSourcesAndEnvsDiff
  . foldManyInputDerivationsAlreadyCompared

renderDiff :: RenderRunner -> RenderContext -> DerivationDiff -> IO ()
renderDiff HumanReadable context derivation
  = Text.IO.putStr $ runRender' (renderDiffHumanReadable derivation) context
renderDiff JSON _ derivation = Data.ByteString.Lazy.Char8.putStrLn (Data.Aeson.encode derivation)

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Options { .. } <- Options.Applicative.execParser parserInfo

    tty <- case color of
        Never -> do
            return NotTTY
        Always -> do
            return IsTTY
        Auto -> do
            b <- System.Posix.Terminal.queryTerminal System.Posix.IO.stdOutput
            return (if b then IsTTY else NotTTY)

    let indent = 0
    let diffContext = DiffContext {..}
    let renderContext = RenderContext {..}
    let status = Status Data.Set.empty
    let action = diff True (StorePath left) (Data.Set.singleton "out") (StorePath right) (Data.Set.singleton "out")
    diffTree <- Control.Monad.State.evalStateT (Control.Monad.Reader.runReaderT (unDiff action) diffContext) status
    let diffTree' =
          transformDiff transformOptions diffTree
    renderDiff renderRunner renderContext diffTree'
