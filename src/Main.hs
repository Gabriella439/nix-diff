{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Algorithm.Diff (Diff(..))
import Data.Attoparsec.Text (IResult(..))
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Filesystem.Path (FilePath)
import Nix.Derivation (Derivation)
import Options.Generic (Generic, ParseRecord)
import Prelude hiding (FilePath)

import qualified Data.Algorithm.Diff
import qualified Data.Attoparsec.Text
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.IO
import qualified Filesystem.Path.CurrentOS
import qualified Nix.Derivation
import qualified Options.Generic

data Options w = Options FilePath FilePath
    deriving (Generic, ParseRecord)

pathToText :: FilePath -> Text
pathToText path =
    case Filesystem.Path.CurrentOS.toText path of
        Left  text -> text
        Right text -> text

derivationName :: FilePath -> Text
derivationName = Data.Text.dropEnd 4 . Data.Text.drop 44 . pathToText

groupByName :: Map FilePath (Set Text) -> Map Text (Map FilePath (Set Text))
groupByName m = Data.Map.fromList assocs
  where
    toAssoc key = (derivationName key, Data.Map.filterWithKey predicate m)
      where
        predicate key' _ = derivationName key == derivationName key'

    assocs = fmap toAssoc (Data.Map.keys m)

readInputs :: FilePath -> IO Derivation
readInputs path = do
    let string = Filesystem.Path.CurrentOS.encodeString path
    text <- Data.Text.IO.readFile string
    case Data.Attoparsec.Text.parse Nix.Derivation.parseDerivation text of
        Done _ derivation -> return derivation
        _                 -> fail "Could not parse derivation"

innerJoin :: Ord k => Map k a -> Map k b -> Map k (a, b)
innerJoin = Data.Map.mergeWithKey both left right
  where
    both _ a b = Just (a, b)

    left _ = Data.Map.empty

    right _ = Data.Map.empty

red :: Text -> Text
red text = "\ESC[1;31m" <> text <> "\ESC[0m"

green :: Text -> Text
green text = "\ESC[1;32m" <> text <> "\ESC[0m"

grey :: Text -> Text
grey text = "\ESC[1;2m" <> text <> "\ESC[0m"

minus :: Text -> Text
minus text = red ("- " <> text)

plus :: Text -> Text
plus text = green ("+ " <> text)

explain :: Text -> Text
explain text = "• " <> text

diffWith :: Monad m => a -> a -> ((Text -> Text, a) -> m ()) -> m ()
diffWith l r k = do
    k (minus, l)
    k (plus , r)

renderOutputs :: Set Text -> Text
renderOutputs outputs =
    ":{" <> Data.Text.intercalate "," (Data.Set.toList outputs) <> "}"

diffText :: Text -> Text -> Text
diffText left right = Data.Text.concat (fmap renderChunk chunks)
  where
    leftString  = Data.Text.unpack left
    rightString = Data.Text.unpack right

    chunks = Data.Algorithm.Diff.getGroupedDiff leftString rightString

    renderChunk (First  l) = green (Data.Text.pack l)
    renderChunk (Second r) = red   (Data.Text.pack r)
    renderChunk (Both l _) = grey  (Data.Text.pack l)

diff :: Int -> FilePath -> Set Text -> FilePath -> Set Text -> IO ()
diff indent leftPath leftOutputs rightPath rightOutputs = do
    if leftPath == rightPath
    then return ()
    else do
        diffWith (leftPath, leftOutputs) (rightPath, rightOutputs) $ \(sign, (path, outputs)) -> do
            echo (sign (pathToText path <> renderOutputs outputs))

        if derivationName leftPath /= derivationName rightPath
        then do
            echo (explain "The derivation names do not match")

        else do
            leftDerivation  <- readInputs leftPath
            rightDerivation <- readInputs rightPath
            let rightInputs = groupByName (Nix.Derivation.inputDrvs leftDerivation)
            let leftInputs  = groupByName (Nix.Derivation.inputDrvs rightDerivation)
    
            let leftNames  = Data.Map.keysSet leftInputs
            let rightNames = Data.Map.keysSet rightInputs
            let leftExtraNames  = Data.Set.difference leftNames  rightNames
            let rightExtraNames = Data.Set.difference rightNames leftNames

            if leftNames /= rightNames
            then do
                echo (explain "The set of input names do not match:")
                diffWith leftExtraNames rightExtraNames $ \(sign, names) -> do
                    forM_ names $ \name -> do
                        echo ("    " <> sign name)
            else do 
                let assocs = Data.Map.toList (innerJoin leftInputs rightInputs)
                descended <- forM assocs $ \(inputName, (leftPaths, rightPaths)) -> do
                    let leftExtraPaths =
                            Data.Map.difference leftPaths  rightPaths
                    let rightExtraPaths =
                            Data.Map.difference rightPaths leftPaths
                    case (Data.Map.toList leftExtraPaths, Data.Map.toList rightExtraPaths) of
                        _   | leftPaths == rightPaths -> do
                            return False
                        ([(leftPath', leftOutputs')], [(rightPath', rightOutputs')])
                            | leftOutputs' == rightOutputs' -> do
                            echo (explain ("The input named `" <> inputName <> "` differs"))
                            diff (indent + 2) leftPath' leftOutputs' rightPath' rightOutputs'
                            return True
                        _ -> do
                            echo (explain ("The set of inputs named `" <> inputName <> "` do not match"))
                            diffWith leftExtraPaths rightExtraPaths $ \(sign, extraPaths) -> do
                                forM_ (Data.Map.toList extraPaths) $ \(extraPath, outputs) -> do
                                    echo ("    " <> sign (pathToText extraPath <> renderOutputs outputs))
                            return False
                if or descended
                then return ()
                else do
                    -- Display a richer diff for this derivation if there was no
                    -- difference in our input derivations
                    let leftEnv  = Nix.Derivation.env leftDerivation
                    let rightEnv = Nix.Derivation.env rightDerivation

                    let leftExtraEnv  = Data.Map.difference leftEnv rightEnv
                    let rightExtraEnv = Data.Map.difference leftEnv rightEnv

                    let bothEnv = innerJoin leftEnv rightEnv

                    diffWith leftExtraEnv rightExtraEnv $ \(sign, extraEnv) -> do
                        forM_ (Data.Map.toList extraEnv) $ \(key, value) -> do
                            echo (sign (key <> "=" <> value))
                    forM_ (Data.Map.toList bothEnv) $ \(key, (leftValue, rightValue)) -> do
                        if leftValue == rightValue || key == "out"
                        then return ()
                        else echo (key <> "=" <> diffText leftValue rightValue)
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

main :: IO ()
main = do
    Options left right <- Options.Generic.unwrapRecord "Explain why two derivations differ"
    diff 0 left (Data.Set.singleton "out") right (Data.Set.singleton "out")
