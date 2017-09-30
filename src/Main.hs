{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Attoparsec.Text (IResult(..))
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Filesystem.Path (FilePath)
import Options.Generic (Generic, ParseRecord)
import Prelude hiding (FilePath)

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

readInputs :: FilePath -> IO (Map Text (Map FilePath (Set Text)))
readInputs path = do
    let string = Filesystem.Path.CurrentOS.encodeString path
    text <- Data.Text.IO.readFile string
    derivation <- do
        case Data.Attoparsec.Text.parse Nix.Derivation.parseDerivation text of
            Done _ derivation -> return derivation
            _                 -> fail "Could not parse derivation"
    return (groupByName (Nix.Derivation.inputDrvs derivation))

innerJoin :: Ord k => Map k a -> Map k b -> Map k (a, b)
innerJoin = Data.Map.mergeWithKey both left right
  where
    both _ a b = Just (a, b)

    left _ = Data.Map.empty

    right _ = Data.Map.empty

plus :: Text -> Text
plus text = "\ESC[1;32m+ " <> text <> "\ESC[0m"

minus :: Text -> Text
minus text = "\ESC[1;31m- " <> text <> "\ESC[0m"

explain :: Text -> Text
explain text = "• " <> text

diffWith :: Monad m => a -> a -> ((Text -> Text, a) -> m ()) -> m ()
diffWith l r k = do
    k (minus, l)
    k (plus , r)

diff :: Int -> FilePath -> FilePath -> IO ()
diff indent leftPath rightPath = do
    if leftPath == rightPath
    then return ()
    else do
        diffWith leftPath rightPath $ \(sign, path) -> do
            echo (sign (pathToText path))

        if derivationName leftPath /= derivationName rightPath
        then do
            echo (explain "The derivation names do not match")

        else do
            leftInputs  <- readInputs leftPath
            rightInputs <- readInputs rightPath
    
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
                forM_ assocs $ \(inputName, (leftMap, rightMap)) -> do
                    let leftExtraMap  = Data.Map.difference leftMap  rightMap
                    let rightExtraMap = Data.Map.difference rightMap leftMap
                    if Data.Map.null leftExtraMap && Data.Map.null rightExtraMap
                    then do
                        -- Check for differences in outputs
                        return ()
                    else if not (Data.Map.size leftExtraMap == 1 && Data.Map.size rightExtraMap == 1)
                    then do
                        echo (explain ("The set of inputs named `" <> inputName <> "` do not match"))
                        diffWith leftExtraMap rightExtraMap $ \(sign, inputMap) -> do
                            forM_ (Data.Map.toList inputMap) $ \(path, outputs) -> do
                                let prettyOutputs =
                                        "{" <> Data.Text.intercalate "," (Data.Set.toList outputs) <> "}"
                                echo ("    " <> sign (pathToText path <> ":" <> prettyOutputs))
                    else do
                        -- Check for differences in outputs
                        let [leftPath' ] = Data.Map.keys leftExtraMap
                        let [rightPath'] = Data.Map.keys rightExtraMap
                        echo (explain ("The input named `" <> inputName <> "` differs"))
                        diff (indent + 2) leftPath' rightPath'
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

main :: IO ()
main = do
    Options left right <- Options.Generic.unwrapRecord "Explain why two derivations differ"
    diff 0 left right
