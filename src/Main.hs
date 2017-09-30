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

renderOutputs :: Set Text -> Text
renderOutputs outputs =
    ":{" <> Data.Text.intercalate "," (Data.Set.toList outputs) <> "}"

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
                forM_ assocs $ \(inputName, (leftPaths, rightPaths)) -> do
                    let leftExtraPaths  = Data.Map.difference leftPaths  rightPaths
                    let rightExtraPaths = Data.Map.difference rightPaths leftPaths
                    if leftPaths == rightPaths
                    then do
                        return ()
                    else case (Data.Map.toList leftExtraPaths, Data.Map.toList rightExtraPaths) of
                        ([(leftPath', leftOutputs')], [(rightPath', rightOutputs')]) | leftOutputs' == rightOutputs' -> do
                            let [(leftPath' , leftOutputs' )] =
                                    Data.Map.toList leftExtraPaths
                            let [(rightPath', rightOutputs')] =
                                    Data.Map.toList rightExtraPaths
                            echo (explain ("The input named `" <> inputName <> "` differs"))
                            diff (indent + 2) leftPath' leftOutputs' rightPath' rightOutputs'
                        _ -> do
                            echo (explain ("The set of inputs named `" <> inputName <> "` do not match"))
                            diffWith leftExtraPaths rightExtraPaths $ \(sign, extraPaths) -> do
                                forM_ (Data.Map.toList extraPaths) $ \(extraPath, outputs) -> do
                                    echo ("    " <> sign (pathToText extraPath <> renderOutputs outputs))
  where
    echo text = Data.Text.IO.putStrLn (Data.Text.replicate indent " " <> text)

main :: IO ()
main = do
    Options left right <- Options.Generic.unwrapRecord "Explain why two derivations differ"
    diff 0 left (Data.Set.singleton "out") right (Data.Set.singleton "out")
