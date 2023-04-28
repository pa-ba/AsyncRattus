{-# LANGUAGE TupleSections #-}

module Main where

import qualified Rattus.Primitives as Prim
import qualified Rattus.Stream as Stream
import Rattus.Stream (Str(..))
import Debug.Trace as D
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Functor ((<&>))
import Sheet
import Text.Read (readMaybe)

input' :: String -> Int -> O a -> Maybe a
input' chId = if chId `elem` inputCells
              then inputMaybe chId
              else \_ _ -> Nothing

processLine :: (Show a) => (String, O (Stream a)) -> String -> (String, O (Stream a))
processLine (_, later) line =
    case parseLine line of
        Just (cellId, i) ->
            case input' cellId i later of
                Just (x ::: xs) -> (show x, xs)
                Nothing -> ("That cell does not affect the output cells", later)
        Nothing -> ("Could not parse input as a valid expression.", later)

parseLine :: String -> Maybe (String, Int)
parseLine line = do
    case words line of
        [cellId, "=", num] -> (readMaybe num :: Maybe Int) <&> (cellId,)
        _ -> Nothing

main :: IO ()
main = do
    let (initial ::: miniSheet2) = miniSheet
    print initial
    interact (unlines . map fst . scanl processLine ("", miniSheet2) . lines)
