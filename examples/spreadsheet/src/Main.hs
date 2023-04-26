{-# LANGUAGE TupleSections #-}

module Main where

import Rattus.Primitives
import qualified Rattus.Stream as Stream
import Debug.Trace as D
import qualified Data.Set as Set
import MegaParser
import Expr
import qualified Data.Map as Map
import Data.Map (Map)
import Sheet

processLine :: String -> String
processLine line =
    case parseLine line of
        Just (cellId, e) -> "RESULT: " ++ show cellId ++ ": " ++ show e
        Nothing -> "Could not parse input as a valid expression."

parseLine :: String -> Maybe (String, Expr)
parseLine line = do
    case words line of
        cellId : "=" : rest -> parse (unwords rest) >>= return . (cellId,)
        _ -> Nothing

main :: IO ()
main = do
    let a = parse "a1 * 10"
    let b = parse "a1 /             10 + -10 "
    let c = parse "a1 * 60 + 10"
    let d = parse "60 * 10 - 100"
    let e = parse "60 + 10 * 100"
    print a 
    print b
    print c
    print $ a >>= \res -> evalMaybe res emptyVarEnv
    print $ b >>= \res -> evalMaybe res emptyVarEnv
    print $ c >>= \res -> evalMaybe res emptyVarEnv
    print $ d >>= \res -> evalMaybe res emptyVarEnv
    print $ e >>= \res -> evalMaybe res emptyVarEnv

    let (env Stream.::: envStr) = input "A3" ("A2", Number 5) testVarEnv
    let (env2 Stream.::: envStr2) = input "A3" ("A2", Number 3) envStr
    print env
    print env2

    interact (unlines . map processLine . lines)
   