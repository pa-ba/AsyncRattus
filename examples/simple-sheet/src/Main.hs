{-# LANGUAGE TupleSections #-}

module Main where

import qualified Rattus.Primitives as Prim
import qualified Rattus.Stream as Stream
import Rattus.Stream (Str(..))
import Debug.Trace as D
import qualified Data.Set as Set
import MegaParser
import Expr
import qualified Data.Map as Map
import Data.Map (Map)
import Sheet hiding (O, Stream)

type O a = Prim.O Int a
type Stream a = Stream.Str Int a

processLine :: (Show a) => (String, O (Stream a)) -> String -> (String, O (Stream a))
processLine (_, later) line =
    case parseLine line of
        Just (cellId, Number i) ->
            case inputMaybe cellId i later of
                Just (x ::: xs) -> (show x, xs)
                Nothing -> ("That cell does not affect the output cells", later)
        _ -> ("Could not parse input as a valid expression.", later)

parseLine :: String -> Maybe (String, Expr)
parseLine line = do
    case words line of
        cellId : "=" : first : rest -> (cellId,) <$> parse (unwords (first : rest))
        _ -> Nothing

main :: IO ()
main = do
    {-
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
    let testCell = cell (Times (Var "A1") (Number 10)) testVarEnv
    let (cellValue Stream.::: cellValues) = input "A2" ("A2", Number 4) testCell
    let (cellValue2 Stream.::: cellValues2) = input "A1" ("A1", Number 6) cellValues
    print cellValue
    print cellValue2
-}
    --------------------------------

    let (initial ::: miniSheet2) = miniSheet
    print initial {-
    let (t ::: miniSheet3) = input "A1" 5 miniSheet2
    print t
    let (t2 ::: miniSheet4) = input "B1" 9 miniSheet3
    print t2
    let (t3 ::: miniSheet5) = input "B2" 10 miniSheet4
    print t3 -}


    interact (unlines . map fst . scanl processLine ("", miniSheet2) . lines)
