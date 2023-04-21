module Main where

import Rattus.Primitives
import Debug.Trace as D
import qualified Data.Set as Set
import Text.Megaparsec
import MegaParser


main :: IO ()
main = do
    let a = parseMaybe (pExpr <* eof) "a1 * 10"
    let b = parseMaybe (pExpr <* eof) "a1 /             10 + -10 "
    let c = parseMaybe (pExpr <* eof) "a1 * 60 + 10"
    print a 
    print b
    print c
    print "hello"
   