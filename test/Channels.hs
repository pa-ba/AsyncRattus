module Main (module Main) where

import Rattus (Rattus(..))
import Rattus.Channels
import Rattus.Strict
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (delay, adv, select, box, unbox)
import qualified Rattus.Stream as Stream
import qualified Rattus.Later as Later
import qualified Data.Set as Set
import Test.HUnit
import System.Exit

{-# ANN module Rattus #-}

data Value = BVal !Bool | CVal !Char | IVal !Int

type O a = Prim.O Value a
type Select a b = Prim.Select Value a b
type Stream a = Stream.Str Value a

(input, inputMaybe, depends, (boolCh :! charCh :! intCh :! Nil)) = mkChannels ("boolCh" :! "charCh" :! "intCh" :! Nil)

boolChan :: O Bool
boolChan = Later.map (box (\(BVal b) -> b)) (unbox boolCh)

charChan :: O Char
charChan = Later.map (box (\(CVal c) -> c)) (unbox charCh)

intChan :: O Int
intChan = Later.map (box (\(IVal i) -> i)) (unbox intCh)

doSelect :: O (Select Bool Char)
doSelect = delay (select boolChan charChan)

nestedSelect :: O (Select (Select Bool Char) Int)
nestedSelect = delay (select doSelect intChan)


testBoolChDepends = TestCase (assertEqual "boolCh depends:" (Set.singleton "boolCh") (depends (unbox boolCh)))
testNeverDepends = TestCase (assertEqual "never depends:" Set.empty (depends Prim.never))
testUnionDepends = TestCase (assertEqual "select boolCh charCh:" (Set.fromList ["boolCh", "charCh"]) (depends doSelect))
testNestedUnionDepends = TestCase (assertEqual "double select:" (Set.fromList ["boolCh", "charCh", "intCh"]) (depends nestedSelect))
dependTests = TestLabel "depend tests" $ TestList [testBoolChDepends, testNeverDepends, testUnionDepends, testNestedUnionDepends]

testInputMaybeJust = TestCase (assertEqual "inputMaybe Just case:" (Just 5) (inputMaybe "intCh" (IVal 5) intChan))
testInputMaybeNothing = TestCase (assertEqual "inputMaybe Nothing case:" Nothing (inputMaybe "boolCh" (IVal 5) intChan))
inputMaybeTests = TestLabel "inputMaybe tests" $ TestList [testInputMaybeJust, testInputMaybeNothing]


allTests = TestList [dependTests, inputMaybeTests]

main :: IO ()
main = do
    counts <- runTestTT allTests
    putStrLn $ showCounts counts
    if (errors counts + failures counts) > 0
    then exitFailure
    else return ()