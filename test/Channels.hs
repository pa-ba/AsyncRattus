module Main (module Main) where

import Rattus (Rattus(..))
import Rattus.Channels
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (delay, adv, select, box, unbox)
import qualified Rattus.Stream as Str
import qualified Rattus.Later as Later
import Data.Set as Set
import Data.List (sort)
import Test.HUnit
import System.Exit

data Value = BVal Bool | CVal Char | IVal Int

type O a = Prim.O Value a
type Select a b = Prim.Select Value a b

(input, inputMaybe, depends, [boolCh, charCh, intCh]) = mkChannels ["boolCh", "charCh", "intCh"]


{-# ANN boolChan Rattus #-}
boolChan :: O Bool
boolChan = Later.map (box (\(BVal b) -> b)) boolCh

{-# ANN charChan Rattus #-}
charChan :: O Char
charChan = Later.map (box (\(CVal c) -> c)) charCh

{-# ANN intChan Rattus #-}
intChan :: O Int
intChan = Later.map (box (\(IVal i) -> i)) intCh

{-# ANN doSelect Rattus #-}
doSelect :: O (Select Bool Char)
doSelect = delay (select boolChan charChan)

{-# ANN plusOne Rattus #-}
plusOne :: O Int -> O Int
plusOne = Later.map (box (+1))

testPlusOne = TestCase (assertEqual "for plusOne:" 100 (input "intCh" (IVal 99) l))
    where l = plusOne intChan

testBoolChDepends = TestCase (assertEqual "boolCh depends:" ["boolCh"] (depends boolCh))
testNeverDepends = TestCase (assertEqual "never depends:" [] (depends Prim.never))
testUnionDepends = TestCase (assertEqual "select boolCh charCh:" ["boolCh", "charCh"] (sort $ depends doSelect))
dependTests = TestLabel "depend tests" $ TestList [testBoolChDepends, testNeverDepends, testUnionDepends]

testInputMaybeJust = TestCase (assertEqual "inputMaybe Just case:" (Just 5) (inputMaybe "intCh" (IVal 5) intChan))
testInputMaybeNothing = TestCase (assertEqual "inputMaybe Nothing case:" Nothing (inputMaybe "boolCh" (IVal 5) intChan))
inputMaybeTests = TestLabel "inputMaybe tests" $ TestList [testInputMaybeJust, testInputMaybeNothing]


allTests = TestList [dependTests, inputMaybeTests, testPlusOne]

main :: IO ()
main = do
    counts <- runTestTT allTests
    putStrLn $ showCounts counts
    if (errors counts + failures counts) > 0
    then exitFailure
    else return ()