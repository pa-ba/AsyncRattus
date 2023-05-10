module Main (module Main) where

import Prelude hiding (Left, Right)
import Rattus (Rattus(..))
import Rattus.Channels
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (delay, adv, select, box, unbox, Select(..))
import qualified Rattus.Stream as Stream
import qualified Rattus.Later as Later
import Rattus.Strict
import qualified Data.Set as Set
import Test.HUnit
import System.Exit

{-# ANN module Rattus #-}

data Value = BVal !Bool | CVal !Char | IVal !Int | MIVal !(Maybe' Int)

getI :: Value -> Int
getI (IVal i) = i
getI _ = error "not an IVal"

type O a = Prim.O Value a
type Stream a = Stream.Str Value a

(input, inputMaybe, depends, [bChan, cChan, iChan, iChan2, iChan3, mIntChan]) = mkChannels ["boolCh", "charCh", "intCh", "intCh2", "intCh3", "maybeIntCh"]

boolChan :: O Bool
boolChan = Later.map (box (\(BVal b) -> b)) (unbox bChan)

charChan :: O Char
charChan = Later.map (box (\(CVal c) -> c)) (unbox cChan)

intChan :: O Int
intChan = Later.map (box (\(IVal i) -> i)) (unbox iChan)

intChan2 :: O Int
intChan2 = Later.map (box (\(IVal i) -> i)) (unbox iChan2)

intChan3 :: O Int
intChan3 = Later.map (box (\(IVal i) -> i)) (unbox iChan3)

maybeIntChan :: O (Maybe' Int)
maybeIntChan = Later.map (box (\(MIVal mi) -> mi)) (unbox mIntChan)

plusOne :: O Int -> O Int
plusOne = Later.map (box (+1))

testPlusOne = TestCase (assertEqual "for plusOne:" 100 (input "intCh" (IVal 99) l))
    where l = plusOne intChan


getIntOrMinusOne :: O (Maybe' Int) -> O Int
getIntOrMinusOne l = Later.fromMaybe (-1) l

getIntOrMinusOneFromChan :: O Int
getIntOrMinusOneFromChan = getIntOrMinusOne maybeIntChan

laterFromMaybeTests = TestLabel "Later:fromMaybe" $ TestList [
        TestCase $ assertEqual "fromMaybe nothing:" (-1) (input "maybeIntCh" (MIVal Nothing') getIntOrMinusOneFromChan),
        TestCase $ assertEqual "fromMaybe just:" 42 (input "maybeIntCh" (MIVal (Just' 42)) getIntOrMinusOneFromChan)
    ]

showL :: Show a => O (Maybe' a) -> O (List Char)
showL = Later.maybe Nil (box (fromList . show))

showMaybeIntChan :: O (List Char)
showMaybeIntChan = showL maybeIntChan

laterMaybeTests = TestLabel "Later:maybe" $ TestList [
        TestCase $ assertEqual "maybe nothing:" "" (toList $ input "maybeIntCh" (MIVal Nothing') showMaybeIntChan),
        TestCase $ assertEqual "maybe just:" "56" (toList $ input "maybeIntCh" (MIVal (Just' 56)) showMaybeIntChan)
    ]

selectManySameChan :: O (List (Int, Bool))
selectManySameChan = Later.selectMany $ boolChan :! boolChan :! boolChan :! Nil

selectManyDiffChan :: O (List (Int, Int))
selectManyDiffChan = Later.selectMany $ intChan :! intChan2 :! intChan3 :! Nil

twoAndThree :: O Int
twoAndThree = delay (
        case select intChan2 intChan3 of
            Left i _ -> i + 1
            Right _ i -> i + 2
            Both _ _ -> error "impossible"
    )

selectManyOverlap :: O (List (Int, Int))
selectManyOverlap = Later.selectMany $ intChan :! intChan2 :! intChan3 :! twoAndThree :! Nil

selectManyTests = TestLabel "Later:selectMany" $ TestList [
        TestCase $ assertEqual "selectMany 3xBoolChan" ((0, True) :! (1, True) :! (2, True) :! Nil) (input "boolCh" (BVal True) selectManySameChan),
        TestCase $ assertEqual "selectMany 3 disjoint chans" ((1, 78) :! Nil) (input "intCh2" (IVal 78) selectManyDiffChan),
        TestCase $ assertEqual "selectMany 3 overlapping chans" ((1, 100) :! (3, 101) :! Nil) (input "intCh2" (IVal 100) selectManyOverlap)
    ]

laterTests = TestLabel "Later tests" $ TestList [testPlusOne, laterFromMaybeTests, laterMaybeTests, selectManyTests]

intStrL :: O (Stream Int)
intStrL = Stream.mapL (box getI) $ Stream.fromLater iChan

intStr :: Stream Int
intStr = 0 Stream.::: intStrL

intStr' :: Stream Int
intStr' = 1 Stream.::: intStrL

intStrPlusOne :: Stream Int
intStrPlusOne = Stream.map (box (+1)) intStr

strMapTests = TestLabel "Stream:map(L)" $ TestList [
        TestCase $ assertEqual "stream map head" 1 (Stream.hd intStrPlusOne),
        TestCase $ assertEqual "stream map next" 48 (Stream.hd (input "intCh" (IVal 47) (Stream.tl intStrPlusOne)))
    ]

strConstTests = TestLabel "Stream:const(Box)" $ TestList [
        TestCase $ assertEqual "const 47 hd" 47 (Stream.hd (Stream.const (47 :: Int))),
        TestCase $ assertEqual "const 47 never" Set.empty (depends (Stream.tl (Stream.const (47 :: Int))))
    ]

timesStr :: Stream Int
timesStr = Stream.scan (box (*)) 1 intStr'

timesStr' :: Stream Int
timesStr' = Stream.scanAwait (box (*)) 1 intStrL

testTimesStr :: Stream Int -> Assertion
testTimesStr tStr = do
    let (x Stream.::: xs) = input "intCh" (IVal 5) (Stream.tl tStr)
    assertEqual "timesStr 2nd elem" 5 x
    let (x2 Stream.::: xs2) = input "intCh" (IVal 12) xs
    assertEqual "timesStr 3rd elem" 60 x2

timesStrShow :: Stream (List Char)
timesStrShow = Stream.scanMap (box (*)) (box (fromList . show)) 1 intStr'

testTimesStr' :: Stream (List Char) -> Assertion
testTimesStr' tStr = do
    let (x Stream.::: xs) = input "intCh" (IVal 5) (Stream.tl tStr)
    assertEqual "timesStr 2nd elem" ('5' :! Nil) x
    let (x2 Stream.::: xs2) = input "intCh" (IVal 12) xs
    assertEqual "timesStr 3rd elem" ('6' :! '0' :! Nil) x2


scanTests = TestLabel "Stream:scanAwait" $ TestList [
        TestCase (testTimesStr timesStr),
        TestCase (testTimesStr timesStr'),
        TestCase (testTimesStr' timesStrShow)
    ]

streamTests = TestLabel "Stream tests" $ TestList [strMapTests, strConstTests, scanTests]

allTests = TestList [laterTests, streamTests]

main :: IO ()
main = do
    counts <- runTestTT allTests
    putStrLn $ showCounts counts
    if (errors counts + failures counts) > 0
    then exitFailure
    else return ()