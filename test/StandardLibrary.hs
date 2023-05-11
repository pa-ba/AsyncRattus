{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Main (module Main) where

import Prelude hiding (Left, Right)
import AsyncRattus (AsyncRattus(..))
import AsyncRattus.Channels
import qualified AsyncRattus.Primitives as Prim
import AsyncRattus.Primitives (delay, adv, select, box, unbox, Select(..))
import qualified AsyncRattus.Stream as Stream
import qualified AsyncRattus.Later as Later
import AsyncRattus.Strict
import qualified Data.Set as Set
import Test.HUnit
import System.Exit

{-# ANN module AsyncRattus #-}

data Value = BVal !Bool | CVal !Char | IVal !Int | MIVal !(Maybe' Int)

getB :: Value -> Bool
getB (BVal b) = b
getB _ = error "not a BVal"

getC :: Value -> Char
getC (CVal c) = c
getC _ = error "not a CVal"

getI :: Value -> Int
getI (IVal i) = i
getI _ = error "not an IVal"

type O a = Prim.O Value a
type Stream a = Stream.Str Value a

(input, inputMaybe, depends, (bChan :! cChan :! iChan :! iChan2 :! iChan3 :! mIntChan :! Nil)) = mkChannels ("boolCh" :! "charCh" :! "intCh" :! "intCh2" :! "intCh3" :! "maybeIntCh" :! Nil)

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

selectManySameChan :: O (List (Int :* Bool))
selectManySameChan = Later.selectMany $ boolChan :! boolChan :! boolChan :! Nil

selectManyDiffChan :: O (List (Int :* Int))
selectManyDiffChan = Later.selectMany $ intChan :! intChan2 :! intChan3 :! Nil

twoAndThree :: O Int
twoAndThree = delay (
        case select intChan2 intChan3 of
            Left i _ -> i + 1
            Right _ i -> i + 2
            Both _ _ -> error "impossible"
    )

selectManyOverlap :: O (List (Int :* Int))
selectManyOverlap = Later.selectMany $ intChan :! intChan2 :! intChan3 :! twoAndThree :! Nil

selectManyTests = TestLabel "Later:selectMany" $ TestList [
        TestCase $ assertEqual "selectMany 3xBoolChan" ((0 :* True) :! (1 :* True) :! (2 :* True) :! Nil) (input "boolCh" (BVal True) selectManySameChan),
        TestCase $ assertEqual "selectMany 3 disjoint chans" ((1 :* 78) :! Nil) (input "intCh2" (IVal 78) selectManyDiffChan),
        TestCase $ assertEqual "selectMany 3 overlapping chans" ((1 :* 100) :! (3 :* 101) :! Nil) (input "intCh2" (IVal 100) selectManyOverlap)
    ]

laterTests = TestLabel "Later tests" $ TestList [testPlusOne, laterFromMaybeTests, laterMaybeTests, selectManyTests]

boolStrL :: O (Stream Bool)
boolStrL = Stream.mapL (box getB) $ Stream.fromLater bChan

charStrL :: O (Stream Char)
charStrL = Stream.mapL (box getC) $ Stream.fromLater cChan


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


scanTests = TestLabel "Stream:scan" $ TestList [
        TestCase (testTimesStr timesStr),
        TestCase (testTimesStr timesStr'),
        TestCase (testTimesStr' timesStrShow)
    ]

zipped :: Stream (Bool :* Char)
zipped = Stream.zip (False Stream.::: boolStrL) ('\0' Stream.::: charStrL) 

zipAwaited :: Stream (Bool :* Char)
zipAwaited = Stream.zipWithAwait (box (:*)) boolStrL charStrL False '\0'

testZipped :: Stream (Bool :* Char) -> Assertion
testZipped zipped = do
    assertEqual "initial zipped value" (False :* '\0') (Stream.hd zipped)
    let (x Stream.::: xs) = input "boolCh" (BVal True) (Stream.tl zipped)
    assertEqual "2nd zipped" (True :* '\0') x
    let (x2 Stream.::: xs2) = input "charCh" (CVal '.') xs
    assertEqual "3rd zipped" (True :* '.') x2

selfZipped :: Stream (Bool :* Bool)
selfZipped = Stream.zipWithAwait (box (:*)) boolStrL boolStrL True False

testZippedBoth :: Assertion
testZippedBoth = do
    assertEqual "initial self-zipped value" (True :* False) (Stream.hd selfZipped)
    let (x Stream.::: xs) = input "boolCh" (BVal True) (Stream.tl selfZipped)
    assertEqual "2nd self-zipped" (True :* True) x
    let (x2 Stream.::: xs2) = input "boolCh" (BVal False) xs
    assertEqual "3rd self-zipped" (False :* False) x2
    

zipTests = TestLabel "Stream:zip" $ TestList [
        TestCase $ testZipped zipped,
        TestCase $ testZipped zipAwaited,
        TestCase testZippedBoth
    ]

filtered :: Stream (Maybe' Int)
filtered = Stream.filter (box (>10)) intStr

testFiltered :: Assertion
testFiltered = do
    assertEqual "initial filtered" Nothing' (Stream.hd filtered)
    let (x Stream.::: xs) = input "intCh" (IVal 15) (Stream.tl filtered)
    assertEqual "2nd filtered" (Just' 15) x
    let (x2 Stream.::: xs2) = input "intCh" (IVal 10) xs
    assertEqual "3rd filtered" Nothing' x2

filterTests = TestLabel "filter tests" (TestCase testFiltered)

shifted :: Stream Int
shifted = Stream.shift 47 intStr

shiftedOne :: Stream Int
shiftedOne = Stream.shiftMany (47 :! Nil) intStr

testShifted :: Stream Int -> Assertion
testShifted shifted = do
    assertEqual "initial shifted value" 47 (Stream.hd shifted)
    let (x Stream.::: xs) = input "intCh" (IVal 5) (Stream.tl shifted)
    assertEqual "2nd shifted" 0 x
    let (x2 Stream.::: xs2) = input "intCh" (IVal 67) xs
    assertEqual "3rd shifted" 5 x2

shiftedTwo :: Stream Int
shiftedTwo = Stream.shiftMany (47 :! 78 :! Nil) intStr

testShiftedMany :: Assertion
testShiftedMany = do
    assertEqual "initial shifted value" 47 (Stream.hd shiftedTwo)
    let (x Stream.::: xs) = input "intCh" (IVal 5) (Stream.tl shiftedTwo)
    assertEqual "2nd shifted" 78 x
    let (x2 Stream.::: xs2) = input "intCh" (IVal 67) xs
    assertEqual "3rd shifted" 0 x2
    let (x3 Stream.::: xs3) = input "intCh" (IVal 99) xs2
    assertEqual "4th shifted" 5 x3

shiftTests = TestLabel "shift tests" $ TestList [
        TestCase (testShifted shifted),
        TestCase (testShifted shiftedOne),
        TestCase testShiftedMany
    ]

streamTests = TestLabel "Stream tests" $ TestList [strMapTests, strConstTests, scanTests, zipTests, filterTests, shiftTests]

lst :: List Int
lst = 1 :! 2 :! 3 :! 4 :! Nil

toFromListTests = TestLabel "to/from List" $ TestList [
        TestCase $ assertEqual "toList" [1, 2, 3, 4] (toList lst),
        TestCase $ assertEqual "fromList" lst (fromList [1, 2, 3, 4])
    ]

initTests = TestLabel "init' tests" $ TestCase $ assertEqual "init' test" (1 :! 2 :! 3 :! Nil) (init' lst)

reverseTests = TestLabel "reverse tests" $ TestList [
        TestCase $ assertEqual "double-reverse = original" lst (reverse' (reverse' lst)),
        TestCase $ assertEqual "direct reverse test" (fromList [4, 3, 2, 1]) (reverse' lst),
        TestCase $ assertEqual "test same as [] reverse" (reverse (toList lst)) (toList (reverse' lst))
    ]

listToMaybeTests = TestLabel "listToMaybe tests" $ TestList [
        TestCase $ assertEqual "0 elements" (Nothing' :: Maybe' Int) (listToMaybe' Nil),
        TestCase $ assertEqual "1 element" (Just' 3) (listToMaybe' (3 :! Nil)),
        TestCase $ assertEqual "2 elements" (Just' 4) (listToMaybe' (4 :! 5 :! Nil))
    ]

appendTests = TestLabel "+++ tests" $ TestList [
        TestCase $ assertEqual "same as []" (toList lst ++ toList lst) (toList (lst +++ lst)),
        TestCase $ assertEqual "0, 0 elements" (Nil :: List Int) (Nil +++ Nil),
        TestCase $ assertEqual "0, 1 elements" (7 :! Nil) (Nil +++ (7 :! Nil)),
        TestCase $ assertEqual "1, 0 elements" (8 :! Nil) ((8 :! Nil) +++ Nil),
        TestCase $ assertEqual "1, 1 elements" (10 :! 11 :! Nil) ((10 :! Nil) +++ (11 :! Nil)),
        TestCase $ assertEqual "2, 1 elements" (20 :! 21 :! 22 :! Nil) ((20 :! 21 :! Nil) +++ (22 :! Nil)),
        TestCase $ assertEqual "2, 1 elements" (30 :! 31 :! 32 :! Nil) ((30 :! Nil) +++ (31 :! 32 :! Nil)),
        TestCase $ assertEqual "2, 2 elements" (40 :! 41 :! 42 :! 43 :! Nil) ((40 :! 41 :! Nil) +++ (42 :! 43 :! Nil))
    ]

-- Given a label and a mapping function over Lists, test the mapping function.
mkMapTests :: String -> (forall a. forall b. (a -> b) -> List a -> List b) -> Test
mkMapTests label mapF = TestLabel label $ TestList [
        TestCase $ assertEqual "nil -> nil" Nil (mapF (+1) Nil),
        TestCase $ assertEqual "singleton" ("3" :! Nil) (mapF show (3 :! Nil)),
        TestCase $ assertEqual "same as []" (map (+2) $ toList lst) (toList $ mapF (+2) lst)
    ]

mapTests = TestLabel "map(Maybe) tests" $ TestList [
        mkMapTests "map' tests" map',
        mkMapTests "fmap on List tests" fmap,
        TestCase $ assertEqual "mapMaybe nil" (Nil :: List Int) (mapMaybe' Just' Nil),
        TestCase $ assertEqual "single Just" (1 :! Nil) (mapMaybe' Just' (1 :! Nil)),
        TestCase $ assertEqual "single Nothing" (Nil :: List Int) (mapMaybe' (const Nothing') ((2 :: Int) :! Nil)),
        TestCase $ assertEqual "filterMap" ("3" :! "4" :! Nil) (mapMaybe' (\x -> if x > 2 then Just' (show x) else Nothing') lst)
        
    ]

maybeTests = TestLabel "maybe tests" $ TestList [
        TestCase $ assertEqual "maybe' Nothing" "" (maybe' "" show (Nothing' :: Maybe' Int)),
        TestCase $ assertEqual "maybe' Just" "42" (maybe' "" show (Just' 42)),
        TestCase $ assertEqual "fromMaybe' Nothing" (-1) (fromMaybe' (-1) Nothing'),
        TestCase $ assertEqual "fromMaybe' Just" 43 (fromMaybe' (-1) (Just' 43))
    ]

strictTests = TestLabel "Strict tests" $ TestList [toFromListTests, initTests, reverseTests, listToMaybeTests, appendTests, mapTests, maybeTests]

allTests = TestList [laterTests, streamTests, strictTests]

{-# ANN main NotAsyncRattus #-}
main :: IO ()
main = do
    counts <- runTestTT allTests
    putStrLn $ showCounts counts
    if (errors counts + failures counts) > 0
    then exitFailure
    else return ()