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

data Value = BVal !Bool | CVal !Char | IVal !Int | MIVal !(Maybe' Int)

type O a = Prim.O Value a
type Stream a = Stream.Str Value a

(input, inputMaybe, depends, [bChan, cChan, iChan, iChan2, iChan3, mIntChan]) = mkChannels ["boolCh", "charCh", "intCh", "intCh2", "intCh3", "maybeIntCh"]

{-# ANN boolChan Rattus #-}
boolChan :: O Bool
boolChan = Later.map (box (\(BVal b) -> b)) (unbox bChan)

{-# ANN charChan Rattus #-}
charChan :: O Char
charChan = Later.map (box (\(CVal c) -> c)) (unbox cChan)

{-# ANN intChan Rattus #-}
intChan :: O Int
intChan = Later.map (box (\(IVal i) -> i)) (unbox iChan)

{-# ANN intChan2 Rattus #-}
intChan2 :: O Int
intChan2 = Later.map (box (\(IVal i) -> i)) (unbox iChan2)

{-# ANN intChan3 Rattus #-}
intChan3 :: O Int
intChan3 = Later.map (box (\(IVal i) -> i)) (unbox iChan3)

{-# ANN maybeIntChan Rattus #-}
maybeIntChan :: O (Maybe' Int)
maybeIntChan = Later.map (box (\(MIVal mi) -> mi)) (unbox mIntChan)


{-# ANN plusOne Rattus #-}
plusOne :: O Int -> O Int
plusOne = Later.map (box (+1))

testPlusOne = TestCase (assertEqual "for plusOne:" 100 (input "intCh" (IVal 99) l))
    where l = plusOne intChan


{-# ANN getIntOrMinusOne Rattus #-}
getIntOrMinusOne :: O (Maybe' Int) -> O Int
getIntOrMinusOne l = Later.fromMaybe (-1) l

getIntOrMinusOneFromChan :: O Int
getIntOrMinusOneFromChan = getIntOrMinusOne maybeIntChan

laterFromMaybeTests = TestLabel "Later:fromMaybe" $ TestList [
        TestCase $ assertEqual "fromMaybe nothing:" (-1) (input "maybeIntCh" (MIVal Nothing') getIntOrMinusOneFromChan),
        TestCase $ assertEqual "fromMaybe just:" 42 (input "maybeIntCh" (MIVal (Just' 42)) getIntOrMinusOneFromChan)
    ]

{-# ANN showL Rattus #-}
showL :: Show a => O (Maybe' a) -> O (List Char)
showL = Later.maybe Nil (box (fromList . show))

{-# ANN showMaybeIntChan Rattus #-}
showMaybeIntChan :: O (List Char)
showMaybeIntChan = showL maybeIntChan

laterMaybeTests = TestLabel "Later:maybe" $ TestList [
        TestCase $ assertEqual "maybe nothing:" "" (toList $ input "maybeIntCh" (MIVal Nothing') showMaybeIntChan),
        TestCase $ assertEqual "maybe just:" "56" (toList $ input "maybeIntCh" (MIVal (Just' 56)) showMaybeIntChan)
    ]

{-# ANN selectManySameChan Rattus #-}
selectManySameChan :: O (List (Int, Bool))
selectManySameChan = Later.selectMany $ boolChan :! boolChan :! boolChan :! Nil

{-# ANN selectManyDiffChan Rattus #-}
selectManyDiffChan :: O (List (Int, Int))
selectManyDiffChan = Later.selectMany $ intChan :! intChan2 :! intChan3 :! Nil

{-# ANN twoAndThree Rattus #-}
twoAndThree :: O Int
twoAndThree = delay (
        case select intChan2 intChan3 of
            Left i _ -> i + 1
            Right _ i -> i + 2
            Both _ _ -> error "impossible"
    )

{-# ANN selectManyOverlap Rattus #-}
selectManyOverlap :: O (List (Int, Int))
selectManyOverlap = Later.selectMany $ intChan :! intChan2 :! intChan3 :! twoAndThree :! Nil

selectManyTests = TestLabel "Later:selectMany" $ TestList [
        TestCase $ assertEqual "selectMany 3xBoolChan" ((0, True) :! (1, True) :! (2, True) :! Nil) (input "boolCh" (BVal True) selectManySameChan),
        TestCase $ assertEqual "selectMany 3 disjoint chans" ((1, 78) :! Nil) (input "intCh2" (IVal 78) selectManyDiffChan),
        TestCase $ assertEqual "selectMany 3 overlapping chans" ((1, 100) :! (3, 101) :! Nil) (input "intCh2" (IVal 100) selectManyOverlap)
    ]

allTests = TestList [testPlusOne, laterFromMaybeTests, laterMaybeTests, selectManyTests]

main :: IO ()
main = do
    counts <- runTestTT allTests
    putStrLn $ showCounts counts
    if (errors counts + failures counts) > 0
    then exitFailure
    else return ()