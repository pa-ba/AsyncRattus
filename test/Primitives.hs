{-# LANGUAGE TypeOperators #-}

module Main (module Main) where

import Prelude hiding (Left, Right)
import Rattus (Rattus(..))
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (delay, adv, select, box, unbox, Select(..))
import qualified Rattus.InternalPrimitives as InternalPrim
import Data.Set as Set
import Test.HUnit
import System.Exit


data Value = BVal Bool | CVal Char | IVal Int | Pair Value Value
type O a = Prim.O Value a
type Select a b = Prim.Select Value a b

-- Construct channels manually to avoid dependence on Channel library
ch1 :: O Value
ch1 = InternalPrim.Delay (Set.singleton 1) snd

ch2 :: O Value
ch2 = InternalPrim.Delay (Set.singleton 2) snd

ch3 :: O Value
ch3 = InternalPrim.Delay (Set.singleton 3) snd

{-# ANN boolChan Rattus #-}
boolChan :: O Bool
boolChan = delay (let (BVal b) = adv ch1 in b)

{-# ANN charChan Rattus #-}
charChan :: O Char
charChan = delay (let (CVal c) = adv ch2 in c)

{-# ANN intChan Rattus #-}
intChan :: O Int
intChan = delay (let (IVal i) = adv ch3 in i)

{-# ANN plusOne Rattus #-}
plusOne :: O Int -> O Int
plusOne l = delay (1 + adv l)

testPlusOne = TestCase (assertEqual "for plusOne:" 42 (InternalPrim.adv' l (3, IVal 41)))
    where l = plusOne intChan

{-# ANN doSelect Rattus #-}
doSelect :: O Int
doSelect = delay (
    case select boolChan charChan of
        Left _ _ -> 1
        Right _ _ -> 2
        Both _ _ -> error "impossible"
    )

{-# ANN nestedSelect Rattus #-}
nestedSelect :: O Int
nestedSelect = delay (
    case select doSelect intChan of
        Left i _ -> i + 4
        Right _ i -> 8
        Both _ _ -> error "impossible"
    )

{-# ANN nonDisjoint Rattus #-}
nonDisjoint :: O Int
nonDisjoint = delay (
    case select doSelect nestedSelect of
        Left i _ -> i + 16
        Right _ i -> i + 32
        Both i j -> i + j + 64
    )

selectTests = TestLabel "selectTests" $ TestList [
        TestCase $ assertEqual "doSelect Left: " 1 (InternalPrim.adv' doSelect (1, BVal True)),
        TestCase $ assertEqual "doSelect Right: " 2 (InternalPrim.adv' doSelect (2, CVal 'H')),
        TestCase $ assertEqual "nestedSelect Left BoolCh: " 5 (InternalPrim.adv' nestedSelect (1, BVal True)),
        TestCase $ assertEqual "nestedSelect Left CharCh: " 6 (InternalPrim.adv' nestedSelect (2, CVal 'F')),
        TestCase $ assertEqual "nestedSelect Right: " 8 (InternalPrim.adv' nestedSelect (3, IVal 9)),
        TestCase $ assertEqual "nonDisjoint BoolCh: " 70 (InternalPrim.adv' nonDisjoint (1, BVal True)),
        TestCase $ assertEqual "nonDisjoint CharCh: " 72 (InternalPrim.adv' nonDisjoint (2, CVal 'A')),
        TestCase $ assertEqual "nonDisjoint IntCh: " 40 (InternalPrim.adv' nonDisjoint (3, IVal 42))
    ]

allTests = TestList [testPlusOne, selectTests]

main :: IO ()
main = do
    counts <- runTestTT allTests
    putStrLn $ showCounts counts
    if (errors counts + failures counts) > 0
    then exitFailure
    else return ()