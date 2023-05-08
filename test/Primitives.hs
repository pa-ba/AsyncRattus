{-# LANGUAGE TypeOperators #-}

module Main (module Main) where

import Rattus (Rattus(..))
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (delay, adv, select, box, unbox)
import qualified Rattus.InternalPrimitives as InternalPrim
import Data.Set as Set
import Test.HUnit
import System.Exit


data Value = BVal Bool | CVal Char | IVal Int | Pair Value Value
type O a = Prim.O Value a

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

allTests = testPlusOne

main :: IO ()
main = do
    counts <- runTestTT allTests
    putStrLn $ showCounts counts
    if (errors counts + failures counts) > 0
    then exitFailure
    else return ()