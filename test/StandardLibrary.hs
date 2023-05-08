module Main (module Main) where

import Rattus (Rattus(..))
import Rattus.Channels
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (delay, adv, select, box, unbox)
import qualified Rattus.Stream as Str
import qualified Rattus.Later as Later
import Data.Set as Set
import Test.HUnit
import System.Exit

data Value = BVal Bool | CVal Char | IVal Int

type O a = Prim.O Value a

(input, inputMaybe, depends, [bChan, cChan, iChan]) = mkChannels ["boolCh", "charCh", "intCh"]

{-# ANN boolChan Rattus #-}
boolChan :: O Bool
boolChan = Later.map (box (\(BVal b) -> b)) bChan

{-# ANN charChan Rattus #-}
charChan :: O Char
charChan = Later.map (box (\(CVal c) -> c)) cChan

{-# ANN intChan Rattus #-}
intChan :: O Int
intChan = Later.map (box (\(IVal i) -> i)) iChan

{-# ANN plusOne Rattus #-}
plusOne :: O Int -> O Int
plusOne = Later.map (box (+1))

testPlusOne = TestCase (assertEqual "for plusOne:" 100 (input "intCh" (IVal 99) l))
    where l = plusOne intChan

allTests = testPlusOne

main :: IO ()
main = do
    counts <- runTestTT allTests
    putStrLn $ showCounts counts
    if (errors counts + failures counts) > 0
    then exitFailure
    else return ()