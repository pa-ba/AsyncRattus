{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calculator where
import AsyncRattus
import AsyncRattus.Signal
import AsyncRattus.Channels
import AsyncRattus.Widgets

import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter, map, all, foldl1)
import qualified AsyncRattus.Widgets
import Data.Text.Array (equal)
import Monomer.Common.Lens (HasY(y), HasX (x))
import GHC.Base (VecElem(Int16ElemRep), map)

-- Benchmark 4

reset :: Int -> Int
reset _ = 0


calc :: C VStack
calc = do
    zero <- mkButton (const ("0" ::Text))
    one <- mkButton (const ("1" ::Text))
    two <- mkButton (const ("2" ::Text))
    three <- mkButton (const ("3" ::Text))
    four <- mkButton (const ("4" ::Text))
    five <- mkButton (const ("5" ::Text))
    six <- mkButton (const ("6" ::Text))
    seven <- mkButton (const ("7" ::Text))
    eight <- mkButton (const ("8" ::Text))
    nine <- mkButton (const ("9" ::Text))

    let onclick0 = mapAwait (box (\ _ x -> x * 10)) (btnOnClickSig zero)
    let onclick1 = mapAwait (box (\ _ x -> x * 10 + 1)) (btnOnClickSig one)
    let onclick2 = mapAwait (box (\ _ x -> x * 10 + 2)) (btnOnClickSig two)
    let onclick3 = mapAwait (box (\ _ x -> x * 10 + 3)) (btnOnClickSig three)
    let onclick4 = mapAwait (box (\ _ x -> x * 10 + 4)) (btnOnClickSig four)
    let onclick5 = mapAwait (box (\ _ x -> x * 10 + 5)) (btnOnClickSig five)
    let onclick6 = mapAwait (box (\ _ x -> x * 10 + 6)) (btnOnClickSig six)
    let onclick7 = mapAwait (box (\ _ x -> x * 10 + 7)) (btnOnClickSig seven)
    let onclick8 = mapAwait (box (\ _ x -> x * 10 + 8)) (btnOnClickSig eight)
    let onclick9 = mapAwait (box (\ _ x -> x * 10 + 9)) (btnOnClickSig nine)

    addBut <- mkButton (const ("+"::Text))
    subBut <- mkButton (const ("-"::Text))
    eqBut <- mkButton (const ("="::Text))

    let resetSig =
            mapAwait (box (\ _ _ -> 0))
            (interleave (box (\ a b -> a)) (interleave (box (\ a b -> a)) (btnOnClickSig addBut) (btnOnClickSig subBut)) (btnOnClickSig eqBut))

    let sigList = [onclick0, onclick1, onclick2, onclick3, onclick4, onclick5, onclick6, onclick7, onclick8, onclick9, resetSig] :: List (O (Sig (Int->Int)))
    let combinedSig = foldl1 (interleave (box (\ a b -> a))) sigList

    let numberSig = scanAwait (box (\ a f-> f a)) 0 combinedSig
    let bufferedSig = buffer 0 numberSig

    let addSig = mapAwait (box (\ _ -> box (+))) (btnOnClickSig addBut)
    let subSig = mapAwait (box (\ _ -> box (-))) (btnOnClickSig subBut)
    let opSig = interleave (box (\ a b -> a)) addSig subSig

    let calcSig = triggerStable (box (\ op x ->box (unbox op x))) (box (0 +)) opSig bufferedSig

    let resultSig = AsyncRattus.Signal.zipWith (box (\ f x -> unbox f x)) calcSig bufferedSig
  
    let eqSig = triggerStable (box (\ _ x -> x)) 0 (btnOnClickSig eqBut) resultSig

   

    let displaySig = 0 ::: interleave (box (\ a b -> b)) (future numberSig) (future eqSig)
    

    result <- mkLabel displaySig

    operators <- mkHStack (const [enabledWidget addBut, enabledWidget subBut, enabledWidget eqBut])
    firstRow <- mkHStack (const [enabledWidget seven, enabledWidget eight, enabledWidget nine])
    secondRow <- mkHStack (const [enabledWidget four, enabledWidget five, enabledWidget six])
    thirdRow <- mkHStack (const [enabledWidget one, enabledWidget two, enabledWidget three])
    fourthRow <- mkHStack (const [enabledWidget zero])
    numbers <- mkVStack (const [enabledWidget firstRow, enabledWidget secondRow, enabledWidget thirdRow, enabledWidget fourthRow])

    input <- mkHStack (const [enabledWidget numbers, enabledWidget operators])

    mkVStack (const [enabledWidget result, enabledWidget input])