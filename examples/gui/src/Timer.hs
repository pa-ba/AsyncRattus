{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}

{-# LANGUAGE TypeOperators #-}


import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text hiding (filter, map, all)

-- Benchmark 4
everySecond :: Box (O())
everySecond = timer 1000000

everySecondSig :: Sig ()
everySecondSig = () ::: mkSig everySecond

nats :: (Int :* Int) -> Sig (Int :* Int)
nats (n :* max) = stop (box (\ (n :* max) -> n >= max)) (scan (box (\ (n :* max) _ -> (min (n + 1) max) :* max)) (n :* max) everySecondSig)


reset :: (Int :* Int) -> (Int :* Int)
reset (_ :* max) = (0 :* max)

setMax :: Int -> (Int :* Int) -> (Int :* Int)
setMax max' (n :* _) = ((min n max') :* max')

first :: (a :* b) -> a
first (x :* _) = x

second :: (a :* b) -> b
second (_ :* y) = y

window :: C VStack
window = do
    slider <- mkSlider 50 (const 1) (const 100)
    resetBtn <- mkButton (const  (pack "Reset"))

    let resSig = mkSig (btnOnClick resetBtn)
    let resetSig = mapAwait (box (\ _ -> reset)) resSig

    let currentMax = current (sldCurr slider)
    let setMaxSig = mapAwait (box setMax) (future (sldCurr slider))
 
    let inputSig = interleave (box (.)) resetSig setMaxSig

    let counterSig = switchB inputSig (box nats) (0 :* currentMax)
    
    let currentSig = map (box first) counterSig
    let maxSig = map (box second) counterSig

    label <- mkLabel (map (box display) currentSig)
    pb <- mkProgressBar (const 0) maxSig currentSig

    mkVStack (const [enabledWidget slider,
                     enabledWidget resetBtn,
                     enabledWidget label,
                     enabledWidget pb])

main :: IO ()
main = runApplication window