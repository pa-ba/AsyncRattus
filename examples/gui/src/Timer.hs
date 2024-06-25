{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}

{-# LANGUAGE TypeOperators #-}

module Timer where
import AsyncRattus
import AsyncRattus.Signal
import AsyncRattus.Channels
import AsyncRattus.Widgets
import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter, map, all)
import qualified Data.Text as Text
import System.Exit
import Data.Text.Read

-- Benchmark 4
everySecond :: Box (O())
everySecond = timer 1000000

everySecondSig :: Sig ()
everySecondSig = () ::: mkSig everySecond

nats :: (Int :* Int) -> Sig (Int :* Int)
nats (n :* max) = stop (box (\ (n :* max) -> n >= max)) (scan (box (\ (n :* max) _ -> (min (n + 1) max) :* max)) (n :* max) everySecondSig)


reset :: (Int :* Int) -> (Int :* Int)
reset (n :* max) = (0 :* max)

setMax :: Int -> (Int :* Int) -> (Int :* Int)
setMax max' (n :* max) = ((min n max') :* max')

first :: (a :* b) -> a
first (x :* _) = x

second :: (a :* b) -> b
second (_ :* y) = y

benchmark4 :: C VStack
benchmark4 = do
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
