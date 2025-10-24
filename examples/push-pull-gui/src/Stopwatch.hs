{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import WidgetRattus.Behaviour
import WidgetRattus.Event
import WidgetRattus
import WidgetRattus.PushPull.Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

elapsedTime' :: C (NominalDiffTime -> Beh NominalDiffTime)
elapsedTime' =
  do
    startTime <- time
    return (\f -> cont (box (\currentTime -> f + diffTime currentTime startTime)))

timerExample :: C VStack
timerExample = do
  -- Time
  startElapsedTime <- elapsedTime
  
  -- Buttons
  startBtn <- mkButton (mkConstText "Start")
  let startEv = btnOnClick startBtn
  stopBtn <- mkButton (mkConstText "Stop")
  let stopEv = btnOnClick stopBtn
  
  -- Start and stop events
  let startTime :: Ev (NominalDiffTime -> Beh NominalDiffTime) =
        mkEv' (box (delay (let _ = adv (unbox startEv) in elapsedTime')))
  let stopTime :: Ev (NominalDiffTime -> Beh NominalDiffTime) =
        mkEv (box (delay (let _ = adv (unbox stopEv) in const)))

  let combinedInput = interleave (box (\x _ -> x)) startTime stopTime
  let stopWatchSig = switchR (const 0) combinedInput

  -- UI
  timeLabName <- mkLabel (mkConstText "Current Time:")
  swLabName <- mkLabel (mkConstText "Elapsed Time:")

  timeLab <- mkLabel startElapsedTime
  stopWatchLab <- mkLabel stopWatchSig
  
  time <- mkConstHStack (timeLabName :* timeLab)
  sw <- mkConstHStack (swLabName :* stopWatchLab)
  buttons <- mkConstHStack (startBtn :* stopBtn)
  mkConstVStack (time :* sw :* buttons)

main :: IO ()
main = runApplication timerExample