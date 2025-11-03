{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import WidgetRattus.Behaviour
import WidgetRattus.Event
import WidgetRattus
import WidgetRattus.PushPull.Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

fromSec :: Int -> DTime
fromSec x = fromInteger (toInteger x)

stopTimer :: DTime -> (DTime :* DTime) -> Maybe' (DTime :* DTime)
stopTimer max (a :* _) | a >= max = Just' (max :* max)
                       | otherwise      =  Nothing'

timeFrom :: DTime -> DTime -> C (Beh (DTime :* DTime))
timeFrom d max = do
  dt <- elapsedTime
  let addTime = mapB (box (\t -> t + d :* max)) dt
  return (stopWith (box (stopTimer max)) addTime)

initialMax :: Int
initialMax = 5

timerGUI :: C VStack
timerGUI = do
  -- Slider
  maxSlider <- mkSlider initialMax (const 1) (const 100)
  let maxBeh :: Beh Int = sldCurr maxSlider
  let maxChangeEv :: Ev DTime = mapE (box fromSec) (sliderEv maxSlider)
  -- Reset button
  resetBtn <- mkButton $ mkConstText "Reset timer"
  let resetTrigger = btnOnClickEv resetBtn
  -- Input events
  let resetEv :: Ev (DTime :* DTime -> C (Beh (DTime :* DTime))) =
        mapE (box (\_ (_ :* max) -> timeFrom 0 max)) resetTrigger

  let maxEv :: Ev (DTime :* DTime -> C (Beh (DTime :* DTime))) =
        mapE (box (\newMax (cur :* _) -> timeFrom cur newMax)) maxChangeEv

  let combinedInput :: Ev (DTime :* DTime -> C (Beh (DTime :* DTime)))
       = interleave (box (\_ m -> m)) resetEv maxEv

  elapsedTime :: Beh (DTime :* DTime) <- timeFrom 0 (fromSec initialMax)
  let timer :: Beh (DTime :* DTime) = switchRC elapsedTime combinedInput
  -- Output 
  text <- mkLabel (mapB (box (\(t :* _) -> "Current: " <> toText t)) timer)
  maxText <- mkLabel (mapB (box (\max -> "Max: " <> toText max)) maxBeh)
  mkConstVStack $ maxSlider :* maxText :* text :* resetBtn

main :: IO ()
main = runApplication timerGUI