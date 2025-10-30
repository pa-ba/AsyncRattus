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

toSec :: DTime -> Int
toSec x = floor $ toRational x

fromSec :: Int -> DTime
fromSec x = fromInteger (toInteger x)

stopTimer :: Int -> (DTime :* Int) -> Maybe' (DTime :* Int)
stopTimer max (a :* _) | toSec a >= max = Just' (fromSec max :* max)
                       | otherwise      =  Nothing'

timeFrom :: DTime -> Int -> C (Beh (DTime :* Int))
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
  let maxBeh = sldCurr maxSlider
  let maxChangeEv = sliderOnChange maxSlider
  -- Reset button
  resetBtn <- mkButton $ mkConstText "Reset timer"
  let resetTrigger = btnOnClickEv resetBtn
  -- Input events
  let resetEv :: Ev (DTime :* Int -> C (Beh (DTime :* Int))) =
        mapE (box (\_ (_ :* max) -> timeFrom 0 max)) resetTrigger

  let maxEv :: Ev (DTime :* Int -> C (Beh (DTime :* Int))) =
        mapE (box (\newMax (cur :* _) -> timeFrom cur newMax)) maxChangeEv

  let combinedInput :: Ev (DTime :* Int -> C (Beh (DTime :* Int)))
       = interleave (box (\_ m -> m)) resetEv maxEv

  elapsedTime :: Beh (DTime :* Int) <- timeFrom 0 initialMax
  let timer :: Beh (DTime :* Int) = switchRC elapsedTime combinedInput
  -- Output 
  text <- mkLabel (mapB (box (\(t :* _) -> "Current: " <> toText (toSec t))) timer)
  maxText <- mkLabel (mapB (box (\max -> "Max: " <> toText max)) maxBeh)
  mkConstVStack $ maxSlider :* maxText :* text :* resetBtn

main :: IO ()
main = runApplication timerGUI