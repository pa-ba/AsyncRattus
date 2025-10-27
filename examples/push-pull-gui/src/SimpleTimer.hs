{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}


module Main where

import WidgetRattus.Behaviour
import WidgetRattus.Event
import WidgetRattus
import WidgetRattus.PushPull.Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import Data.Text (Text)

window :: C VStack 
window = do
  resetBtn <- mkButton (const ("Reset timer" :: Text))
  now <- time
  let resetEv      :: Ev ()     = btnOnClickEv resetBtn
  let startTimeEv  :: Ev Time   = sample (box (\ _ t -> t)) resetEv timeB
  let startTime    :: Beh Time  = stepper now startTimeEv

  let timer  = zipWith (box (<->)) timeB startTime
  let txt    = mapB (box (\t -> "Current: " <> toText (floor t))) timer

  label <- mkLabel txt

  mkConstVStack (label :* resetBtn)

main :: IO() 
main = runApplication window