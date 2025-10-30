{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}



module WidgetRattus.Time (
  Time(..),
  DTime,
  time,
  addTime,
  diffTime,
  (<->),
  module Data.Time.Clock
  )
  where
import WidgetRattus.Primitives

import Data.Time.Clock
import WidgetRattus.Plugin

type DTime = NominalDiffTime

{-# ANN addTime AllowLazyData #-}
addTime :: DTime -> Time -> Time
addTime diff (Time d t) = let UTCTime d' t' = addUTCTime diff (UTCTime d t) in Time d' t'

{-# ANN diffTime AllowLazyData #-}
diffTime :: Time -> Time -> DTime
diffTime (Time d1 t1) (Time d2 t2) = diffUTCTime (UTCTime d1 t1) (UTCTime d2 t2)

(<->) :: Time -> Time -> Float
t' <-> t = fromRational (toRational (diffTime t' t))