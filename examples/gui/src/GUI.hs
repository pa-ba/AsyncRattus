{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}

module Main (module Main) where
import Counter
import TemperatureConverter
import FlightBooker
import Timer
import Calculator
import WidgetRattus.Widgets

-- main module for specifying which benchmark should be rendered.
main :: IO ()
main = runApplication benchmark2 -- change name of benchmark to render here
