{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}

module Main (module Main) where
import Counter
import TemperatureConverter
import FlightBooker
import Timer
import Calculator
import AsyncRattus.Widgets
import AsyncRattus.Channels (timer)
-- main module for specifying which benchmark should be rendered.
main :: IO ()
main = runApplication benchmark2 -- change name of benchmark to render here
