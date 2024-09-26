{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}

module TemperatureConverter where
import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Channels
import WidgetRattus.Widgets
import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter, map, all)
import Data.Text.Read
import qualified WidgetRattus.Widgets
import GHC.TypeError (ErrorMessage(Text))

-- Benchmark 2
celsiusToFahrenheit :: Text -> Text
celsiusToFahrenheit t =
        case signed decimal t of
            Right (t', _) -> toText (t' * 9 `div` 5 + 32)
            Left _ -> "Invalid input"

fahrenheitToCelsius :: Text -> Text
fahrenheitToCelsius t =
    case signed decimal t of
        Right (t', _) -> toText ((t' - 32) * 5 `div` 9)
        Left _ -> "Invalid input"

-- Initial version of benchmark 2.
initialbenchmark2 :: C HStack
initialbenchmark2 = do
    c1 <- chan
    c2 <- chan

    let sigC = mkSig (box (wait c1))
    let sigF = mkSig (box (wait c2))

    let convertFtoC = mapAwait (box fahrenheitToCelsius) sigF
    let convertCtoF = mapAwait (box celsiusToFahrenheit) sigC

    let sigC' = "0":::interleave (box (\ x y -> x)) convertFtoC sigC
    let sigF' = "32":::interleave (box (\ x y -> x)) convertCtoF sigF

    let tfC = TextField {tfContent = sigC', tfInput = c1}
    let tfF = TextField {tfContent = sigF', tfInput = c2}

    fLabel <- mkLabel (const ("Fahrenheit" :: Text)) -- requires type annotations
    cLabel <- mkLabel (const ("Celsius" :: Text)) 

    fStack <- mkVStack (const [enabledWidget tfC, enabledWidget cLabel])
    cStack <- mkVStack (const [enabledWidget tfF, enabledWidget fLabel])

    mkHStack (const [enabledWidget fStack, enabledWidget cStack])

-- Improved version of Benchmark 2
benchmark2 :: C HStack
benchmark2 = do
    tfF1 <- mkTextField "32"
    tfC1 <- mkTextField "0"

    let convertFtoC = map (box fahrenheitToCelsius) (tfContent tfF1)
    let convertCtoF = map (box celsiusToFahrenheit) (tfContent tfC1)

    let tfF2 = addInputSigTF tfF1 (future convertCtoF)
    let tfC2 = addInputSigTF tfC1 (future convertFtoC)

    fLabel <- mkLabel (const ("Fahrenheit" :: Text))
    cLabel <- mkLabel (const ("Celsius" :: Text))

    fStack <- mkVStack (const [enabledWidget tfF2, enabledWidget fLabel])
    cStack <- mkVStack (const [enabledWidget tfC2, enabledWidget cLabel])  
    mkHStack (const [enabledWidget fStack, enabledWidget cStack])          
 

