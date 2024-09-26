{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}


import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text hiding (filter, map, all)
import Data.Text.Read

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

window :: C HStack
window = do
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
    mkHStack (const [fStack, cStack])          
 

main :: IO ()
main = runApplication window