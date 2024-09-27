{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}

import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets

import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text hiding (filter, map, all)

window :: C VStack
window = do 
    btn <- mkButton (const ("Increment" :: Text))
    let sig = btnOnClickSig btn
    let sig' = scanAwait (box (\ n _ -> n+1 :: Int)) 0 sig 
    lbl <- mkLabel sig'
    mkVStack (const [mkWidget lbl, mkWidget btn])
    

main :: IO ()
main = runApplication window