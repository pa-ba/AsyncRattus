{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where
import WidgetRattus.Event
import WidgetRattus
import WidgetRattus.PushPull.Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

counterAndTimer :: C VStack
counterAndTimer = do
  -- Button
  counterBtn <- mkButton $ mkConstText "Increment"
  let counterEv = scan (box (\n _ -> n + 1 :: Int)) 0 $ btnOnClickEv counterBtn

  -- UI
  lbl <- mkLabel $ discr 0 counterEv
  mkConstVStack $ lbl :* counterBtn

main :: IO ()
main = runApplication counterAndTimer