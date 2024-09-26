{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

module Main (module Main) where

import WidgetRattus
--import WidgetRattus.Signal
import WidgetRattus.Future
import WidgetRattus.Channels

import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter)
import System.Exit
import Data.Text.Read

{-# ANN consoleInput AllowRecursion #-}
consoleInput :: IO (Box (O Text))
consoleInput = do
         (inp :* cb) <- getInput
         let loop = do line <- getLine
                       cb line
                       loop
         forkIO loop
         return inp

regPrint :: (Producer p a, Show a) => p -> IO ()
regPrint sig = setOutput sig print

regQuit :: (Producer p a) => p -> IO ()
regQuit sig = setOutput sig (\ _ -> exitSuccess)




everySecond :: Box (O ())
everySecond = timer 1000000

everySecondSigF :: SigF ()
everySecondSigF = () :>: mkSigF' everySecond

readInt :: Text -> Maybe' Int
readInt text = case decimal text of
                 Right (x, rest) | null rest -> Just' x
                 _ -> Nothing'

nats :: Int -> SigF Int
nats init = scan (box (\ n _ -> n+1)) init everySecondSigF

main = do
  inp <- consoleInput
  let console :: F (SigF Text)
      console = mkSigF inp
      quitSig :: F (SigF Text)
      quitSig = filterAwait (box (== "quit")) console
      showSig :: F (SigF Text)
      showSig = filterAwait (box (== "show")) console

      numSig :: F (SigF Int)
      numSig = filterMapAwait (box (readInt)) console

      nats' :: SigF Int
      nats' = switchS (nats 0) (mapF (box (\ n m -> nats (current n+m))) numSig)
  
      showNat :: F (SigF Int)
      showNat = triggerAwait (box (\_ n -> n)) showSig nats'

  regQuit quitSig
  regPrint showNat
  startEventLoop
