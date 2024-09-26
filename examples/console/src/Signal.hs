{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

module Main (module Main) where

import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Channels

import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter, map)
import System.Exit
import Data.Text.Read

{-# ANN consoleInput AllowRecursion #-}

consoleInput :: IO (Box (O (Sig Text)))
consoleInput = do
         (inp :* cb) <- getInputSig
         let loop = do line <- getLine
                       cb line
                       loop
         forkIO loop
         return inp

setPrint :: (Producer p a, Show a) => p -> IO ()
setPrint sig = setOutput sig print

setQuit :: (Producer p a) => p -> IO ()
setQuit sig = setOutput sig (\ _ -> exitSuccess)




everySecond :: Box (O ())
everySecond = timer 1000000

everySecondSig :: Sig ()
everySecondSig = () ::: mkSig everySecond

readInt :: Text -> Maybe' Int
readInt text = case decimal text of
                 Right (x, rest) | null rest -> Just' x
                 _ -> Nothing'

nats :: Int -> Sig Int
nats init = scan (box (\ n _ -> n+1)) init everySecondSig

main = do
  console :: O (Sig Text) <- unbox <$> consoleInput
  quitSig :: O (Sig Text) <- unbox <$> filterAwait (box (== "quit")) console
  showSig :: O (Sig Text) <- unbox <$> filterAwait (box (== "show")) console
  negSig :: Box (O (Sig Text)) <- filterAwait (box (== "negate")) console
  numSig :: Box (O (Sig Int)) <- filterMapAwait (box readInt) console

  let sig :: Box (O (Sig (Int -> Int)))
      sig = box (interleave (box (.))
                 (mapAwait (box (\_ n -> -n)) (unbox negSig))
                 (mapAwait (box (\m n -> m+n)) (unbox numSig)))
  
  let nats' :: Int -> Sig Int
      nats' init = switchS (nats init) (delay (\n -> nats' (current (adv (unbox sig)) n)))
  
  showNat :: Box (O (Sig Int)) <- triggerAwait (box (\_ n -> n)) showSig (nats' 0)

  setQuit quitSig
  setPrint showNat
  startEventLoop
