{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

module Main (module Main) where

import AsyncRattus
import AsyncRattus.Signal
import AsyncRattus.Channels

import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter, map)
import System.Exit
import Data.Text.Read


{-# ANN module AsyncRattus #-}


{-# ANN consoleInput NotAsyncRattus #-}
consoleInput :: IO (Box (O Text))
consoleInput = do
         (inp :* cb) <- registerInput
         let loop = do line <- getLine
                       cb line
                       loop
         forkIO loop
         return inp

{-# ANN regPrint NotAsyncRattus #-}
regPrint :: (Producer p a, Show a) => p -> IO ()
regPrint sig = registerOutput sig print

{-# ANN regQuit NotAsyncRattus #-}
regQuit :: (Producer p a) => p -> IO ()
regQuit sig = registerOutput sig (\ _ -> exitSuccess)




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
  inp <- consoleInput
  let console :: O (Sig Text)
      console = mkSig inp
  quitSig :: O (Sig Text) <- filterAwait (box (== "quit")) console
  showSig :: O (Sig Text) <- filterAwait (box (== "show")) console
  negSig :: Box (O (Sig Text)) <- filterAwait' (box (== "negate")) console
  numSig :: Box (O (Sig Int)) <- filterMapAwait' (box readInt) console

  let sig :: Box (O (Sig (Int -> Int)))
      sig = box (interleave (box (\x _ -> x))
                 (mapAwait (box (\_ n -> -n)) (unbox negSig))
                 (mapAwait (box (\m n -> m+n)) (unbox numSig)))
  
  let nats' :: Int -> Sig Int
      nats' init = switchS (nats init) (delay (\n -> nats' (current (adv (unbox sig)) n)))
  
  showNat :: O (Sig Int) <- triggerAwait (box (\_ n -> n)) showSig (nats' 0)

  regQuit quitSig
  regPrint showNat
  startEventLoop


-- main = do
--   inp <- consoleInput
--   let console :: O (Sig Text)
--       console = mkSig inp
--   quitSig :: O (Sig Text) <- filterAwait (box (== "quit")) console
--   showSig :: O (Sig Text) <- filterAwait (box (== "show")) console

--   numSig :: Box (O (Sig Int)) <- filterMapAwait' (box (readInt)) console

--   let nats' :: Int -> Sig Int
--       nats' init = switchS (nats init) (delay (\n -> nats' (n + current (adv (unbox numSig)))))
  
--   showNat :: O (Sig Int) <- triggerAwait (box (\_ n -> n)) showSig (nats' 0)

--   regQuit quitSig
--   regPrint showNat
--   startEventLoop
