{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
module Main (module Main) where

import AsyncRattus
import AsyncRattus.Signal
import AsyncRattus.Channels

import Control.Concurrent ( forkIO )
import Data.Char
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, Left, Right)
import Prelude hiding (map, const, zipWith, zip, filter, Left, Right)
import Control.DeepSeq ( force )

{-# ANN module AsyncRattus #-}

-- intInput  :: IO (Box (O Int))
-- intInput = do
--          (inp, cb) <- registerInput
--          let loop = do line <- getLine
--                        when (all isDigit line) $ cb (read line)
--                        loop
--          forkIO loop
--          return inp

-- intOutput  :: O (Sig Int) -> IO ()
-- intOutput sig = registerOutput sig print



-- {-# ANN main AsyncRattus #-}
-- main = do ints <- intInput

--           let intSig :: O (Sig Int)
--               intSig = mkSignal ints
--               newSig :: O (Sig Int)
--               newSig = mapAwait (box (+1)) intSig

--           intOutput newSig
--           startEventLoop

everySecond :: Box (O ())
everySecond = timer 1000000

everySecondSig :: Sig ()
everySecondSig = ()::: mkSignal everySecond


{-# ANN numchar NotAsyncRattus #-}
numchar :: IO (Box (O Int) :* Box (O Char))
numchar = do
         putStrLn "register num and char input"
         (intInp :* intCb) <- registerInput
         (charInp :* charCb) <- registerInput
         let loop = do ch <- getChar
                       if isNumber ch then intCb (digitToInt ch)
                         else if ch == '\n' then return ()
                         else charCb ch
                       loop
         forkIO loop
         return (intInp :* charInp)
{-# ANN registerConsoleOutput NotAsyncRattus #-}
registerConsoleOutput :: (Producer p a, Show a) => String -> p -> IO ()
registerConsoleOutput str sig = registerOutput sig (\ x -> putStrLn (str ++ ": " ++ show x))



main = do
  (num :* char) <- numchar

  let numSig :: Box (O (Sig Int))
      numSig = box (mkSignal num)

      charSig :: Box (O (Sig Char))
      charSig = box (mkSignal char)

      sigBoth :: O (Sig (Char :* Int))
      sigBoth = future (zipWithAwait (box (:*)) (unbox charSig) (unbox numSig) 'a' 0)

      sigEither :: O (Sig Char)
      sigEither = interleave (box (\ x _ -> x)) (unbox charSig) (mapAwait (box intToDigit) (unbox numSig))

      nats :: Sig Int
      nats = scan (box (\ n _ -> n+1)) 0 everySecondSig

      nats' :: Int -> Int -> Sig Int
      nats' d init = switchS (scan (box (\ n _ -> n + d)) init everySecondSig) (delay (adv (unbox char) `seq` nats' (-d)))

      beh :: O (Sig Int)
      beh = switchAwait (unbox numSig) (mapO (box (\ _ -> 0 ::: mapAwait (box negate) (unbox numSig))) (unbox charSig))

  registerConsoleOutput (force "charSig") (unbox charSig)
  registerConsoleOutput (force "numSig") (unbox numSig)
  registerConsoleOutput (force "sigEither") sigEither
  registerConsoleOutput (force "sigBoth") sigBoth
  registerConsoleOutput (force "beh") beh
  registerConsoleOutput (force "nats") (nats' 1 0)
  startEventLoop
