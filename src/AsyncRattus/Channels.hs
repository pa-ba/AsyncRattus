{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module AsyncRattus.Channels (
  registerInput,
  registerOutput,
  mkInput,
  startEventLoop,
  timer,
  Producer (..),
  Str (..)
) where

import AsyncRattus.InternalPrimitives

import AsyncRattus.Plugin.Annotation
import AsyncRattus.Strict
import Control.Concurrent.MVar
import Control.Monad
import System.IO.Unsafe
import Data.IORef
import Unsafe.Coerce
import qualified Data.HashTable.IO as H
import Data.HashTable.IO (BasicHashTable)
import qualified Data.IntSet as IntSet
import Control.Concurrent


-- | @Str a@ is a stream of values of type @a@.
data Str a = !a ::: !(O (Str a))


class Producer p where
  type Output p
  mkStr :: p -> Str (Maybe' (Output p))

{-# ANN module AsyncRattus #-}
{-# ANN module AllowLazyData #-}

instance Producer p => Producer (O p) where
  type Output (O p) = Output p
  mkStr p = Nothing' ::: delay (mkStr (adv p))


{-# NOINLINE nextFreshChannel #-}
nextFreshChannel :: IORef InputChannelIdentifier
nextFreshChannel = unsafePerformIO (newIORef (-1))


{-# NOINLINE input #-}
input :: MVar InputValue
input = unsafePerformIO newEmptyMVar

data OutputChannel where
  OutputChannel :: !(O (Str (Maybe' a))) -> !(a -> IO ()) -> OutputChannel


{-# NOINLINE output #-}
output :: BasicHashTable InputChannelIdentifier (List (IORef (Maybe' OutputChannel)))
output = unsafePerformIO (H.new)

{-# NOINLINE eventLoopStarted #-}
eventLoopStarted :: IORef Bool
eventLoopStarted = unsafePerformIO (newIORef False)



registerInput :: IO (Box (O a) :* (a -> IO ()))
registerInput = do ch <- atomicModifyIORef nextFreshChannel (\ x -> (x - 1, x))
                   return ((box (Delay (singletonClock ch) (\ (InputValue _ v) -> unsafeCoerce v)))
                          :* \ x -> putMVar input (InputValue ch x))

registerOutput' :: O (Str (Maybe' a)) -> (a -> IO ()) -> IO ()
registerOutput' !sig cb = do
  ref <- newIORef (Just' (OutputChannel sig cb))
  let upd Nothing = (Just (ref :! Nil),())
      upd (Just ls) = (Just (ref :! ls),())
  let upd' ch Nothing = do
        forkIO (threadDelay ch >> putMVar input (InputValue ch ()))
        return (Just (ref :! Nil),())
      upd' _ (Just ls) = return (Just (ref :! ls),())
  let run pre ch =
        if ch > 0 then
          pre >> H.mutateIO output ch (upd' ch)
        else 
          pre >> H.mutate output ch upd
  IntSet.foldl' run (return ()) (extractClock sig)

registerOutput :: Producer p => p -> (Output p -> IO ()) -> IO ()
registerOutput !sig cb = do
  let cur ::: sig' = mkStr sig
  case cur of
    Just' cur' -> cb cur'
    Nothing' -> return ()
  registerOutput' sig' cb

mkInput :: Producer p => p -> IO (Box (O (Output p)))
mkInput p = do (out :* cb) <-registerInput
               registerOutput p cb
               return out

timer :: Int -> Box (O ())
timer d = Box (Delay (singletonClock (d `max` 10)) (\ _ -> ()))


update :: InputValue -> IORef (Maybe' OutputChannel) -> IO ()
update inp ref = do
  mout <- readIORef ref
  case mout of
    Nothing' -> return ()
    Just' (OutputChannel (Delay _ sigf) cb) -> do
      writeIORef ref Nothing'
      let w ::: d = sigf inp
      case w of
        Just' w' -> cb w'
        Nothing' -> return ()
      registerOutput' d cb


{-# ANN eventLoop NotAsyncRattus #-}

eventLoop :: IO ()
eventLoop = do inp@(InputValue ch _) <- takeMVar input
               res <- H.lookup output ch
               case res of
                 Nothing -> return ()
                 Just ls -> do
                   H.delete output ch
                   mapM_ (update inp) ls
               eventLoop

               

startEventLoop :: IO ()
startEventLoop = do
  started <- atomicModifyIORef eventLoopStarted (\b -> (True,b))
  when (not started) eventLoop
