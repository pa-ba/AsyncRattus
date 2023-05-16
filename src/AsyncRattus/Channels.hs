{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module AsyncRattus.Channels (
  registerInput,
  registerOutput,
  startEventLoop
) where

import AsyncRattus.InternalPrimitives

import AsyncRattus.Stream
import Control.Concurrent.MVar
import Control.Monad
import System.IO.Unsafe
import Data.IORef
import Unsafe.Coerce
import qualified Data.HashTable.IO as H
import Data.HashTable.IO (BasicHashTable)
import qualified Data.IntSet as IntSet

{-# NOINLINE nextFreshChannel #-}
nextFreshChannel :: IORef InputChannelIdentifier
nextFreshChannel = unsafePerformIO (newIORef (-1))


{-# NOINLINE input #-}
input :: MVar InputValue
input = unsafePerformIO newEmptyMVar

data OutputChannel where
  OutputChannel :: O (Str a) -> (a -> IO ()) -> OutputChannel


{-# NOINLINE output #-}
output :: BasicHashTable InputChannelIdentifier [IORef (Maybe OutputChannel)]
output = unsafePerformIO (H.new)

{-# NOINLINE eventLoopStarted #-}
eventLoopStarted :: IORef Bool
eventLoopStarted = unsafePerformIO (newIORef False)



registerInput :: IO (Box (O a), (a -> IO ()))
registerInput = do ch <- atomicModifyIORef nextFreshChannel (\ x -> (x - 1, x))
                   return ((box (Delay (singletonClock ch) (\ (InputValue _ v) -> unsafeCoerce v)))
                          , \ x -> putMVar input (InputValue ch x))

registerOutput :: O (Str a) -> (a -> IO ()) -> IO ()
registerOutput !sig cb = do
  ref <- newIORef (Just (OutputChannel sig cb))
  let upd Nothing = (Just [ref],())
      upd (Just ls) = (Just (ref : ls),())
  let run pre ch = pre >> H.mutate output ch upd
  IntSet.foldl' run (return ()) (extractClock sig)
        

-- timer :: Int -> Box (O ())
-- timer d = Box (Delay (singletonClock (abs d)) (\ _ -> ()))


update :: InputValue -> IORef (Maybe OutputChannel) -> IO ()
update inp ref = do
  mout <- readIORef ref
  case mout of
    Nothing -> return ()
    Just (OutputChannel (Delay _ sigf) cb) -> do
      writeIORef ref Nothing
      let w ::: d = sigf inp
      cb w
      registerOutput d cb



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
