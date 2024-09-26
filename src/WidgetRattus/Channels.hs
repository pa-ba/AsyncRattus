{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module is meant for library authors that want to build APIs
-- for interacting with asynchronous resources, e.g. a GUI framework. 

module WidgetRattus.Channels (
  getInput,
  setOutput,
  mkInput,
  startEventLoop,
  timer,
  Producer (..),
  chan,
  C (..),
  delayC,
  wait,
  Chan
) where
import WidgetRattus.InternalPrimitives

import WidgetRattus.Plugin.Annotation
import WidgetRattus.Strict
import Control.Monad
import System.IO.Unsafe
import Data.IORef
import Unsafe.Coerce
import qualified Data.HashTable.IO as H
import Data.HashTable.IO (BasicHashTable)
import qualified Data.IntSet as IntSet
import Control.Concurrent hiding (Chan)

-- | A type @p@ satisfying @Producer p a@ is essentially a signal that
-- produces values of type @a@ but it might not produce such values at
-- each tick.
class Producer p a | p -> a where
  -- | Get the current value of the producer if any.
  getCurrent :: p -> Maybe' a
  -- | Get the next state of the producer. Morally, the type of this
  -- method should be
  --
  -- > getNext :: p -> (exists q. Producer q a => O q)
  --
  -- We encode the existential type using continuation-passing style.
  getNext :: p -> (forall q. Producer q a => O q -> b) -> b

instance Producer p a => Producer (O p) a where
  getCurrent _ = Nothing'
  getNext p cb = cb p

instance Producer p a => Producer (Box p) a where
  getCurrent p = getCurrent (unbox p)
  getNext p cb = getNext (unbox p) cb

newtype C a = C {unC :: IO a} deriving (Functor, Applicative, Monad)

chan :: C (Chan a)
chan = C (Chan <$> atomicModifyIORef nextFreshChannel (\ x -> (x - 1, x)))

delayC :: O (C a) -> C (O a)
delayC d = return (delay (unsafePerformIO (unC (adv d))))

{-# ANN wait AllowRecursion #-}
wait :: Chan a -> O a
wait (Chan ch) = Delay (singletonClock ch) (lookupInp ch) 

{-# NOINLINE nextFreshChannel #-}
nextFreshChannel :: IORef InputChannelIdentifier
nextFreshChannel = unsafePerformIO (newIORef (-1))


{-# NOINLINE inputValue #-}
inputValue :: MVar (Maybe' InputValue)
inputValue = unsafePerformIO (newMVar Nothing')

{-# NOINLINE inputSem #-}
inputSem :: MVar ()
inputSem = unsafePerformIO newEmptyMVar

data OutputChannel where
  OutputChannel :: Producer p a => !(O p) -> !(a -> IO ()) -> OutputChannel


{-# NOINLINE output #-}
output :: BasicHashTable InputChannelIdentifier (List (IORef (Maybe' OutputChannel)))
output = unsafePerformIO (H.new)

{-# NOINLINE eventLoopStarted #-}
eventLoopStarted :: IORef Bool
eventLoopStarted = unsafePerformIO (newIORef False)


-- | This function can be used to implement input signals. It returns
-- a boxed delayed computation @s@ and a callback function @cb@. The
-- signal @mkSig s@ will produce a new value @v@ whenever the callback
-- function @cb@ is called with argument @v@.
getInput :: IO (Box (O a) :* (a -> IO ()))
getInput = do ch <- atomicModifyIORef nextFreshChannel (\ x -> (x - 1, x))
              return ((box (Delay (singletonClock ch) (lookupInp ch)))
                       :* \ x -> newInput ch x)


newInput :: InputChannelIdentifier -> a -> IO ()
newInput ch x = do iv <- takeMVar inputValue
                   case iv of 
                    Nothing' -> putMVar inputValue (Just' (OneInput ch x)) >> putMVar inputSem ()
                    Just' more -> putMVar inputValue (Just' (MoreInputs ch x more))

{-# ANN lookupInp AllowRecursion #-}
lookupInp :: InputChannelIdentifier -> InputValue -> a
lookupInp _ (OneInput _ v) = unsafeCoerce v
lookupInp ch (MoreInputs ch' v more) = if ch' == ch then unsafeCoerce v else lookupInp ch more

{-# ANN setOutput' AllowLazyData #-}
setOutput' :: Producer p a => (a -> IO ()) -> O p -> IO ()
setOutput' cb !sig = do
  ref <- newIORef (Just' (OutputChannel sig cb))
  let upd Nothing = (Just (ref :! Nil),())
      upd (Just ls) = (Just (ref :! ls),())
  let upd' ch Nothing = do
        forkIO (threadDelay ch >> newInput ch ())
        return (Just (ref :! Nil),())
      upd' _ (Just ls) = return (Just (ref :! ls),())
  let run pre ch =
        if ch > 0 then
          pre >> H.mutateIO output ch (upd' ch)
        else 
          pre >> H.mutate output ch upd
  IntSet.foldl' run (return ()) (extractClock sig)


-- | This function can be used to produces outputs. Given a signal @s@
-- and function @f@, the call @setOutput s f@ registers @f@ as a
-- callback function that is called with argument @v@ whenever the
-- signal produces a new value @v@. For this function to work,
-- 'startEventLoop' must be called.
setOutput :: Producer p a => p -> (a -> IO ()) -> IO ()
setOutput !sig cb = do
  case getCurrent sig of
    Just' cur' -> cb cur'
    Nothing' -> return ()
  getNext sig (setOutput' cb)

-- | This function is essentially the composition of 'getInput' and
-- 'setOutput'. It turns any producer into a signal.
mkInput :: Producer p a => p -> IO (Box (O a))
mkInput p = do (out :* cb) <- getInput
               setOutput p cb
               return out

-- | @timer n@ produces a delayed computation that ticks every @n@
-- milliseconds. In particular @mkSig (timer n)@ is a signal that
-- produces a new value every #n# milliseconds.
timer :: Int -> Box (O ())
timer d = Box (Delay (singletonClock (d `max` 10)) (\ _ -> ()))


update :: InputValue -> IORef (Maybe' OutputChannel) -> IO ()
update inp ref = do
  mout <- readIORef ref
  case mout of
    Nothing' -> return ()
    Just' (OutputChannel (Delay _ sigf) cb) -> do
      writeIORef ref Nothing'
      let new = sigf inp
      case getCurrent new of
        Just' w' -> cb w'
        Nothing' -> return ()
      getNext new (setOutput' cb)


{-# ANN getOutputsForInputs AllowRecursion #-}
{-# ANN getOutputsForInputs AllowLazyData #-}
getOutputsForInputs :: List (IORef (Maybe' OutputChannel)) -> InputValue -> IO (List (IORef (Maybe' OutputChannel)))
getOutputsForInputs acc (OneInput ch _) = do res <- H.lookup output ch
                                             case res of 
                                              Nothing -> return acc
                                              Just ls -> H.delete output ch >> return (acc `union'` ls)
getOutputsForInputs acc (MoreInputs ch _ more) = do res <- H.lookup output ch
                                                    case res of 
                                                      Nothing -> getOutputsForInputs acc more
                                                      Just ls -> H.delete output ch >> getOutputsForInputs (acc `union'` ls) more

{-# ANN eventLoop AllowRecursion #-}
{-# ANN eventLoop AllowLazyData #-}

eventLoop :: IO ()
eventLoop = do _ <- takeMVar inputSem
               minp <- takeMVar inputValue
               putMVar inputValue Nothing'
               case minp of
                 Nothing' -> error "WidgetRattus.Channels.eventLoop unexpected state"
                 Just' inp -> do
                   ls <- getOutputsForInputs Nil inp
                   mapM_ (update inp) ls
               eventLoop

-- | In order for 'setOutput' to work, this IO action must be invoked.
startEventLoop :: IO ()
startEventLoop = do
  started <- atomicModifyIORef eventLoopStarted (\b -> (True,b))
  when (not started) eventLoop