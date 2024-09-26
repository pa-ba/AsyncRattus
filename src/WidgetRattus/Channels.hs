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

module WidgetRattus.Channels (
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
import System.IO.Unsafe
import Data.IORef
import Unsafe.Coerce

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



{-# ANN lookupInp AllowRecursion #-}
lookupInp :: InputChannelIdentifier -> InputValue -> a
lookupInp _ (OneInput _ v) = unsafeCoerce v
lookupInp ch (MoreInputs ch' v more) = if ch' == ch then unsafeCoerce v else lookupInp ch more

-- | @timer n@ produces a delayed computation that ticks every @n@
-- milliseconds. In particular @mkSig (timer n)@ is a signal that
-- produces a new value every #n# milliseconds.
timer :: Int -> Box (O ())
timer d = Box (Delay (singletonClock (d `max` 10)) (\ _ -> ()))
