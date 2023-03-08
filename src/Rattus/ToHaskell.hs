{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Helper functions to convert streams and signal functions from
-- Rattus into Haskell.

module Rattus.ToHaskell
  (runTransducer,
   runSF,
   fromStr,
   fromStr',
   toStr,
   toStr',
   Trans(..)
  ) where

import System.IO.Unsafe
import Data.IORef
import Rattus.Primitives
import Rattus.Stream
import Rattus.Yampa
import Rattus.Strict
import qualified Data.Set as Set



-- | A state machine that takes inputs of type @a@ and produces output
-- of type @b@. In addition to the output of type @b@ the underlying
-- function also returns the new state of the state machine.
data Trans a b = Trans (a -> (b, Trans a b))

-- | Turn a stream function into a state machine.
runTransducer :: (Str a -> Str b) -> Trans a b
runTransducer tr = Trans run
  where run a = unsafePerformIO $ do
          asR <- newIORef undefined
          as <- unsafeInterleaveIO $ readIORef asR
          let b ::: bs = tr (a ::: delay as)
          return (b, Trans (run' (adv bs) asR))
        run' bs asR a = unsafePerformIO $ do
          asR' <- newIORef undefined
          as' <- unsafeInterleaveIO $ readIORef asR'
          writeIORef asR (a ::: delay as')
          let b ::: bs' = bs
          return (b, Trans (run' (adv bs') asR'))

-- | Turn a signal function into a state machine from inputs of type
-- @a@ and time (since last input) to output of type @b@.
runSF :: SF a b -> Trans (a, Double) b
runSF sf = Trans (\(a,t) -> let (s:* b) = stepSF sf t a in (b, runSF (adv s)))


-- | Turns a lazy infinite list into a stream.
toStr :: [a] -> Str a
toStr (x : xs) = x ::: delay (toStr xs)
toStr _ = error "toStr: input terminated"


toStr' :: [a] -> Str a
toStr' (x : xs) = x ::: delay' undefined (toStr' xs)


-- | Turns a stream into a lazy infinite list.
fromStr :: Str a -> [a]
fromStr (x ::: xs) = x : fromStr (adv xs)


fromStr' :: Str a -> [a]
fromStr' (x ::: xs) = x : fromStr' (adv' xs undefined)
