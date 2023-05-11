{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}


-- | The bare-bones Asynchronous Rattus language. To program with streams,
-- you can use "AsyncRattus.Stream".

module AsyncRattus (
  -- * Asynchronous Rattus language primitives
  module AsyncRattus.Primitives,
  -- * Strict data types
  module AsyncRattus.Strict,
  -- * Annotation
  AsyncRattus(..),
  -- * Applicative operators
  (|#|),
  (|##),
  (<##),
  -- * box for stable types
  box'
  )
  where

import AsyncRattus.Plugin
import AsyncRattus.Strict
import AsyncRattus.Primitives

-- all functions in this module are in Asynchronous Rattus 
{-# ANN module AsyncRattus #-}


-- | Variant of '<#>' where the argument is of a stable type..
{-# INLINE (<##) #-}
(<##) :: Stable a => O v (a -> b) -> a -> O v b
f <## x = delay (adv f x)

-- | Applicative operator for 'Box'.
{-# INLINE (|#|) #-}
(|#|) :: Box (a -> b) -> Box a -> Box b
f |#| x = box (unbox f (unbox x))

-- | Variant of '|#|' where the argument is of a stable type..
{-# INLINE (|##) #-}
(|##) :: Stable a => Box (a -> b) -> a -> Box b
f |## x = box (unbox f x)


-- | Variant of 'box' for stable types that can be safely used nested
-- in recursive definitions or in another box.
box' ::  Stable a => a -> Box a
box' x = box x
