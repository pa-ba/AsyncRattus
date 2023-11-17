{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}


-- | The bare-bones Asynchronous Rattus language. To program with streams,
-- you can use "AsyncRattus.Stream".

module AsyncRattus (
  -- * Asynchronous Rattus language primitives
  module AsyncRattus.Primitives,
  -- * Strict data types
  module AsyncRattus.Strict,
  -- * Derive class instance declarations
  module AsyncRattus.Derive,
  -- * Annotation
  AsyncRattus(..),
  -- * other
  mapO
  )
  where

import AsyncRattus.Plugin
import AsyncRattus.Strict
import AsyncRattus.Primitives
import AsyncRattus.Derive

mapO :: Box (a -> b) -> O a -> O b
mapO f later = delay (unbox f (adv later))
