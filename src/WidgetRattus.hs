{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}


-- | The bare-bones Asynchronous Rattus language. To program with streams,
-- you can use "WidgetRattus.Stream".

module WidgetRattus (
  -- * Asynchronous Rattus language primitives
  module WidgetRattus.Primitives,
  -- * Strict data types
  module WidgetRattus.Strict,
  -- * Derive class instance declarations
  module WidgetRattus.Derive,
  -- * Annotation
  WidgetRattus(..),
  -- * other
  mapO
  )
  where

import WidgetRattus.Plugin
import WidgetRattus.Strict
import WidgetRattus.Primitives
import WidgetRattus.Derive

mapO :: Box (a -> b) -> O a -> O b
mapO f later = delay (unbox f (adv later))
