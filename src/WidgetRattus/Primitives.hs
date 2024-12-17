-- | The language primitives of Async Rattus. Note that the typing
--  rules for 'delay', 'adv','select' and 'box' are more restrictive
--  than the Haskell types that are indicated. The stricter Async
--  Rattus typing rules for these primitives are given below.

{-# LANGUAGE TypeOperators #-}
module WidgetRattus.Primitives
  (O
  ,Box
  ,Select(..)
  ,delay
  ,adv
  ,promote
  ,box
  ,unbox
  ,select
  ,never
  ,Stable
  ,Continuous
  ,chan
  ,Chan
  ,C
  ,delayC
  ,timer
  ,wait
  ) where
import WidgetRattus.InternalPrimitives
