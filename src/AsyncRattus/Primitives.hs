-- | The language primitives of Rattus. Note that the Rattus types
--  'delay', 'adv', and 'box' are more restrictive that the Haskell
--  types that are indicated. The more stricter Rattus typing rules
--  for these primitives are given. To ensure that your program
--  adheres to these stricter typing rules, use the plugin in
--  "Rattus.Plugin" so that GHC will check these stricter typing
--  rules.
{-# LANGUAGE TypeOperators #-}
module Rattus.Primitives
  (O
  ,Box
  ,Select(..)
  ,Clock
  ,delay
  ,adv
  ,box
  ,unbox
  ,select
  ,never
  ,Stable
  ) where
import Rattus.InternalPrimitives