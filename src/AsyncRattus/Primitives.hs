-- | The language primitives of Asynchronous Rattus. Note that the Asynchronous Rattus types
--  'delay', 'adv','select' and 'box' are more restrictive that the Haskell
--  types that are indicated. The stricter Asynchronous Rattus typing rules
--  for these primitives are given. To ensure that your program
--  adheres to these stricter typing rules, use the plugin in
--  "AsyncRattus.Plugin" so that GHC will check these stricter typing
--  rules.
{-# LANGUAGE TypeOperators #-}
module AsyncRattus.Primitives
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
import AsyncRattus.InternalPrimitives