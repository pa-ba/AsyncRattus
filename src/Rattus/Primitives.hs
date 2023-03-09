-- | The language primitives of Rattus. Note that the Rattus types
--  'delay', 'adv', and 'box' are more restrictive that the Haskell
--  types that are indicated. The more stricter Rattus typing rules
--  for these primitives are given. To ensure that your program
--  adheres to these stricter typing rules, use the plugin in
--  "Rattus.Plugin" so that GHC will check these stricter typing
--  rules.
{-# LANGUAGE TypeOperators #-}
module Rattus.Primitives where
{-module Rattus.Primitives
  (O
  ,Box
  ,Value
  ,delay
  ,delay'
  ,delayCustom
  ,extractF
  ,extractClock
  ,adv
  ,adv'
  ,box
  ,unbox
  ,Stable
  ) where -}
import Data.Set (Set)



-- | A type is @Stable@ if it is a strict type and the later modality
-- @O@ and function types only occur under @Box@.
--
-- For example, these types are stable: @Int@, @Box (a -> b)@, @Box (O
-- Int)@, @Box (Str a -> Str b)@.
--
-- But these types are not stable: @[Int]@ (because the list type is
-- not strict), @Int -> Int@, (function type is not stable), @O
-- Int@, @Str Int@.

class  Stable a  where

data Select a b = Left !a !(O b) | Right !(O a) !b | Both !a !b

-- An input channel is identified by an integer. The programmer should not know about it.
type InputChannelIdentifier = Int

type Clock = Set InputChannelIdentifier

-- A value that arrives on an input channel
data Value = IntValue Int | CharValue Char | BoolValue Bool 

type InputValue = (InputChannelIdentifier, Value)

-- | The "later" type modality. A value of type @O a@ is a computation
-- that produces a value of type @a@ in the next time step. Use
-- 'delay' and 'adv' to construct and consume 'O'-types.
data O a = Delay Clock (InputValue -> a)

-- | The "stable" type modality. A value of type @Box a@ is a
-- time-independent computation that produces a value of type @a@.
-- Use 'box' and 'unbox' to construct and consume 'Box'-types.
data Box a = Box a

-- | This is the constructor for the "later" modality 'O':
--
-- >     Γ ✓ ⊢ t :: 𝜏
-- > --------------------
-- >  Γ ⊢ delay t :: O 𝜏
--
{-# INLINE [1] delay #-}
delay :: a -> O a
delay x = Delay undefined (const x)


delay' :: Clock -> a -> O a
delay' cl a = Delay cl (const a)

extractClock :: O a -> Clock
extractClock (Delay cl f) = cl

-- | This is the eliminator for the "later" modality 'O':
--
-- >     Γ ⊢ t :: O 𝜏
-- > ---------------------
-- >  Γ ✓ Γ' ⊢ adv t :: 𝜏
--
{-# INLINE [1] adv #-}
adv :: O a -> a
adv (Delay cl f) = undefined

adv' :: O a -> InputValue -> a
adv' (Delay cl f) inputValue = f inputValue

-- | This is the constructor for the "stable" modality 'Box':
--
-- >     Γ☐ ⊢ t :: 𝜏
-- > --------------------
-- >  Γ ⊢ box t :: Box 𝜏
--
-- where Γ☐ is obtained from Γ by removing ✓ and any variables @x ::
-- 𝜏@, where 𝜏 is not a stable type.

{-# INLINE [1] box #-}
box :: a -> Box a
box x = Box x


-- | This is the eliminator for the "stable" modality  'Box':
--
-- >   Γ ⊢ t :: Box 𝜏
-- > ------------------
-- >  Γ ⊢ unbox t :: 𝜏
{-# INLINE [1] unbox #-}
unbox :: Box a -> a
unbox (Box d) = d


{-# RULES
  "unbox/box"    forall x. unbox (box x) = x
    #-}


{-# RULES
  "box/unbox"    forall x. box (unbox x) = x
    #-}

                
{-# RULES
  "adv/delay"    forall x. adv (delay x) = x
    #-}
                
{-# RULES
  "delay/adv"    forall x. delay (adv x) = x
    #-}
