module Rattus.InternalPrimitives where

import Prelude hiding (Left, Right)
import Data.Set (Set, empty)

-- An input channel is identified by an integer. The programmer should not know about it.
type InputChannelIdentifier = Int

type Clock = Set InputChannelIdentifier

type InputValue a = (InputChannelIdentifier, a)

-- | The "later" type modality. A value of type @O a@ is a computation
-- that produces a value of type @a@ in the next time step. Use
-- 'delay' and 'adv' to construct and consume 'O'-types.
data O v a = Delay Clock (InputValue v -> a)

data Select v a b = Left !a !(O v b) | Right !(O v a) !b | Both !a !b

rattusError = error "Did you forget to mark this as Async Rattus code?"

-- | This is the constructor for the "later" modality 'O':
--
-- >     Γ ✓ ⊢ t :: 𝜏
-- > --------------------
-- >  Γ ⊢ delay t :: O 𝜏
--
{-# INLINE [1] delay #-}
delay :: a -> O v a
delay x = Delay rattusError (const x)

delay' :: Clock -> a -> O v a
delay' cl a = Delay cl (const a)

extractClock :: O v a -> Clock
extractClock (Delay cl _) = cl

adv' :: O v a -> InputValue v -> a
adv' (Delay cl f) inpVal@(chId, _) | chId `elem` cl = f inpVal
adv' (Delay cl _) (chId, _) = error $ "Rattus internal error: inpVal chId " ++ show chId ++ " not in clock for delay: " ++ show cl


-- | This is the eliminator for the "later" modality 'O':
--
-- >     Γ ⊢ t :: O 𝜏
-- > ---------------------
-- >  Γ ✓ Γ' ⊢ adv t :: 𝜏
--
{-# INLINE [1] adv #-}
adv :: O v a -> a
adv (Delay _ _) = rattusError


select :: O v a -> O v b -> Select v a b
select a b = select' a b rattusError

select' :: O v a -> O v b -> InputValue v -> Select v a b
select' a@(Delay clA inpFA) b@(Delay clB inpFB) inputValue@(chId, _)
  | chId `elem` clA && chId `elem` clB = Both (inpFA inputValue) (inpFB inputValue)
  | chId `elem` clA = Left (inpFA inputValue) b
  | chId `elem` clB = Right a (inpFB inputValue)
  | otherwise = error "Tick did not come on correct input channels"


never :: O v a
never = Delay empty (error "Trying to adv on the 'never' delayed computation")

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



-- | The "stable" type modality. A value of type @Box a@ is a
-- time-independent computation that produces a value of type @a@.
-- Use 'box' and 'unbox' to construct and consume 'Box'-types.
data Box a = Box a


-- | This is the constructor for the "stable" modality 'Box':
--
-- >     Γ☐ ⊢ t :: 𝜏
-- > --------------------
-- >  Γ ⊢ box t :: Box 𝜏
--
-- where Γ☐ is obtained from Γ by removing ✓ and any variables @x ::
-- 𝜏@, wheere 𝜏 is not a stable type.

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

