{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AsyncRattus.InternalPrimitives where

import Prelude hiding (Left, Right)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IORef
import Control.Concurrent.MVar
import Data.Maybe
import System.IO.Unsafe
import System.Mem.Weak
import Control.Monad

-- An input channel is identified by an integer. The programmer should not know about it.
type InputChannelIdentifier = Int

type Clock = IntSet

singletonClock :: InputChannelIdentifier -> Clock
singletonClock = IntSet.singleton

clockUnion :: Clock -> Clock -> Clock
clockUnion = IntSet.union

channelMember :: InputChannelIdentifier -> Clock -> Bool
channelMember = IntSet.member

data InputValue where
  InputValue :: !InputChannelIdentifier -> !a -> InputValue


-- | The "later" type modality. A value @v@ of type @O 𝜏@ consists of
-- two components: Its clock, denoted @cl(v)@, and a delayed
-- computation that will produce a value of type @𝜏@ as soon as the
-- clock @cl(v)@ ticks. The clock @cl(v)@ is only used for type
-- checking and is not directly accessible, whereas the delayed
-- computation is accessible via 'adv' and 'select'.

data O a = Delay !Clock (InputValue -> a)

-- | The return type of the 'select' primitive.
data Select a b = Fst !a !(O b) | Snd !(O a) !b | Both !a !b

asyncRattusError pr = error (pr ++ ": Did you forget to mark this as Async Rattus code?")

-- | This is the constructor for the "later" modality 'O':
--
-- >     Γ ✓θ ⊢ t :: 𝜏
-- > --------------------
-- >  Γ ⊢ delay t :: O 𝜏
--
-- The typing rule requires that its argument @t@ typecheck with an
-- additional tick @✓θ@ of some clock @θ@.
{-# INLINE [1] delay #-}
delay :: a -> O a
delay _ = asyncRattusError "delay"

extractClock :: O a -> Clock
extractClock (Delay cl _) = cl

{-# INLINE [1] adv' #-}
adv' :: O a -> InputValue -> a
adv' (Delay _ f) inp = f inp


-- | This is the eliminator for the "later" modality 'O':
--
-- >   Γ ⊢ t :: O 𝜏     Γ' tick-free
-- > ---------------------------------
-- >     Γ ✓cl(t) Γ' ⊢ adv t :: 𝜏
--
-- It requires that a tick @✓θ@ is in the context whose clock matches
-- exactly the clock of @t@, i.e. @θ = cl(t)@.

{-# INLINE [1] adv #-}
adv :: O a -> a
adv _ = asyncRattusError "adv"

-- | If we want to eliminate more than one delayed computation, i.e.\
-- two @s :: O σ@ and @t :: O 𝜏@, we need to use 'select' instead of
-- just 'adv'.
--
-- >   Γ ⊢ s :: O σ     Γ ⊢ t :: O 𝜏     Γ' tick-free
-- > --------------------------------------------------
-- >    Γ ✓cl(s)⊔cl(t) Γ' ⊢ select s t :: Select σ 𝜏
--
-- It requires that we have a tick @✓θ@ in the context whose clock
-- matches the union of the clocks of @s@ and @t@, i.e. @θ =
-- cl(s)⊔cl(t)@. The union of two clocks ticks whenever either of the
-- two clocks ticks, i.e. @cl(s)⊔cl(t)@, whenever @cl(s)@ or @cl(t)@
-- ticks.
--
-- That means there are three possible outcomes, which are reflected
-- in the result type of @select s t@. A value of @Select σ 𝜏@ is
-- either
--
--   * a value of type @σ@ and a delayed computation of type @O 𝜏@, if
--     @cl(s)@ ticks before @cl(t)@,
--
--   * a value of type @𝜏@ and a delayed computation of type @O σ@, if
--     @cl(t)@ ticks before @cl(s)@, or
--
--   * a value of type @σ@ and a value of type @𝜏@, if @cl(s)@ and
--   * @cl(s)@ tick simultaneously.


{-# INLINE [1] select #-}
select :: O a -> O b -> Select a b
select _ _ = asyncRattusError "select"

select' :: O a -> O b -> InputValue -> Select a b
select' a@(Delay clA inpFA) b@(Delay clB inpFB) inputValue@(InputValue chId _)
  = if chId `channelMember` clA then
      if chId `channelMember` clB then Both (inpFA inputValue) (inpFB inputValue)
      else Fst (inpFA inputValue) b
    else Snd a (inpFB inputValue)


-- | The clock of @never :: O 𝜏@ will never tick, i.e. it will never
-- produce a value of type @𝜏@. With 'never' we can for example
-- implement the constant signal @x ::: never@ of type @Sig a@ for any @x ::
-- a@.
never :: O a
never = Delay IntSet.empty (error "Trying to adv on the 'never' delayed computation")

-- | A type is @Stable@ if it is a strict type and the later modality
-- @O@ and function types only occur under @Box@.
--
-- For example, these types are stable: @Int@, @Box (a -> b)@, @Box (O
-- Int)@, @Box (Sig a -> Sig b)@.
--
-- But these types are not stable: @[Int]@ (because the list type is
-- not strict), @Int -> Int@, (function type is not stable), @O
-- Int@, @Sig Int@.

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
-- where Γ☐ is obtained from Γ by removing all ticks and all variables
-- @x :: 𝜏@, where 𝜏 is not a stable type.

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


defaultPromote :: Continuous a => a -> Box a
defaultPromote x = unsafePerformIO $ 
    do r <- newIORef x
       r' <- mkWeakIORef r (return ()) 
       modifyIORef promoteStore (ContinuousData r' :)
       return (Box (unsafePerformIO $ readIORef r))


class Continuous p where
  progressInternal :: InputValue -> p -> p
  promoteInternal :: p -> Box p
  promoteInternal = defaultPromote

-- For stable types we can circumvent the "promote store".
instance {-# OVERLAPPABLE #-} Stable a => Continuous a where
    progressInternal _ x = x
    promoteInternal = Box

data ContinuousData where
   ContinuousData :: Continuous a => !(Weak (IORef a)) -> ContinuousData

-- TODO: The list type needs to be replaced by a more efficient
-- mutable data structure.
{-# NOINLINE promoteStore #-}
promoteStore :: IORef [ContinuousData]
promoteStore = unsafePerformIO (newIORef [])

{-# NOINLINE progressPromoteStoreMutex #-}
progressPromoteStoreMutex :: MVar ()
progressPromoteStoreMutex = unsafePerformIO (newMVar ())


-- | Atomic version of 'progressPromoteStore'.

progressPromoteStoreAtomic :: InputValue -> IO ()
progressPromoteStoreAtomic inp = do
    takeMVar progressPromoteStoreMutex
    progressPromoteStore inp
    putMVar progressPromoteStoreMutex ()


-- | For promote to work, its argument must be stored in the "promote
-- store", and whenenver an input is received on some channel, all
-- values in the "promote store" must be advanced (using
-- 'progressInternal').

progressPromoteStore :: InputValue -> IO ()
progressPromoteStore inp = do 
    xs <- atomicModifyIORef promoteStore (\x -> ([],x))
    putStrLn ("promote store size: " ++ show (length xs))
    xs' <- filterM run xs
    atomicModifyIORef promoteStore (\x -> (x ++ xs',()))
  where run (ContinuousData x) = do
          d <- deRefWeak x
          case d of
            Nothing -> return False
            Just x -> modifyIORef' x (progressInternal inp) >> return True

promote :: Continuous a => a -> Box a
promote x = promoteInternal x

newtype Chan a = Chan InputChannelIdentifier

{-# RULES
  "unbox/box"    forall x. unbox (box x) = x
    #-}


{-# RULES
  "box/unbox"    forall x. box (unbox x) = x
    #-}
