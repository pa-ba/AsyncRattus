{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}


-- | Programming with signals.

module WidgetRattus.Signal
  ( map
  , mkInputSig
  , getInputSig
  , filterMap
  , filterMapAwait
  , filter
  , filterAwait
  , trigger
  , triggerAwait
  , mapAwait
  , switch
  , switchS
  , switchB
  , combine
  , triggerStable
  , triggerStable3
  , buffer
  , bufferAwait
  , switchAwait
  , interleave
  , mkSig
  , mkBoxSig
  , current
  , future
  , const
  , jump
  , jumping
  , stop
  , scan
  , scanC
  , scanAwait
  , scanAwaitC
  , scanMap
  , Sig(..)
  , zipWith
  , zipWith3
  , zip
  , cond
  , update
  , integral
  , derivative
  )

where

import WidgetRattus
import Prelude hiding (map, const, zipWith, zipWith3, zip, filter)
import Data.VectorSpace
import Data.Ratio ((%))
-- TODO: InternalPrimitives is only used to implment instance of
-- Continuous. Replace this manual instance declaration with Template
-- Haskell.
import WidgetRattus.InternalPrimitives

infixr 5 :::

-- | @Sig a@ is a stream of values of type @a@.
data Sig a = !a ::: !(O (Sig a))

instance Producer (Sig a) a where
  getCurrent p = Just' (current p)
  getNext p cb = cb (future p)

newtype SigMaybe a = SigMaybe (Sig (Maybe' a))

instance Producer (SigMaybe a) a where
  getCurrent (SigMaybe p) = current p
  getNext (SigMaybe p) cb = cb (delay (SigMaybe (adv (future p))))

-- | Get the current value of a signal.
current :: Sig a -> a
current (x ::: _) = x


-- | Get the future the signal.
future :: Sig a -> O (Sig a)
future (_ ::: xs) = xs

-- | Apply a function to the value of a signal.
map :: Box (a -> b) -> Sig a -> Sig b
map f (x ::: xs) = unbox f x ::: delay (map f (adv xs))

-- | Variant of 'getInput' that returns a signal instead of a boxed
-- delayed computation.
getInputSig :: IO (Box (O (Sig a)) :* (a -> IO ()))
getInputSig = do (s :* cb) <- getInput
                 return (mkBoxSig s :* cb)

-- | Turn a producer into a signal. This is a variant of 'mkInput'
-- that returns a signal instead of a boxed delayed computation.
mkInputSig :: Producer p a => p -> IO (Box (O (Sig a)))
mkInputSig p = mkBoxSig <$> mkInput p


-- | This function is essentially the composition of 'filter' with
-- 'map'. The signal produced by @filterMap f s@ has the value @v@
-- whenever @s@ has the value @u@ such that @unbox f u = Just' v@.
filterMap :: Box (a -> Maybe' b) -> Sig a -> IO (Box (O (Sig b)))
filterMap f s = mkInputSig (SigMaybe (map f s))

-- | This function is similar to 'filterMap' but takes a delayed
-- signal (type @O (Sig a)@) as an argument instead of a signal (@Sig
-- a@).
filterMapAwait :: Box (a -> Maybe' b) -> O (Sig a) -> IO (Box (O (Sig b)))
filterMapAwait f s = mkInputSig (delay (SigMaybe (map f (adv s))))

-- | Filter the given signal using a predicate. The signal produced by
-- @filter p s@ contains only values from @s@ that satisfy the
-- predicate @p@.
filter :: Box (a -> Bool) -> Sig a -> IO (Box (O (Sig a)))
filter p = filterMap (box (\ x -> if unbox p x then Just' x else Nothing'))

-- | This function is similar to 'filter' but takes a delayed signal
-- (type @O (Sig a)@) as an argument instead of a signal (@Sig a@).
filterAwait :: Box (a -> Bool) -> O (Sig a) -> IO (Box (O (Sig a)))
filterAwait p = filterMapAwait (box (\ x -> if unbox p x then Just' x else Nothing'))


-- | This function is a variant of 'zipWith'. Whereas @zipWith f xs
-- ys@ produces a new value whenever @xs@ or @ys@ produce a new value,
-- @trigger f xs ys@ only produces a new value when xs produces a new
-- value.
--
-- Example:
--
-- >                      xs:  1 2 3     2
-- >                      ys:  1     0 5 2
-- >
-- > zipWith (box (+)) xs ys:  2 3 4 3 8 4
-- > trigger (box (+)) xy ys:  2     3 8 4

trigger :: (Stable a, Stable b) => Box (a -> b -> c) -> Sig a -> Sig b -> IO (Box (Sig c))
trigger f (a ::: as) bs@(b:::_) = do s <- triggerAwait f as bs
                                     return (box (unbox f a b ::: unbox s))
-- | This function is similar to 'trigger' but takes a delayed signal
-- (type @O (Sig a)@) as an argument instead of a signal (@Sig a@).
triggerAwait :: Stable b => Box (a -> b -> c) -> O (Sig a) -> Sig b -> IO (Box (O (Sig c)))
triggerAwait f as bs = mkBoxSig <$> mkInput (box SigMaybe `mapO` (trig f as bs)) where
  trig :: Stable b => Box (a -> b -> c) -> O (Sig a) -> Sig b -> O (Sig (Maybe' c))
  trig f as (b ::: bs) =
    delay (case select as bs of
            Fst (a' ::: as') bs' -> Just' (unbox f a' b) ::: trig f as' (b ::: bs')
            Snd as' bs' -> Nothing' ::: trig f as' bs'
            Both (a' ::: as') (b' ::: bs') -> Just' (unbox f a' b') ::: trig f as' (b' ::: bs')
          )

-- | A version of @map@ for delayed signals.
mapAwait :: Box (a -> b) -> O (Sig a) -> O (Sig b)
mapAwait f d = delay (map f (adv d))

-- | Turns a boxed delayed computation into a delayed signal.
mkSig :: Box (O a) -> O (Sig a)
mkSig b = delay (adv (unbox b) ::: mkSig b)

-- | Variant of 'mkSig' that returns a boxed delayed signal
mkBoxSig :: Box (O a) -> Box (O (Sig a))
mkBoxSig b = box (mkSig b)


-- | Construct a constant signal that never updates.
const :: a -> Sig a
const x = x ::: never

-- | Similar to Haskell's 'scanl'.
--
-- > scan (box f) x (v1 ::: v2 ::: v3 ::: ... ) == (x `f` v1) ::: ((x `f` v1) `f` v2) ::: ...
--
-- Note: Unlike 'scanl', 'scan' starts with @x `f` v1@, not @x@.

scan :: (Stable b) => Box(b -> a -> b) -> b -> Sig a -> Sig b
scan f acc (a ::: as) = acc' ::: delay (scan f acc' (adv as))
  where acc' = unbox f acc a

scanC :: (Stable b) => Box(b -> a -> C b) -> b -> Sig a -> C (Sig b)
scanC f acc (a ::: as) = do
    acc' <- unbox f acc a
    fut <- delayC $ delay (scanC f acc' (adv as))
    return (acc' ::: fut)
  where 
        
-- | Like 'scan', but uses a delayed signal.
scanAwait :: (Stable b) => Box (b -> a -> b) -> b -> O (Sig a) -> Sig b
scanAwait f acc as = acc ::: delay (scan f acc (adv as))

scanAwaitC :: (Stable b) => Box (b -> a -> C b) -> b -> O (Sig a) -> C (Sig b)
scanAwaitC f acc as = do 
    fut <- delayC $ delay (scanC f acc (adv as))
    return (acc ::: fut)

-- | 'scanMap' is a composition of 'map' and 'scan':
--
-- > scanMap f g x === map g . scan f x
scanMap :: (Stable b) => Box (b -> a -> b) -> Box (b -> c) -> b -> Sig a -> Sig c
scanMap f p acc (a ::: as) =  unbox p acc' ::: delay (scanMap f p acc' (adv as))
  where acc' = unbox f acc a

-- | @jump (box f) xs@ first behaves like @xs@, but as soon as @f x =
-- Just xs'@ for a (current or future) value @x@ of @xs@, it behaves
-- like @xs'@.

jump :: Box (a -> Maybe' (Sig a)) -> Sig a -> Sig a
jump f (x ::: xs) = case unbox f x of
                        Just' xs' -> xs'
                        Nothing' -> x ::: delay (jump f (adv xs))


-- | Similar to 'jump', but it can jump repeatedly. That is, @jumping
-- (box f) xs@ first behaves like @xs@, but every time @f x = Just
-- xs'@ for a (current or future) value @x@ of @jumping (box f) xs@,
-- it behaves like @xs'@.

jumping :: Box (a -> Maybe' (Sig a)) -> Sig a -> Sig a
jumping f (x ::: xs) = case unbox f x of
                         Just' (x' ::: xs') -> x' ::: delay (jumping f (adv xs'))
                         Nothing'           -> x  ::: delay (jumping f (adv xs))

-- | Stops as soon as the the predicate becomes true for the current
-- value. That is, @stop (box p) xs@ first behaves as @xs@, but as
-- soon as @f x = True@ for some (current or future) value @x@ of
-- @xs@, then it behaves as @const x@.
stop :: Box (a -> Bool) -> Sig a ->  Sig a
stop p = jump (box (\ x -> if unbox p x then Just' (const x) else Nothing'))

-- | This function allows to switch from one signal to another one
-- dynamically. The signal defined by @switch xs ys@ first behaves
-- like @xs@, but as soon as @ys@ produces a new value, @switch xs ys@
-- behaves like @ys@.
--
-- Example:
--
-- >           xs: 1 2 3 4 5   6 7 8   9
-- >           ys:         1 2   3 4 5 6
-- >
-- > switch xs ys: 1 2 3 1 2 4   3 4 5 6
switch :: Sig a -> O (Sig a) -> Sig a
switch (x ::: xs) d = x ::: delay (case select xs d of
                                     Fst   xs'  d'  -> switch xs' d'
                                     Snd   _    d'  -> d'
                                     Both  _    d'  -> d')

-- | This function is similar to 'switch', but the (future) second
-- signal may depend on the last value of the first signal.
switchS :: Stable a => Sig a -> O (a -> Sig a) -> Sig a
switchS (x ::: xs) d = x ::: delay (case select xs d of
                                     Fst   xs'  d'  -> switchS xs' d'
                                     Snd   _    f  -> f x
                                     Both  _    f  -> f x)

-- | This function is similar to 'switch' but works on delayed signals
-- instead of signals.
switchAwait :: O (Sig a) -> O (Sig a) -> O (Sig a)
switchAwait xs ys = delay (case select xs ys of
                                  Fst  xs'  d'  -> switch xs' d'
                                  Snd  _    d'  -> d'
                                  Both _    d'  -> d')

-- | This function interleaves two signals producing a new value @v@
-- whenever either input stream produces a new value @v@. In case the
-- input signals produce a new value simultaneously, the function
-- argument is used break ties, i.e. to compute the new output value based
-- on the two new input values
--
-- Example:
--
-- >                         xs: 1 3   5 3 1 3
-- >                         ys:   0 2   4
-- >
-- > interleave (box (+)) xs ys: 1 3 2 5 7 1 3
interleave :: Box (a -> a -> a) -> O (Sig a) -> O (Sig a) -> O (Sig a)
interleave f xs ys = delay (case select xs ys of
                              Fst (x ::: xs') ys' -> x ::: interleave f xs' ys'
                              Snd xs' (y ::: ys') -> y ::: interleave f xs' ys'
                              Both (x ::: xs') (y ::: ys') -> unbox f x y ::: interleave f xs' ys')


-- | Takes two signals and updates the first signal using the
-- functions produced by the second signal:
--
-- Law:
--
-- (xs `update` fs) `update` gs = (xs `update` (interleave (box (.)) gs fs))
update :: (Stable a) => Sig a -> O (Sig (a -> a)) -> Sig a
update (x ::: xs) fs = x ::: delay 
    (case select xs fs of
      Fst xs' ys' -> update xs' ys'
      Snd xs' (f ::: fs') -> update (f x ::: xs') fs'
      Both (x' ::: xs') (f ::: fs') -> update (f x' ::: xs') fs')


-- | This function is a variant of combines the values of two signals
-- using the function argument. @zipWith f xs ys@ produces a new value
-- @unbox f x y@ whenever @xs@ or @ys@ produce a new value, where @x@
-- and @y@ are the current values of @xs@ and @ys@, respectively.
--
-- Example:
--
-- >                      xs:  1 2 3     2
-- >                      ys:  1     0 5 2
-- >
-- > zipWith (box (+)) xs ys:  2 3 4 3 8 4

zipWith :: (Stable a, Stable b) => Box(a -> b -> c) -> Sig a -> Sig b -> Sig c
zipWith f (a ::: as) (b ::: bs) = unbox f a b ::: delay (
    case select as bs of
      Fst as' lbs -> zipWith f as' (b ::: lbs)
      Snd las bs' -> zipWith f (a ::: las) bs'
      Both as' bs' -> zipWith f as' bs'
  )

-- | Variant of 'zipWith' with three signals.
zipWith3 :: forall a b c d. (Stable a, Stable b, Stable c) => Box(a -> b -> c -> d) -> Sig a -> Sig b -> Sig c -> Sig d
zipWith3 f as bs cs = zipWith (box (\f x -> unbox f x)) cds cs
  where cds :: Sig (Box (c -> d))
        cds = zipWith (box (\a b -> box (\ c -> unbox f a b c))) as bs

-- | If-then-else lifted to signals. @cond bs xs ys@ produces a stream
-- whose value is taken from @xs@ whenever @bs@ is true and from @ys@
-- otherwise.
cond :: Stable a => Sig Bool -> Sig a -> Sig a -> Sig a
cond = zipWith3 (box (\b x y -> if b then x else y))


-- | This is a special case of 'zipWith' using the tupling
-- function. That is,
--
-- > zip = zipWith (box (:*))
zip :: (Stable a, Stable b) => Sig a -> Sig b -> Sig (a:*b)
zip = zipWith (box (:*))


-- Variant of the switchS Async Rattus function
-- switchB allows for recursive dynamic change in signal behaviour
-- whenever the input signal ticks.
-- The new behaviour is determined by the input function
-- as well as the current value of the input and output signals.
switchB :: Stable a => O (Sig (a -> a)) -> Box (a -> Sig a)-> a -> Sig a
switchB steps f st = switchS ((unbox f) st)
      (delay (let step ::: steps' = adv steps in switchB steps' f . step))


-- Helper function that interleaves two signals of functions.
combine :: O (Sig (a -> a)) -> O (Sig (a -> a)) -> O (Sig (a -> a))
combine = interleave (box (.))

-- Variant of the Async Rattus trigger function.
-- Implemented without the Maybe monad, hence ticks in response
-- to either input signal, but only changes its value when the 
-- delayed signal ticks.
triggerStable :: (Stable b, Stable c) => Box (a -> b -> c) -> c -> O (Sig a) -> Sig b -> Sig c
triggerStable f c as (b ::: bs) = c :::
    delay (case select as bs of
            Fst (a' ::: as') bs' -> triggerStable f (unbox f a' b) as' (b ::: bs')
            Snd as' bs' -> triggerStable f c as' bs'
            Both (a' ::: as') (b' ::: bs') -> triggerStable f (unbox f a' b') as' (b' ::: bs'))

-- Variant of triggerStable function that takes three inputs.
-- The resulting signal only updates when the later signal ticks.
triggerStable3 :: (Stable a, Stable b, Stable c, Stable d) => Box (a -> b -> c -> d) -> Box(c->d) -> d -> O (Sig a) -> Sig b -> Sig c -> Sig d
triggerStable3 f g d as bs cs = triggerStable (box (\f x -> unbox f x)) d cds cs
      where cds = future (triggerStable (box (\a b -> box (\ c -> unbox f a b c ))) g as bs)

-- Buffer takes an initial value and a signal as input and returns a signal that
-- is always one tick behind the input signal.
buffer :: Stable a => a -> Sig a -> Sig a
buffer x (y ::: ys) = x ::: delay (buffer y (adv ys))

-- Like buffer but works for delayed signals
bufferAwait :: Stable a => a -> O (Sig a) -> O (Sig a)
bufferAwait x xs = delay (buffer x (adv xs))

-- | Sampling interval (in microseconds) for the 'integral' and
-- 'derivative' functions.

dt :: Int
dt = 20000

-- | @integral x xs@ computes the integral of the signal @xs@ with the
-- constant @x@. For example, if @xs@ is the velocity of an object,
-- the signal @integral 0 xs@ describes the distance travelled by that
-- object.
integral :: forall a v . (VectorSpace v a, Eq v, Fractional a, Stable v, Stable a)
  => v -> Sig v -> Sig v
integral = int 
  where int cur (x ::: xs)
          | x == zeroVector = cur ::: delay (int cur (adv xs))
          | otherwise = cur ::: delay (
              case select xs (unbox (timer dt)) of
                Fst xs' _ -> int cur xs'
                Snd xs' _ -> int (dtf *^ (cur ^+^ x)) (x ::: xs')
                Both (x' ::: xs') _ ->  int (dtf *^ (cur ^+^ x')) (x'::: xs'))
         -- sampling interval in seconds
        dtf :: a
        dtf = fromRational (fromIntegral dt % 1000000)
                
-- | Compute the derivative of a signal. For example, if @xs@ is the
-- velocity of an object, the signal @derivative xs@ describes the
-- acceleration travelled by that object.
derivative :: forall a v . (VectorSpace v a, Eq v, Fractional a, Stable v, Stable a)
  => Sig v -> Sig v
derivative xs = der zeroVector (current xs) xs where
  -- inverse sampling interval in seconds
  dtf :: a
  dtf = fromIntegral dt * 0.000001

  der :: v -> v -> Sig v -> Sig v
  der d last (x:::xs)
    | d == zeroVector = zeroVector ::: delay
                        (let x' ::: xs' = adv xs
                         in der ((x' ^-^ x) ^/ dtf) x (x' ::: xs'))
    | otherwise = d ::: delay (
        case select xs (unbox (timer dt)) of
          Fst xs' _ -> der d last xs'
          Snd xs' _ -> der ((x ^-^ last) ^/ dtf) x (x ::: xs')
          Both (x' ::: xs') _ ->  der ((x' ^-^ last) ^/ dtf) x' (x' ::: xs'))


instance Continuous a => Continuous (Sig a) where
    progressInternal inp (x ::: xs@(Delay cl _)) = 
        if inputInClock inp cl then (adv' xs inp)
        else progressInternal inp x ::: xs
    progressAndNext inp (x ::: xs@(Delay cl _)) = 
        if inputInClock inp cl then let n = adv' xs inp in (n, nextProgress n)
        else let (n , cl') = progressAndNext inp x in (n ::: xs , cl `clockUnion` cl')
    nextProgress (x ::: (Delay cl _)) = nextProgress x `clockUnion` cl

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

{-# NOINLINE [1] map #-}
{-# NOINLINE [1] const #-}
{-# NOINLINE [1] scan #-}
{-# NOINLINE [1] scanMap #-}
{-# NOINLINE [1] zip #-}
{-# NOINLINE [1] update #-}
{-# NOINLINE [1] switch #-}


{-# RULES

  "const/switch" forall x xs.
  switch (const x) xs = x ::: xs;

  "update/update" forall xs fs gs.
    update (update xs fs) gs = update xs (interleave (box (.)) gs fs) ;

  "const/map" forall (f :: Stable b => Box (a -> b))  x.
    map f (const x) = let x' = unbox f x in const x' ;

  "map/map" forall f g xs.
    map f (map g xs) = map (box (unbox f . unbox g)) xs ;

  "map/scan" forall f p acc as.
    map p (scan f acc as) = scanMap f p acc as ;

  "zip/map" forall xs ys f.
    map f (zip xs ys) = let f' = unbox f in zipWith (box (\ x y -> f' (x :* y))) xs ys;

  "scan/scan" forall f g b c as.
    scan g c (scan f b as) =
      let f' = unbox f; g' = unbox g in
      scanMap (box (\ (b:*c) a -> let b' = f' b a in (b':* g' c b'))) (box snd') (b:*c) as ;

  "scan/scanMap" forall f g p b c as.
    scan g c (scanMap f p b as) =
      let f' = unbox f; g' = unbox g; p' = unbox p in
      scanMap (box (\ (b:*c) a -> let b' = f' (p' b) a in (b':* g' c b'))) (box snd') (b:*c) as ;

#-}
