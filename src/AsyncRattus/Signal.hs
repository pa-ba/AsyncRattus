{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}


-- | Programming with signals.

module AsyncRattus.Signal
  ( map
  , filterMap
  , filterMapAwait
  , filter
  , filterAwait
  , trigger
  , triggerAwait
  , filterMap'
  , filterMapAwait'
  , filter'
  , filterAwait'
  , trigger'
  , triggerAwait'
  , mapAwait
  , switch
  , switchS
  , switchAwait
  , interleave
  , mkSig
  , mkBoxSig
  , current
  , future
  , fromLater
  , const
  , scan
  , scanAwait
  , scanMap
  , Sig(..)
  , zipWith
  , zipWithAwait
  , zip
  , integral
  , derivative
  )

where

import AsyncRattus
import AsyncRattus.Channels
import Prelude hiding (map, const, zipWith, zip, filter)
import Data.VectorSpace
import Data.Ratio ((%))

instance Producer (Sig a) a where
  prod = map (box Just')


newtype SigMaybe a = SigMaybe (Sig (Maybe' a))

instance Producer (SigMaybe a) a where
  prod (SigMaybe s) = s


{-# ANN module AsyncRattus #-}

-- | Get the current value of a signal.
current :: Sig a -> a
current (x ::: _) = x


-- | Get the future the signal.
future :: Sig a -> O (Sig a)
future (_ ::: xs) = xs

-- | Apply a function to the value of a signal.
map :: Box (a -> b) -> Sig a -> Sig b
map f (x ::: xs) = unbox f x ::: delay (map f (adv xs))


filterMap :: Box (a -> Maybe' b) -> Sig a -> IO (O (Sig b))
filterMap f s = mkSig <$> mkInput (SigMaybe (map f s))

filterMap' :: Box (a -> Maybe' b) -> Sig a -> IO (Box (O (Sig b)))
filterMap' f s = mkBoxSig <$> mkInput (SigMaybe (map f s))


filterMapAwait' :: Box (a -> Maybe' b) -> O (Sig a) -> IO (Box (O (Sig b)))
filterMapAwait' f s = mkBoxSig <$> mkInput (delay (SigMaybe (map f (adv s))))

filterMapAwait :: Box (a -> Maybe' b) -> O (Sig a) -> IO (O (Sig b))
filterMapAwait f s = mkSig <$> mkInput (delay (SigMaybe (map f (adv s))))

filter :: Box (a -> Bool) -> Sig a -> IO (O (Sig a))
filter p = filterMap (box (\ x -> if unbox p x then Just' x else Nothing'))

filter' :: Box (a -> Bool) -> Sig a -> IO (Box (O (Sig a)))
filter' p = filterMap' (box (\ x -> if unbox p x then Just' x else Nothing'))

filterAwait' :: Box (a -> Bool) -> O (Sig a) -> IO (Box (O (Sig a)))
filterAwait' p = filterMapAwait' (box (\ x -> if unbox p x then Just' x else Nothing'))

filterAwait :: Box (a -> Bool) -> O (Sig a) -> IO (O (Sig a))
filterAwait p = filterMapAwait (box (\ x -> if unbox p x then Just' x else Nothing'))

trigger' :: (Stable a, Stable b) => Box (a -> b -> c) -> Sig a -> Sig b -> IO (Box (Sig c))
trigger' f (a ::: as) bs@(b:::_) = do s <- triggerAwait' f as bs
                                      return (box (unbox f a b ::: unbox s))

trigger :: (Stable b) => Box (a -> b -> c) -> Sig a -> Sig b -> IO (Sig c)
trigger f (a ::: as) bs@(b:::_) = do s <- triggerAwait f as bs
                                     return (unbox f a b ::: s)

triggerAwait :: Stable b => Box (a -> b -> c) -> O (Sig a) -> Sig b -> IO (O (Sig c))
triggerAwait f as bs = unbox <$> triggerAwait' f as bs

triggerAwait' :: Stable b => Box (a -> b -> c) -> O (Sig a) -> Sig b -> IO (Box (O (Sig c)))
triggerAwait' f as bs = mkBoxSig <$> mkInput (box SigMaybe `mapO` (trig f as bs)) where
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

mkSig :: Box (O a) -> O (Sig a)
mkSig b = delay (adv (unbox b) ::: mkSig b)

mkBoxSig :: Box (O a) -> Box (O (Sig a))
mkBoxSig b = box (mkSig b)


-- Construct a signal from a stable delayed value, e.g. typically from
-- an input channel.
fromLater :: (Stable a) => Box (O a) -> O (Sig a)
fromLater l = delay (let x = adv (unbox l) in x ::: fromLater l)

-- | Construct a constant signal that never updates.
const :: a -> Sig a
const x = x ::: never

-- | Construct a signal by repeatedly applying a function to a given
-- start element. That is, @unfold (box f) x@ will produce the signal
-- @x ::: f x ::: f (f x) ::: ...@
-- unfold :: Stable a => Box (a -> a) -> a -> Sig a
-- unfold f x = x ::: delay (unfold f (unbox f x))

-- | Similar to Haskell's 'scanl'.
--
-- > scan (box f) x (v1 ::: v2 ::: v3 ::: ... ) == (x `f` v1) ::: ((x `f` v1) `f` v2) ::: ...
--
-- Note: Unlike 'scanl', 'scan' starts with @x `f` v1@, not @x@.

scan :: (Stable b) => Box(b -> a -> b) -> b -> Sig a -> Sig b
scan f acc (a ::: as) = acc' ::: delay (scan f acc' (adv as))
  where acc' = unbox f acc a

-- Like 'scan', but uses a delayed signal.
scanAwait :: (Stable b) => Box (b -> a -> b) -> b -> O (Sig a) -> Sig b
scanAwait f acc as = acc ::: delay (scan f acc (adv as))

-- | 'scanMap' is a composition of 'map' and 'scan':
--
-- > scanMap f g x === map g . scan f x
scanMap :: (Stable b) => Box (b -> a -> b) -> Box (b -> c) -> b -> Sig a -> Sig c
scanMap f p acc (a ::: as) =  unbox p acc' ::: delay (scanMap f p acc' (adv as))
  where acc' = unbox f acc a


switch :: Sig a -> O (Sig a) -> Sig a
switch (x ::: xs) d = x ::: delay (case select xs d of
                                     Fst   xs'  d'  -> switch xs' d'
                                     Snd   _    d'  -> d'
                                     Both  _    d'  -> d')

switchS :: Stable a => Sig a -> O (a -> Sig a) -> Sig a
switchS (x ::: xs) d = x ::: delay (case select xs d of
                                     Fst   xs'  d'  -> switchS xs' d'
                                     Snd   _    f  -> f x
                                     Both  _    f  -> f x)

switchAwait :: O (Sig a) -> O (Sig a) -> O (Sig a)
switchAwait xs ys = delay (case select xs ys of
                                  Fst  xs'  d'  -> switch xs' d'
                                  Snd  _    d'  -> d'
                                  Both _    d'  -> d')


interleave :: Box (a -> a -> a) -> O (Sig a) -> O (Sig a) -> O (Sig a)
interleave f xs ys = delay (case select xs ys of
                              Fst (x ::: xs') ys' -> x ::: interleave f xs' ys'
                              Snd xs' (y ::: ys') -> y ::: interleave f xs' ys'
                              Both (x ::: xs') (y ::: ys') -> unbox f x y ::: interleave f xs' ys')


-- | Similar to 'Prelude.zipWith' on Haskell lists.
-- | Inspired by 'zip' in the Async RaTT paper.
zipWith :: (Stable a, Stable b) => Box(a -> b -> c) -> Sig a -> Sig b -> Sig c
zipWith f (a ::: as) (b ::: bs) = unbox f a b ::: delay (
    case select as bs of
      Fst as' lbs -> zipWith f as' (b ::: lbs)
      Snd las bs' -> zipWith f (a ::: las) bs'
      Both as' bs' -> zipWith f as' bs'
  )

zipWithAwait :: (Stable a, Stable b) => Box(a -> b -> c) -> O (Sig a) -> O (Sig b) -> a -> b -> Sig c
zipWithAwait f as bs defaultA defaultB = unbox f defaultA defaultB :::  delay (
    case select as bs of
      Both as' bs' -> zipWith f as' bs'
      Fst as' lbs -> zipWith f as' (defaultB ::: lbs)
      Snd las bs' -> zipWith f (defaultA ::: las) bs'
  )

zip :: (Stable a, Stable b) => Sig a -> Sig b -> Sig (a:*b)
zip = zipWith (box (:*))

-- | Sampling interval (in microseconds) for the 'integral' and
-- 'derivative' functions.

dt :: Int
dt = 20000

  
integral :: forall a v . (VectorSpace v a, Eq v, Fractional a, Stable v, Stable a)
  => v -> Sig v -> Sig v
integral = int 
  where int cur (x ::: xs)
          | x == zeroVector = cur ::: delay (int cur (adv xs))
          | otherwise = cur ::: delay (
              case select xs (unbox (timer dt)) of
                Fst xs' _ -> int cur xs'
                Snd xs' () -> int (dtf *^ (cur ^+^ x)) (x ::: xs')
                Both (x' ::: xs') () ->  int (dtf *^ (cur ^+^ x')) (x'::: xs'))
         -- sampling interval in seconds
        dtf :: a
        dtf = fromRational (fromIntegral dt % 1000000)
                

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
          Snd xs' () -> der ((x ^-^ last) ^/ dtf) x (x ::: xs')
          Both (x' ::: xs') () ->  der ((x' ^-^ last) ^/ dtf) x' (x' ::: xs'))

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

{-# NOINLINE [1] map #-}
{-# NOINLINE [1] const #-}
{-# NOINLINE [1] scan #-}
{-# NOINLINE [1] scanMap #-}
{-# NOINLINE [1] zip #-}


{-# RULES

  "const/map" forall (f :: Stable b => Box (a -> b))  x.
    map f (const x) = let x' = unbox f x in const x' ;

  "map/map" forall f g xs.
    map f (map g xs) = map (box (unbox f . unbox g)) xs ;

  "map/scan" forall f p acc as.
    map p (scan f acc as) = scanMap f p acc as ;

  "zip/map" forall xs ys f.
    map f (zip xs ys) = let f' = unbox f in zipWith (box (\ x y -> f' (x :* y))) xs ys
#-}


#if __GLASGOW_HASKELL__ >= 808
{-# RULES
  "scan/scan" forall f g b c as.
    scan g c (scan f b as) =
      let f' = unbox f; g' = unbox g in
      scanMap (box (\ (b:*c) a -> let b' = f' b a in (b':* g' c b'))) (box snd') (b:*c) as ;

  "scan/scanMap" forall f g p b c as.
    scan g c (scanMap f p b as) =
      let f' = unbox f; g' = unbox g; p' = unbox p in
      scanMap (box (\ (b:*c) a -> let b' = f' (p' b) a in (b':* g' c b'))) (box snd') (b:*c) as ;

#-}
#endif
