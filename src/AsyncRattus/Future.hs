{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}


-- | Programming with futures.

module AsyncRattus.Future
  ( F(..)
  , SigF(..)
  , current
  , future
  , bindF
  , mapF
  , sync
  , sync'
  , switchAwait
  , switch
  , switchS
  , mapMaybe
  , filterAwait
  , filter
  , map
  , mapAwait
  , zipWith
  , zipWithAwait
  , fromSig
  , scan
  , scanAwait
  )

where

import AsyncRattus
import Prelude hiding (map, filter, zipWith)
import AsyncRattus.Channels

{-# ANN module AsyncRattus #-}

newtype OneShot a = OneShot (F a)

instance Producer (OneShot a) where
  type Output (OneShot a) = a
  mkSig (OneShot (Now x)) = Just' x ::: never
  mkSig (OneShot (Wait x)) = Nothing' ::: delay (mkSig (OneShot (adv x)))

instance Producer p => Producer (F p) where
  type Output (F p) = Output p
  mkSig (Now x) = mkSig x
  mkSig (Wait x) = Nothing' ::: delay (mkSig (adv x))

instance Producer (SigF a) where
  type Output (SigF a) = a
  mkSig (x :>: xs) = Just' x ::: delay (mkSig (adv xs))



-- | @F a@ will produces a value of type @a@ after zero or more ticks
-- of some clocks
data F a = Now !a | Wait !(O (F a))



bindF :: F a -> Box (a -> F b) -> F b
bindF (Now x) f = unbox f x
bindF (Wait x) f = Wait (delay (bindF (adv x) f))

mapF :: Box (a -> b) -> F a -> F b
mapF f d = d `bindF` (box (\ x -> Now (unbox f x)))


sync :: O (F a) -> O (F b) -> O (F a :* F b)
sync x y = delay (case select x y of
                     Fst x' y' -> (x' :* Wait y')
                     Snd x' y' -> (Wait x' :* y')
                     Both x' y' -> (x' :* y'))


sync' :: (Stable a, Stable b) => F a -> F b -> F (a :* b)
sync' (Now x) (Now y) = Now (x :* y)
sync' (Wait x) (Now y) = Wait (delay (syncA (adv x) y))
sync' (Now x) (Wait y) = Wait (delay (syncB x (adv y)))
sync' (Wait x) (Wait y) = Wait (delay (case select x y of
                                         Fst x' y' -> sync' x' (Wait y')
                                         Snd x' y' -> sync' (Wait x') y'
                                         Both x' y' -> sync' x' y'
                                      )) 

syncA :: (Stable b) => F a -> b -> F (a :* b)
syncA (Now x) y = Now (x :* y)
syncA (Wait x) y = Wait (delay (syncA (adv x) y))


syncB :: (Stable a) => a -> F b -> F (a :* b)
syncB x (Now y) = Now (x :* y)
syncB x (Wait y) = Wait (delay (syncB x (adv y)))


-- | @SigF a@ is a signal of values of type @a@. In contrast to 'Sig',
-- 'SigF' supports the 'filter' and 'mapMaybe' functions.
data SigF a = !a :>: !(O (F (SigF a)))


-- | Get the current value of a signal.
current :: SigF a -> a
current (x :>: _) = x


-- | Get the future the signal.
future :: SigF a -> O (F (SigF a))
future (_ :>: xs) = xs

fromSig :: Sig a -> SigF a
fromSig (x ::: xs) = x :>: delay (Now (fromSig (adv xs)))

  
switchAwait :: F (SigF a) -> F (SigF a) -> F(SigF a)
switchAwait _ (Now ys) = Now ys
switchAwait (Now (x :>: xs)) (Wait ys) = Now (x :>: delay (uncurry' switchAwait (adv (sync xs ys)) ))
switchAwait (Wait xs) (Wait ys) = Wait (delay (uncurry' switchAwait (adv (sync xs ys)) ))

switch :: SigF a -> F (SigF a) -> SigF a
switch _ (Now ys) = ys
switch (x :>: xs) (Wait ys) = x :>: delay (uncurry' switchAwait (adv (sync xs ys)))

switchS :: Stable a => SigF a -> F (a -> SigF a) -> SigF a
switchS (x :>: _) (Now f) = f x
switchS (x :>: xs) (Wait ys) = x :>: delay (uncurry' (switchAwaitS x) (adv (sync xs ys)))

switchAwaitS :: Stable a => a -> F (SigF a) -> F (a -> SigF a) -> F (SigF a)
switchAwaitS _ (Now (x :>: _)) (Now f) = Now (f x)
switchAwaitS _ (Now (x :>: xs)) (Wait ys) =
  Now (x :>: delay (uncurry' (switchAwaitS x) (adv (sync xs ys))))
switchAwaitS x (Wait _) (Now f) = Now (f x)
switchAwaitS x (Wait xs) (Wait ys) = Wait (delay (uncurry' (switchAwaitS x) (adv (sync xs ys))))



mapMaybeAwait :: Box (a -> Maybe' b) -> F(SigF a) -> F (SigF b)
mapMaybeAwait f (Wait xs) = Wait (delay (mapMaybeAwait f (adv xs)))
mapMaybeAwait f (Now (x :>: xs)) = case unbox f x of
                                     Just' y  -> Now (y :>: delay (mapMaybeAwait f (adv xs)))
                                     Nothing' -> Wait (delay (mapMaybeAwait f (adv xs)))

mapMaybe :: Box (a -> Maybe' b) -> SigF a -> F (SigF b)
mapMaybe f xs = mapMaybeAwait f (Now xs)


filterAwait :: Box (a -> Bool) -> F( SigF a) -> F (SigF a)
filterAwait p = mapMaybeAwait (box (\ x -> if unbox p x then Just' x else Nothing'))

filter :: Box (a -> Bool) -> SigF a -> F (SigF a)
filter p = mapMaybe (box (\ x -> if unbox p x then Just' x else Nothing'))

mapAwait :: Box (a -> b) -> F (SigF a) -> F (SigF b)
mapAwait f (Now (x :>: xs)) = Now (unbox f x :>: delay (mapAwait f (adv xs)))
mapAwait f (Wait xs) = Wait (delay (mapAwait f (adv xs)))

map :: Box (a -> b) -> SigF a -> SigF b
map f (x :>: xs) = unbox f x :>: delay (mapAwait f (adv xs))



zipWith :: (Stable a, Stable b) => Box(a -> b -> c) -> SigF a -> SigF b -> SigF c
zipWith f (a :>: as) (b :>: bs) = unbox f a b :>: delay (uncurry' (zipWithAwait f a b) (adv (sync as bs)))

zipWithAwait :: (Stable a, Stable b) => Box(a -> b -> c) -> a -> b -> F (SigF a) -> F (SigF b) -> F (SigF c)
zipWithAwait f _ _ (Now (a :>: as)) (Now (b :>: bs)) = Now (unbox f a b :>: delay (uncurry' (zipWithAwait f a b) (adv (sync as bs))))
zipWithAwait f _ b (Now (a :>: as)) (Wait bs) = Now (unbox f a b :>: delay (uncurry' (zipWithAwait f a b) (adv (sync as bs))))
zipWithAwait f a _ (Wait as) (Now (b :>: bs)) = Now (unbox f a b :>: delay (uncurry' (zipWithAwait f a b) (adv (sync as bs))))
zipWithAwait f a b (Wait as) (Wait bs) = Wait (delay (uncurry' (zipWithAwait f a b) (adv (sync as bs))))

scan :: (Stable b) => Box(b -> a -> b) -> b -> SigF a -> SigF b
scan f acc (a :>: as) = acc' :>: delay (scanAwait f acc' (adv as))
  where acc' = unbox f acc a

scanAwait :: (Stable b) => Box (b -> a -> b) -> b -> F (SigF a) -> F (SigF b)
scanAwait f acc (Now (a :>: as)) = Now (acc' :>: delay (scanAwait f acc' (adv as)))
  where acc' = unbox f acc a
scanAwait f acc (Wait as) = Wait (delay (scanAwait f acc (adv as)))
