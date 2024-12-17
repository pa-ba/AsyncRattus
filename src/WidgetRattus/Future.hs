{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}


-- | Programming with futures.

module WidgetRattus.Future
  ( F(..)
  , SigF(..)
  , mkSigF
  , mkSigF'
  , current
  , future
  , bindF
  , mapF
  , sync
  , syncF
  , switchAwait
  , switch
  , switchS
  , filterMap
  , filterMapAwait
  , filterAwait
  , filter
  , trigger
  , triggerAwait
  , map
  , mapAwait
  , zipWith
  , zipWithAwait
  , fromSig
  , scan
  , scanAwait
  )

where

import WidgetRattus
import WidgetRattus.Signal (Sig(..))
import Prelude hiding (map, filter, zipWith)

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

syncF :: (Stable a, Stable b) => F a -> F b -> F (a :* b)
syncF (Now x) (Now y) = Now (x :* y)
syncF (Wait x) (Now y) = Wait (delay (syncA (adv x) y))
syncF (Now x) (Wait y) = Wait (delay (syncB x (adv y)))
syncF (Wait x) (Wait y) = Wait (delay (case select x y of
                                         Fst x' y' -> syncF x' (Wait y')
                                         Snd x' y' -> syncF (Wait x') y'
                                         Both x' y' -> syncF x' y'
                                      )) 

syncA :: (Stable b) => F a -> b -> F (a :* b)
syncA (Now x) y = Now (x :* y)
syncA (Wait x) y = Wait (delay (syncA (adv x) y))


syncB :: (Stable a) => a -> F b -> F (a :* b)
syncB x (Now y) = Now (x :* y)
syncB x (Wait y) = Wait (delay (syncB x (adv y)))


-- | @SigF a@ is a signal of values of type @a@. In contrast to 'Sig',
-- 'SigF' supports the 'filter' and 'filterMap' functions.
data SigF a = !a :>: !(O (F (SigF a)))


-- | Get the current value of a signal.
current :: SigF a -> a
current (x :>: _) = x


-- | Get the future the signal.
future :: SigF a -> O (F (SigF a))
future (_ :>: xs) = xs


mkSigF :: Box (O a) -> F (SigF a)
mkSigF b = Wait (mkSigF' b) where

mkSigF' :: Box (O a) -> O (F (SigF a))
mkSigF' b = delay (Now (adv (unbox b) :>: mkSigF' b))


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



filterMapAwait :: Box (a -> Maybe' b) -> F(SigF a) -> F (SigF b)
filterMapAwait f (Wait xs) = Wait (delay (filterMapAwait f (adv xs)))
filterMapAwait f (Now (x :>: xs)) = case unbox f x of
                                     Just' y  -> Now (y :>: delay (filterMapAwait f (adv xs)))
                                     Nothing' -> Wait (delay (filterMapAwait f (adv xs)))

filterMap :: Box (a -> Maybe' b) -> SigF a -> F (SigF b)
filterMap f xs = filterMapAwait f (Now xs)


filterAwait :: Box (a -> Bool) -> F( SigF a) -> F (SigF a)
filterAwait p = filterMapAwait (box (\ x -> if unbox p x then Just' x else Nothing'))

filter :: Box (a -> Bool) -> SigF a -> F (SigF a)
filter p = filterMap (box (\ x -> if unbox p x then Just' x else Nothing'))

trigger :: Stable b => Box (a -> b -> c) -> SigF a -> SigF b -> SigF c
trigger f (a :>: as) (b :>: bs) =
  unbox f a b :>:
  delay (uncurry' (trigger' b f) (adv (sync as bs)))

triggerAwait :: Stable b => Box (a -> b -> c) -> F (SigF a) -> SigF b -> F (SigF c)
triggerAwait f (Now (a :>: as)) (b :>: bs)
  = Now (unbox f a b :>: delay (uncurry' (trigger' b f) (adv (sync as bs))))
triggerAwait f (Wait as) (b :>: bs)
  = Wait (delay (uncurry' (trigger' b f) (adv (sync as bs))))

trigger' :: Stable b => b -> Box (a -> b -> c) -> F (SigF a) -> F (SigF b) -> F (SigF c)
trigger' b f (Now (a :>: as)) (Wait bs) =
  Now (unbox f a b :>: delay (uncurry' (trigger' b f) (adv (sync as bs))))
trigger' _ f (Now (a :>: as)) (Now (b :>: bs)) =
  Now (unbox f a b :>: delay (uncurry' (trigger' b f) (adv (sync as bs))))
trigger' b f (Wait as) (Wait bs) =
  Wait (delay (uncurry' (trigger' b f) (adv (sync as bs))))
trigger' _ f (Wait as) (Now (b :>: bs)) =
  Wait (delay (uncurry' (trigger' b f) (adv (sync as bs))))


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
