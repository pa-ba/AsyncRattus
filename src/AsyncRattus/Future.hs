{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}


-- | Programming with futures.

module AsyncRattus.Future
  ( F(..)
  , StrF(..)
  , bindF
  , sync
  , sync'
  , switchAwait
  , switch
  , mapMaybe
  , filterAwait
  , filter
  , map
  , mapAwait
  , zipWith
  , zipWithAwait
  )

where

import AsyncRattus
import Prelude hiding (map, filter, zipWith)



-- | @F a@ will produces a value of type @a@ after zero or more ticks
-- of some clocks
data F a = Now !a | Wait !(O (F a))

-- | @StrF a@ is a stream of values of type @a@.
data StrF a = !a ::: !(O (F (StrF a)))


-- all functions in this module are in Asynchronous Rattus 
{-# ANN module AsyncRattus #-}

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

  
switchAwait :: F (StrF a) -> F (StrF a) -> F(StrF a)
switchAwait _ (Now ys) = Now ys
switchAwait (Now (x ::: xs)) (Wait ys) = Now (x ::: delay (uncurry' switchAwait (adv (sync xs ys)) ))
switchAwait (Wait xs) (Wait ys) = Wait (delay (uncurry' switchAwait (adv (sync xs ys)) ))

switch :: StrF a -> F (StrF a) -> StrF a
switch _ (Now ys) = ys
switch (x ::: xs) (Wait ys) = x ::: delay (uncurry' switchAwait (adv (sync xs ys)))

mapMaybeAwait :: Box (a -> Maybe' b) -> F(StrF a) -> F (StrF b)
mapMaybeAwait f (Wait xs) = Wait (delay (mapMaybeAwait f (adv xs)))
mapMaybeAwait f (Now (x ::: xs)) = case unbox f x of
                                     Just' y  -> Now (y ::: delay (mapMaybeAwait f (adv xs)))
                                     Nothing' -> Wait (delay (mapMaybeAwait f (adv xs)))

mapMaybe :: Box (a -> Maybe' b) -> StrF a -> F (StrF b)
mapMaybe f xs = mapMaybeAwait f (Now xs)


filterAwait :: Box (a -> Bool) -> F( StrF a) -> F (StrF a)
filterAwait p = mapMaybeAwait (box (\ x -> if unbox p x then Just' x else Nothing'))

filter :: Box (a -> Bool) -> StrF a -> F (StrF a)
filter p = mapMaybe (box (\ x -> if unbox p x then Just' x else Nothing'))

mapAwait :: Box (a -> b) -> F (StrF a) -> F (StrF b)
mapAwait f (Now (x ::: xs)) = Now (unbox f x ::: delay (mapAwait f (adv xs)))
mapAwait f (Wait xs) = Wait (delay (mapAwait f (adv xs)))

map :: Box (a -> b) -> StrF a -> StrF b
map f (x ::: xs) = unbox f x ::: delay (mapAwait f (adv xs))



zipWith :: (Stable a, Stable b) => Box(a -> b -> c) -> StrF a -> StrF b -> StrF c
zipWith f (a ::: as) (b ::: bs) = unbox f a b ::: delay (uncurry' (zipWithAwait f a b) (adv (sync as bs)))

zipWithAwait :: (Stable a, Stable b) => Box(a -> b -> c) -> a -> b -> F (StrF a) -> F (StrF b) -> F (StrF c)
zipWithAwait f _ _ (Now (a ::: as)) (Now (b ::: bs)) = Now (unbox f a b ::: delay (uncurry' (zipWithAwait f a b) (adv (sync as bs))))
zipWithAwait f _ b (Now (a ::: as)) (Wait bs) = Now (unbox f a b ::: delay (uncurry' (zipWithAwait f a b) (adv (sync as bs))))
zipWithAwait f a _ (Wait as) (Now (b ::: bs)) = Now (unbox f a b ::: delay (uncurry' (zipWithAwait f a b) (adv (sync as bs))))
zipWithAwait f a b (Wait as) (Wait bs) = Wait (delay (uncurry' (zipWithAwait f a b) (adv (sync as bs))))

bindF :: F a -> Box (a -> F b) -> F b
bindF (Now x) f = unbox f x
bindF (Wait x) f = Wait (delay (bindF (adv x) f))
