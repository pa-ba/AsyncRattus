{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}


-- | Programming with streams.

module AsyncRattus.Stream
  ( map
  , mapAwait
  , switch
  , switchS
  , switchAwait
  , interleave
  , mkSignal
  , hd
  , tl
  , fromLater
  , const
  , shift
  , shiftMany
  , scan
  , scanAwait
  , scanMap
  , Str(..)
  , zipWith
  , zipWithAwait
  , zip
  , filter
  )

where

import AsyncRattus
import Prelude hiding (map, const, zipWith, zip, filter)


-- | @Str a@ is a stream of values of type @a@.
data Str a = !a ::: !(O (Str a))

-- all functions in this module are in Asynchronous Rattus 
{-# ANN module AsyncRattus #-}

-- | Get the first element (= head) of a stream.
hd :: Str a -> a
hd (x ::: _) = x


-- | Get the tail of a stream, i.e. the remainder after removing the
-- first element.
tl :: Str a -> O (Str a)
tl (_ ::: xs) = xs

-- | Apply a function to each element of a stream.
map :: Box (a -> b) -> Str a -> Str b
map f (x ::: xs) = unbox f x ::: delay (map f (adv xs))


-- | Apply a function to each element of a stream.
mapAwait :: Box (a -> b) -> O (Str a) -> O (Str b)
mapAwait f d = delay (map f (adv d))

mkSignal :: Box (O a) -> O (Str a)
mkSignal b = delay (adv (unbox b) ::: mkSignal b)


-- Construct a stream which just yields values from a later
fromLater :: (Stable a) => Box (O a) -> O (Str a)
fromLater l = delay (let x = adv (unbox l) in x ::: fromLater l)

-- | Construct a stream that has the given value and then never ticks.
-- | From the Async RaTT paper
const :: a -> Str a
const x = x ::: never

-- | Construct a stream by repeatedly applying a function to a given
-- start element. That is, @unfold (box f) x@ will produce the stream
-- @x ::: f x ::: f (f x) ::: ...@
-- unfold :: Stable a => Box (a -> a) -> a -> Str a
-- unfold f x = x ::: delay (unfold f (unbox f x))

-- | Similar to Haskell's 'scanl'.
--
-- > scan (box f) x (v1 ::: v2 ::: v3 ::: ... ) == (x `f` v1) ::: ((x `f` v1) `f` v2) ::: ...
--
-- Note: Unlike 'scanl', 'scan' starts with @x `f` v1@, not @x@.

scan :: (Stable b) => Box(b -> a -> b) -> b -> Str a -> Str b
scan f acc (a ::: as) = acc' ::: delay (scan f acc' (adv as))
  where acc' = unbox f acc a

-- Like scan, but uses a delayed stream. Taken from the Async RaTT paper.
scanAwait :: (Stable b) => Box (b -> a -> b) -> b -> O (Str a) -> Str b
scanAwait f acc as = acc ::: delay (scan f acc (adv as))

-- | 'scanMap' is a composition of 'map' and 'scan':
--
-- > scanMap f g x === map g . scan f x
scanMap :: (Stable b) => Box (b -> a -> b) -> Box (b -> c) -> b -> Str a -> Str c
scanMap f p acc (a ::: as) =  unbox p acc' ::: delay (scanMap f p acc' (adv as))
  where acc' = unbox f acc a


switch :: Str a -> O (Str a) -> Str a
switch (x ::: xs) d = x ::: delay (case select xs d of
                                     Fst   xs'  d'  -> switch xs' d'
                                     Snd   _    d'  -> d'
                                     Both  _    d'  -> d')

switchS :: Stable a => Str a -> O (a -> Str a) -> Str a
switchS (x ::: xs) d = x ::: delay (case select xs d of
                                     Fst   xs'  d'  -> switchS xs' d'
                                     Snd   _    f  -> f x
                                     Both  _    f  -> f x)

switchAwait :: O (Str a) -> O (Str a) -> O (Str a)
switchAwait xs ys = delay (case select xs ys of
                                  Fst  xs'  d'  -> switch xs' d'
                                  Snd  _    d'  -> d'
                                  Both _    d'  -> d')


interleave :: Box (a -> a -> a) -> O (Str a) -> O (Str a) -> O (Str a)
interleave f xs ys = delay (case select xs ys of
                              Fst (x ::: xs') ys' -> x ::: interleave f xs' ys'
                              Snd xs' (y ::: ys') -> y ::: interleave f xs' ys'
                              Both (x ::: xs') (y ::: ys') -> unbox f x y ::: interleave f xs' ys')


-- | Similar to 'Prelude.zipWith' on Haskell lists.
-- | Inspired by 'zip' in the Async RaTT paper.
zipWith :: (Stable a, Stable b) => Box(a -> b -> c) -> Str a -> Str b -> Str c
zipWith f (a ::: as) (b ::: bs) = unbox f a b ::: delay (
    case select as bs of
      Fst as' lbs -> zipWith f as' (b ::: lbs)
      Snd las bs' -> zipWith f (a ::: las) bs'
      Both as' bs' -> zipWith f as' bs'
  )

zipWithAwait :: (Stable a, Stable b) => Box(a -> b -> c) -> O (Str a) -> O (Str b) -> a -> b -> Str c
zipWithAwait f as bs defaultA defaultB = unbox f defaultA defaultB :::  delay (
    case select as bs of
      Both as' bs' -> zipWith f as' bs'
      Fst as' lbs -> zipWith f as' (defaultB ::: lbs)
      Snd las bs' -> zipWith f (defaultA ::: las) bs'
  )

zip :: (Stable a, Stable b) => Str a -> Str b -> Str (a:*b)
zip = zipWith (box (:*))

-- | Filter out elements from a stream according to a predicate.
filter :: Box(a -> Bool) -> Str a -> Str (Maybe' a)
filter p = map (box (\a -> if unbox p a then Just' a else Nothing'))

{-| Given a value a and a stream as, this function produces a stream
  that behaves like -}
shift :: Stable a => a -> Str a -> Str a
shift a (x ::: xs) = a ::: delay (shift x (adv xs))


{-| Given a list @[a1, ..., an]@ of elements and a stream @xs@ this
  function constructs a stream that starts with the elements @a1, ...,
  an@, and then proceeds as @xs@. In particular, this means that the
  ith element of the original stream @xs@ is the (i+n)th element of
  the new stream. In other words @shiftMany@ behaves like repeatedly
  applying @shift@ for each element in the list. -}
shiftMany :: Stable a => List a -> Str a -> Str a
shiftMany l xs = run l Nil xs where
  run :: Stable a => List a -> List a -> Str a -> Str a
  run (b :! bs) buf (x ::: xs) = b ::: delay (run bs (x :! buf) (adv xs))
  run Nil buf (x ::: xs) =
    case reverse' buf of
      b :! bs -> b ::: delay (run bs (x :! Nil) (adv xs))
      Nil -> x ::: xs
    

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
