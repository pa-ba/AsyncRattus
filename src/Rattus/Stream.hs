{-# OPTIONS -fplugin=Rattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
-- | Programming with streams.

module Rattus.Stream
  ( map
  , mapL
  , hd
  , tl
  , fromLater
--  , constBox
  , shift
  , shiftMany
  , scan
  , scanAwait
  , scanMap
--  , scanMap2
  , Str(..)
  , zipWith
  , zipWithAwait
  , zip
--  , unfold
  , filter
--  , integral
  )

where

import Rattus
import Prelude hiding (map, const, zipWith, zip, filter, Left, Right)

import Data.VectorSpace

-- | @Str a@ is a stream of values of type @a@.
data Str v a = !a ::: !(O v (Str v a))

-- all functions in this module are in Rattus 
{-# ANN module Rattus #-}

-- | Get the first element (= head) of a stream.
hd :: Str v a -> a
hd (x ::: _) = x


-- | Get the tail of a stream, i.e. the remainder after removing the
-- first element.
tl :: Str v a -> O v (Str v a)
tl (_ ::: xs) = xs

-- | Apply a function to each element of a stream.
map :: Box (a -> b) -> Str v a -> Str v b
map f (x ::: xs) = unbox f x ::: delay (map f (adv xs))

mapL :: Box (a -> b) -> O v (Str v a) -> O v (Str v b)
mapL f s = delay (map f (adv s))

-- Construct a stream which just yields values from a later
fromLater :: (Stable a) => Box (O v a) -> O v (Str v a)
fromLater l = delay (let x = adv (unbox l) in x ::: fromLater l)

-- | Construct a stream that has the given value and then never ticks.
-- | From the Async RaTT paper
const :: Stable a => a -> Str v a
const x = x ::: never

-- | Variant of 'const' that allows any type @a@ as argument as long
-- as it is boxed.
constBox :: Box a -> Str v a
constBox a = unbox a ::: never

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
scan :: (Stable b) => Box(b -> a -> b) -> b -> Str v a -> Str v b
scan f acc (a ::: as) = acc' ::: delay (scan f acc' (adv as))
  where acc' = unbox f acc a

-- Like scan, but uses a delayed stream. Taken from the Async RaTT paper.
scanAwait :: (Stable b) => Box (b -> a -> b) -> b -> O v (Str v a) -> Str v b
scanAwait f acc as = acc ::: delay (scan f acc (adv as))

-- | 'scanMap' is a composition of 'map' and 'scan':
--
-- > scanMap f g x === map g . scan f x
scanMap :: (Stable b) => Box (b -> a -> b) -> Box (b -> c) -> b -> Str v a -> Str v c
scanMap f p acc (a ::: as) =  unbox p acc' ::: delay (scanMap f p acc' (adv as))
  where acc' = unbox f acc a


-- | 'scanMap2' is similar to 'scanMap' but takes two input streams.
{-
scanMap2 :: (Stable b) => Box(b -> a1 -> a2 -> b) -> Box (b -> c) -> b -> Str a1 -> Str a2 -> Str c
scanMap2 f p acc (a1 ::: as1) (a2 ::: as2) =
    unbox p acc' ::: delay (scanMap2 f p acc' (adv as1) (adv as2))
  where acc' = unbox f acc a1 a2
-}

-- | Similar to 'Prelude.zipWith' on Haskell lists.
-- | Inspired by 'zip' in the Async RaTT paper.
zipWith :: (Stable a, Stable b) => Box(a -> b -> c) -> Str v a -> Str v b -> Str v c
zipWith f (a ::: as) (b ::: bs) = unbox f a b ::: delay (
    case select as bs of
      Left as' lbs -> zipWith f as' (b ::: lbs)
      Right las bs' -> zipWith f (a ::: las) bs'
      Both as' bs' -> zipWith f as' bs'
  )

zipWithAwait :: (Stable a, Stable b) => Box(a -> b -> c) -> O v (Str v a) -> O v (Str v b) -> a -> b -> Str v c
zipWithAwait f as bs defaultA defaultB = unbox f defaultA defaultB :::  delay (
    case select as bs of
      Both as' bs' -> zipWith f as' bs'
      Left as' lbs -> zipWith f as' (defaultB ::: lbs)
      Right las bs' -> zipWith f (defaultA ::: las) bs'
  )

-- | Combines two signals by picking the most recent value from each.
-- | From the Async RaTT paper.
zip :: (Stable a, Stable b) => Str v a -> Str v b -> Str v (a:*b)
zip = zipWith (box (:*))

-- | Filter out elements from a stream according to a predicate.
filter :: Box(a -> Bool) -> Str v a -> Str v (Maybe' a)
filter p = map (box (\a -> if unbox p a then Just' a else Nothing'))

{-| Given a value a and a stream as, this function produces a stream
  that behaves like -}
shift :: Stable a => a -> Str v a -> Str v a
shift a (x ::: xs) = a ::: delay (shift x (adv xs))


{-| Given a list @[a1, ..., an]@ of elements and a stream @xs@ this
  function constructs a stream that starts with the elements @a1, ...,
  an@, and then proceeds as @xs@. In particular, this means that the
  ith element of the original stream @xs@ is the (i+n)th element of
  the new stream. In other words @shiftMany@ behaves like repeatedly
  applying @shift@ for each element in the list. -}
shiftMany :: Stable a => List a -> Str v a -> Str v a
shiftMany l xs = run l Nil xs where
  run :: Stable a => List a -> List a -> Str v a -> Str v a
  run (b :! bs) buf (x ::: xs) = b ::: delay (run bs (x :! buf) (adv xs))
  run Nil buf (x ::: xs) =
    case reverse' buf of
      b :! bs -> b ::: delay (run bs (x :! Nil) (adv xs))
      Nil -> x ::: xs
    
-- | Calculates an approximation of an integral of the stream of type
-- @Str a@ (the y-axis), where the stream of type @Str s@ provides the
-- distance between measurements (i.e. the distance along the y axis).
{-
integral :: (Stable a, VectorSpace a s) => a -> Str s -> Str a -> Str a
integral acc (t ::: ts) (a ::: as) = acc' ::: delay (integral acc' (adv ts) (adv as))
  where acc' = acc ^+^ (t *^ a)
-}

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

{-# NOINLINE [1] map #-}
{-# NOINLINE [1] const #-}
{-# NOINLINE [1] constBox #-}
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
