{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

module PushPull where


import WidgetRattus
import WidgetRattus.Signal (Sig(..))
import qualified WidgetRattus.Signal as Sig
import Prelude hiding (map, const, zipWith, zipWith3, zip, filter)

type Ev a     = O (Sig a)
type Beh a    = Sig (Pull a)

data Pull a  = K !a | Fun !(Box (Time -> a))

mapE :: Box (a -> b) -> Ev a -> Ev b
mapE f xs = delay (let (x ::: xs') = adv xs in unbox f x ::: mapE f xs')

interleave :: Box (a -> a -> a) -> Ev a -> Ev a -> Ev a
interleave f xs ys = 
  delay  (case select xs ys of
                              Fst   (x ::: xs')  ys'          -> x            ::: interleave f xs' ys'
                              Snd   xs'          (y ::: ys')  -> y            ::: interleave f xs' ys'
                              Both  (x ::: xs')  (y ::: ys')  -> unbox f x y  ::: interleave f xs' ys')

scan :: Stable b => Box (b -> a -> b) -> b -> Ev a -> Ev b
scan f acc as = delay (Sig.scan f acc (adv as))

sum :: Ev Int -> Ev Int
sum = scan (box (+)) 0

cont :: Box (Time -> a) -> Beh a
cont f = Fun f ::: never

const :: a -> Beh a
const x = K x ::: never

stepper :: a -> Ev a -> Beh a 
stepper initial ev = (K initial ::: mapE (box K) ev)

mapB :: Box (a -> b) -> Beh a -> Beh b
mapB f (x ::: xs) = mapP f x ::: delay (mapB f (adv xs))

mapP :: Box (a -> b) -> Pull a -> Pull b 
mapP f (K a)    = K (unbox f a) 
mapP f (Fun t)  = Fun (box (unbox f . unbox t))

mapP2 :: (Stable a, Stable b) => Box (a -> b -> c) -> Pull a -> Pull b -> Pull c
mapP2 f (K x)    (K y)    = K    (unbox f x y)
mapP2 f (Fun x)  (Fun y)  = Fun  (box (\ t -> unbox f (unbox x t) (unbox y t)))
mapP2 f (Fun x)  (K y)    = Fun  (box (\ t -> unbox f (unbox x t) y))
mapP2 f (K x)    (Fun y)  = Fun  (box (unbox f x . unbox y))

zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Beh a -> Beh b -> Beh c
zipWith f (x ::: xs) (y ::: ys) =  
  mapP2 f x y ::: delay  (case select xs ys of 
                            Fst   xs'  lys  -> zipWith f xs'          (y ::: lys)
                            Snd   lxs  ys'  -> zipWith f (x ::: lxs)  ys'
                            Both  xs'  ys'  -> zipWith f xs'          ys')

switch :: Beh a -> O (Beh a) -> Beh a 
switch (x ::: xs) d = x ::: delay  (case select xs d of
                                      Fst   xs'  d'  -> switch xs' d'
                                      Snd   _    d'  -> d'
                                      Both  _    d'  -> d')

at :: Pull b -> Time -> b
at (K x)   _  =  x
at (Fun f) t  = unbox f t

switchS :: Stable a => Beh a -> O (a -> Beh a) -> Beh a 
switchS (x ::: xs) d = x ::: withTime (delay (
  case select xs d of
    Fst   xs'  d'  -> \_ -> switchS xs' d'
    Snd   _    f   -> \t -> f (x `at` t)
    Both  _    f   -> \t -> f (x `at` t)))

switchR :: Stable a => Beh a -> Ev (a -> Beh a) -> Beh a 
switchR (x ::: xs) ev = x ::: withTime (delay (
  case select xs ev of
    Fst   xs'  ev'          -> \_ -> switchR xs' ev'
    Snd   _    (f ::: ev')  -> \t -> switchR (f (x `at` t)) ev'
    Both  _    (f ::: ev')  -> \t -> switchR (f (x `at` t)) ev'))

filter :: Box (a -> Bool) -> Ev a -> Ev (Maybe' a)
filter p = mapE (box (\ x -> if unbox p x then Just' x else Nothing'))


sample :: Stable b => Box (a -> b -> c) -> Ev a -> Beh b -> Ev (Maybe' c)
sample f ev (x ::: xs) = run x ev xs where
  run x ev xs = withTime $ delay (case select ev xs of 
    Fst (e ::: ev') xs' -> \t -> Just' (unbox f e (x `at` t)) ::: run x ev' xs'
    Snd ev' (x' ::: xs') -> \_ -> Nothing' ::: run x' ev' xs'
    Both (e ::: ev') (x' ::: xs') -> \t -> Just' (unbox f e (x' `at` t)) ::: run x' ev' xs')