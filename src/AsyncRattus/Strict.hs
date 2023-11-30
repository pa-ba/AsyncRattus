{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}


-- | This module contains strict versions of some standard data
-- structures.



module AsyncRattus.Strict
  ( List(..),
    singleton,
    IsList(..),
    init',
    reverse',
    (+++),
    listToMaybe',
    map',
    zip',
    zipWith',
    mapMaybe',
    (:*)(..),
    Maybe'(..),
    maybe',
    fromMaybe',
    fst',
    snd',
    curry',
    uncurry'
  )where

import Prelude hiding (map)
import Data.VectorSpace
import AsyncRattus.Derive
import GHC.Exts

infixr 2 :*
-- | Strict pair type.
data a :* b = !a :* !b

continuous ''(:*)

-- | First projection function.
fst' :: (a :* b) -> a
fst' (a:*_) = a

-- | Second projection function.
snd' :: (a :* b) -> b
snd' (_:*b) = b

curry' :: ((a :* b) -> c) -> a -> b -> c
curry' f x y = f (x :* y)

uncurry' :: (a -> b -> c) -> (a :* b) -> c
uncurry' f (x :* y) = f x y


instance Functor ((:*) a) where
  fmap f (x:*y) = (x :* f y)
  
instance (Show a, Show b) => Show (a:*b) where
  show (a :* b) = "(" ++ show a ++ " :* " ++ show b ++ ")"

instance (Eq a, Eq b) => Eq (a :* b) where
  (x1 :* y1) == (x2 :* y2) = x1 == x2 && y1 == y2


instance (VectorSpace v a, VectorSpace w a, Floating a, Eq a) => VectorSpace (v :* w) a where
  zeroVector = zeroVector :* zeroVector

  a *^ (x :* y) = (a *^ x) :* (a *^ y)

  (x :* y) ^/ a = (x ^/ a) :* (y ^/ a)

  negateVector (x :* y) = (negateVector x) :* (negateVector y)

  (x1 :* y1) ^+^ (x2 :* y2) = (x1 ^+^ x2) :* (y1 ^+^ y2)

  (x1 :* y1) ^-^ (x2 :* y2) = (x1 ^-^ x2) :* (y1 ^-^ y2)

  (x1 :* y1) `dot` (x2 :* y2) = (x1 `dot` x2) + (y1 `dot` y2)

infixr 8 :!

-- | Strict list type.
data List a = Nil | !a :! !(List a)

continuous ''List


singleton :: a -> List a
singleton x = x :! Nil



instance IsList (List a) where
  type Item (List a) = a

  fromList [] = Nil
  fromList (x : xs) = x :! fromList xs

  toList Nil = []
  toList (x :! xs) = x : toList xs


-- | Remove the last element from a list if there is one, otherwise
-- return 'Nil'.
init' :: List a -> List a
init' Nil = Nil
init' (_ :! Nil) = Nil
init' (x :! xs) = x :! init' xs

-- | Reverse a list.
reverse' :: List a -> List a
reverse' l =  rev l Nil
  where
    rev Nil     a = a
    rev (x:!xs) a = rev xs (x:!a)
    
-- | Returns @'Nothing''@ on an empty list or @'Just'' a@ where @a@ is the
-- first element of the list.
listToMaybe' :: List a -> Maybe' a
listToMaybe' Nil = Nothing'
listToMaybe' (x :! _) = Just' x

-- | Append two lists.
(+++) :: List a -> List a -> List a
(+++) Nil     ys = ys
(+++) (x:!xs) ys = x :! xs +++ ys


map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (x :! xs) = f x :! map' f xs

zip' :: List a -> List b -> List (a :* b)
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (x :! xs) (y :! ys) = (x :* y) :! zip' xs ys

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (x :! xs) (y :! ys) = f x y :! zipWith' f xs ys


-- | A version of 'map' which can throw out elements.  In particular,
-- the function argument returns something of type @'Maybe'' b@.  If
-- this is 'Nothing'', no element is added on to the result list.  If
-- it is @'Just'' b@, then @b@ is included in the result list.
mapMaybe'          :: (a -> Maybe' b) -> List a -> List b
mapMaybe' _ Nil     = Nil
mapMaybe' f (x:!xs) =
 let rs = mapMaybe' f xs in
 case f x of
  Nothing' -> rs
  Just' r  -> r:!rs

instance Foldable List where
  
  foldMap f = run where
    run Nil = mempty
    run (x :! xs) = f x <> run xs
  foldr f = run where
    run b Nil = b
    run b (a :! as) = (run $! (f a b)) as
  foldl f = run where
    run a Nil = a
    run a (b :! bs) = (run $! (f a b)) bs
  elem a = run where
    run Nil = False
    run (x :! xs)
      | a == x = True
      | otherwise = run xs
    
  
instance Functor List where
  fmap = map'

instance Eq a => Eq (List a) where
  Nil == Nil = True
  Nil == _ = False
  _ == Nil = False
  (x :! xs) == (y :! ys) = if x == y then xs == ys else False

instance Show a => Show (List a) where
  show Nil = "Nil"
  show (x :! xs) = show x ++ " :! " ++ show xs

-- | Strict variant of 'Maybe'.
data Maybe' a = Just' !a | Nothing'

continuous ''Maybe'

instance Eq a => Eq (Maybe' a) where
  Nothing' == Nothing' = True
  Just' x == Just' y = x == y
  _ == _ = False

instance Show a => Show (Maybe' a) where
  show Nothing' = "Nothing'"
  show (Just' x) = "Just' " ++ show x

-- | takes a default value, a function, and a 'Maybe'' value.  If the
-- 'Maybe'' value is 'Nothing'', the function returns the default
-- value.  Otherwise, it applies the function to the value inside the
-- 'Just'' and returns the result.
maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' n _ Nothing'  = n
maybe' _ f (Just' x) = f x

fromMaybe' :: a -> Maybe' a -> a
fromMaybe' _ (Just' x) = x
fromMaybe' d Nothing' = d

