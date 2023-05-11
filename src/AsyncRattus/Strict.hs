{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains strict versions of some standard data
-- structures.



module AsyncRattus.Strict
  ( List(..),
    singleton,
    fromList,
    toList,
    init',
    reverse',
    (+++),
    listToMaybe',
    map',
    mapMaybe',
    (:*)(..),
    Maybe'(..),
    maybe',
    fromMaybe',
   fst',
   snd',
  )where

import AsyncRattus.Primitives
import AsyncRattus.Plugin.Annotation
import Prelude hiding (map)

infixr 2 :*
infixr 8 :!

-- | Strict list type.
data List a = Nil | !a :! !(List a)

{-# ANN module AsyncRattus #-}
-- All recursive functions in this module are defined by structural
-- induction on a strict type.
{-# ANN module AllowRecursion #-}

singleton :: a -> List a
singleton x = x :! Nil

fromList :: [a] -> List a
fromList [] = Nil
fromList (x : xs) = x :! fromList xs

toList :: List a -> [a]
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
listToMaybe' = foldr (const . Just') Nothing'

-- | Append two lists.
(+++) :: List a -> List a -> List a
(+++) Nil     ys = ys
(+++) (x:!xs) ys = x :! xs +++ ys


map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (x :! xs) = f x :! map' f xs


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

-- | Strict pair type.
data a :* b = !a :* !b

-- | First projection function.
fst' :: (a :* b) -> a
fst' (a:*_) = a

-- | Second projection function.
snd' :: (a :* b) -> b
snd' (_:*b) = b

instance Functor ((:*) a) where
  fmap f (x:*y) = (x :* f y)
  
instance (Show a, Show b) => Show (a:*b) where
  show (a :* b) = "(" ++ show a ++ " :* " ++ show b ++ ")"

instance (Eq a, Eq b) => Eq (a :* b) where
  (x1 :* y1) == (x2 :* y2) = x1 == x2 && y1 == y2