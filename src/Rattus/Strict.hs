{-# OPTIONS -fplugin=Rattus.Plugin #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains strict versions of some standard data
-- structures.



module Rattus.Strict
  ( List(..),
    singleton,
    init',
    reverse',
    (+++),
    listToMaybe',
    mapMaybe',
    (:*)(..),
    Maybe'(..),
    maybe',
   fst',
   snd',
  )where

import Data.VectorSpace
import Rattus.Primitives
import Rattus.Plugin.Annotation

infixr 2 :*
infixr 8 :!

-- | Strict list type.
data List a = Nil | !a :! !(List a)

singleton :: a -> List a
singleton x = x :! Nil

{-# ANN module Rattus #-}
-- All recursive functions in this module are defined by structural
-- induction on a strict type.
{-# ANN module AllowRecursion #-}

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
  fmap f = run where
    run Nil = Nil
    run (x :! xs) = f x :! run xs


-- | Strict variant of 'Maybe'.
data Maybe' a = Just' !a | Nothing'

-- | takes a default value, a function, and a 'Maybe'' value.  If the
-- 'Maybe'' value is 'Nothing'', the function returns the default
-- value.  Otherwise, it applies the function to the value inside the
-- 'Just'' and returns the result.
maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' n _ Nothing'  = n
maybe' _ f (Just' x) = f x

-- | Strict pair type.
data a :* b = !a :* !b

-- | First projection function.
fst' :: (a :* b) -> a
fst' (a:*_) = a

-- | Second projection function.
snd' :: (a :* b) -> b
snd' (_:*b) = b


instance RealFloat a => VectorSpace (a :* a) a where
    zeroVector = 0 :* 0

    a *^ (x :* y) = (a * x) :* (a * y)

    (x :* y) ^/ a = (x / a) :* (y / a)

    negateVector (x :* y) = (-x) :* (-y)

    (x1 :* y1) ^+^ (x2 :* y2) = (x1 + x2) :* (y1 + y2)

    (x1 :* y1) ^-^ (x2 :* y2) = (x1 - x2) :* (y1 - y2)

    (x1 :* y1) `dot` (x2 :* y2) = x1 * x2 + y1 * y2

instance Functor ((:*) a) where
  fmap f (x:*y) = (x :* f y)
  
instance (Show a, Show b) => Show (a:*b) where
  show (a :* b) = "(" ++ show a ++ " :* " ++ show b ++ ")"
