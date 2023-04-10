{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fplugin=Rattus.Plugin #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RebindableSyntax #-}

-- | A simply Yampa-like library for signal functions.

module Rattus.Yampa (
  SF,
  identity,
  compose,
  switch,
  rSwitch,
  constant,
  loopPre,
  stepSF,
  initially,
  integral,
  (-->),
  (-:>),
  (>--),
  (-=>),
  (>=-),
  (^>>),
  (>>^),
  (<<^),
  (^<<),
  module Rattus.Arrow, (>>>)) where

import Rattus.Primitives
import Rattus.Plugin
import Rattus.Strict

import GHC.Float
import Control.Category
import Rattus.Arrow
import Prelude hiding (id)

import Data.VectorSpace

-- all functions in this module are in Rattus 
--{-# ANN module Rattus #-}

type DTime = Double 

-- | Signal functions from inputs of type @a@ to outputs of type @b@.
data SF a b = SF{
  -- | Run a signal function for one step.
  stepSF :: !(DTime -> a -> (O(SF a b) :* b))}

-- | The identity signal function that does nothing.
identity :: SF a a
identity = SF (\ _ x -> (delay identity :* x))

-- | Compose two signal functions.
compose :: SF b c -> SF a b -> SF a c
compose (SF sf2) (SF sf1) = SF sf
  where sf d a = let (r1 :* b) = sf1 d a
                     (r2 :* c) = sf2 d b
                 in (delay (compose (adv r2) (adv r1)) :* c)

-- | Compute the integral of a signal. The first argument is the
-- offset.
integral :: (Stable a, VectorSpace a s, Fractional s) => a -> SF a a
integral acc = SF sf'
  where sf' t a = let acc' = acc ^+^ (realToFrac t *^ a)
                  in (delay (integral acc') :* acc')

-- | @switch s f@ behaves like @s@ composed with @arr fst@ until @s@
-- produces a value of the form @Just' c@ in the second
-- component. From then on it behaves like $f c@.
switch :: SF a (b :* Maybe' c) -> Box (c -> SF a b) -> SF a b
switch (SF sf) f = SF sf'
  where sf' t a = let (nxt :* (b :* c')) =  sf t a
                  in case c' of
                       Just' c -> stepSF (unbox f c) t a
                       Nothing' -> (delay (switch (adv nxt) f):* b)

-- | @rSwitch s@ behaves like @s@, but every time the second input is
-- of the form @Just' s'@ it will change behaviour to @s'@.
rSwitch :: SF a b -> SF (a :* Maybe' (SF a b)) b
rSwitch (SF sf) = SF sf'
  where sf' t (a :* m) = case m of
                        Just' (SF newSf) ->
                           let (nxt :* b) = newSf t a
                           in (delay (rSwitch (adv nxt)) :* b)
                        Nothing' -> let (nxt :* b) = sf t a
                                   in (delay (rSwitch (adv nxt)) :* b)

-- | Constant signal function.
constant :: Stable b => b -> SF a b
constant x = run
  where run = SF (\ _ _ -> (delay run :* x))

-- | The output at time zero is the first argument, and from that
-- point on it behaves like the signal function passed as second
-- argument.
(-->) :: b -> SF a b -> SF a b
b --> (SF sf) = SF sf'
  where sf' d x = (fst' (sf d x) :* b)

-- | Insert a sample in the output, and from that point on, behave
-- like the given signal function.
--
-- Note: The type of -:> is different from Yampa's: The second
-- argument must be delayed (or boxed).
(-:>) :: b -> O (SF a b) -> SF a b
b -:> sf = SF sf'
  where sf' _d _x = (sf :* b)

-- | The input at time zero is the first argument, and from that point
-- on it behaves like the signal function passed as second argument.
(>--) :: a -> SF a b -> SF a b
a >-- (SF sf) = SF sf'
  where sf' d _a = sf d a


-- | Apply a function to the first output value at time
-- zero.
(-=>) :: (b -> b) -> SF a b -> SF a b
f -=> (SF sf) = SF sf'
  where sf' d a = let (r:*b) = sf d a
                  in (r:*f b)
                     
-- | Apply a function to the first input value at time
-- zero.
(>=-) :: (a -> a) -> SF a b -> SF a b
f >=- (SF sf) = SF sf'
  where sf' d a = sf d (f a)

-- | Override initial value of input signal.

initially :: a -> SF a a
initially = (--> identity)

-- | Lift a function to a signal function (applied pointwise).
-- 
-- Note: The type of is different from Yampa's: The function argument
-- must be boxed.
arrPrim :: Box (a -> b) -> SF a b
arrPrim f = run where
  run = SF (\ _d a -> (delay run:* unbox f a ))


{-# ANN firstPrim AllowLazyData #-}
-- | Apply a signal function to the first component.
firstPrim :: SF a b -> SF (a,c) (b,c)
firstPrim (SF sf) = SF sf'
  where sf' d (a,c) = let (r:* b) = sf d a
                       in (delay (firstPrim (adv r)):* (b,c))

{-# ANN secondPrim AllowLazyData #-}
-- | Apply a signal function to the second component.
secondPrim :: SF a b -> SF (c,a) (c,b)
secondPrim (SF sf) = SF sf'
  where sf' d (c,a) = let (r:* b) = sf d a
                       in (delay (secondPrim (adv r)):* (c,b))

{-# ANN parSplitPrim AllowLazyData #-}
-- | Apply two signal functions in parallel.
parSplitPrim :: SF a b -> SF c d  -> SF (a,c) (b,d)
parSplitPrim (SF sf1) (SF sf2) = SF sf'
  where sf' dt (a,c) = let (r1:* b) = sf1 dt a
                           (r2:* d) = sf2 dt c
                       in (delay (parSplitPrim (adv r1) (adv r2)):* (b,d))

{-# ANN parFanOutPrim AllowLazyData #-}
-- | Apply two signal functions in parallel on the same input.
parFanOutPrim :: SF a b -> SF a c -> SF a (b, c)
parFanOutPrim (SF sf1) (SF sf2) = SF sf'
  where sf' dt a = let (r1:* b) = sf1 dt a
                       (r2:* c) = sf2 dt a
                   in (delay (parFanOutPrim (adv r1) (adv r2)):* (b,c))

instance Category SF where
  id = identity
  (.) = compose

instance Arrow SF where
  arrBox = arrPrim
  first = firstPrim
  second = secondPrim
  (***) = parSplitPrim
  (&&&) = parFanOutPrim


-- | Loop with an initial value for the signal being fed back.
-- 
-- Note: The type of @loopPre@ is different from Yampa's as we need
-- the @O@ type here.
loopPre :: c -> SF (a:*c) (b:*O c) -> SF a b
loopPre c (SF sf) = SF sf'
  where sf' d a = let (r:* (b:*c')) = sf d (a:*c)
                  in (delay (loopPre (adv c') (adv r)):* b)


-- | Precomposition with a pure function.
(^>>) :: Arrow a => Box (b -> c) -> a c d -> a b d
f ^>> a = arrBox f >>> a

-- | Postcomposition with a pure function.
(>>^) :: Arrow a => a b c -> Box (c -> d) -> a b d
a >>^ f = a >>> arrBox f

-- | Precomposition with a pure function (right-to-left variant).
(<<^) :: Arrow a => a c d -> Box (b -> c) -> a b d
a <<^ f = a <<< arrBox f

-- | Postcomposition with a pure function (right-to-left variant).
(^<<) :: Arrow a => Box (c -> d) -> a b c -> a b d
f ^<< a = arrBox f <<< a
