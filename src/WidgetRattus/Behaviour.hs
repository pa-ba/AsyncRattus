{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module WidgetRattus.Behaviour where

import WidgetRattus
import WidgetRattus.InternalPrimitives (Continuous (..), O (Delay), adv', advC', clockUnion, inputInClock)
import WidgetRattus.Signal hiding (const, integral, jump, map, switch, zipWith)
import Prelude hiding (const, map, zipWith)

data Pull a where
  K :: !a -> Pull a
  Fun :: (Stable s) => !s -> !(Box (s -> Time -> (a :* Maybe' s))) -> Pull a

continuous ''Pull

at :: Pull a -> Time -> a
at (K a) _ =  a
at (Fun s f) t = let (a :* _) = unbox f s t in a

mapP :: Box (a -> b) -> Pull a -> Pull b
mapP f (K a) = K (unbox f a)
mapP f (Fun s f') = Fun s (box (\s t -> let (a :* s') = unbox f' s t in (unbox f a :* s')))

delayCF :: O(a -> C b) -> O(a -> b)
delayCF (Delay c f) = Delay c (\inp a -> advC' (f inp a) inp)

newtype Beh a = Beh (Sig (Pull a))

unwrap :: Beh a -> Sig (Pull a)
unwrap (Beh a) = a

cont :: Box (Time -> a) -> Beh a
cont f = Beh (Fun () (box (\ _ t -> (unbox f t :* Just' ()))) ::: never)

const :: a -> Beh a
const x = Beh (K x ::: never)

timeB :: Beh Time
timeB = cont (box id)

mapB :: Box (a -> b) -> Beh a -> Beh b
mapB f (Beh (x ::: xs)) = Beh (mapP f x ::: delay (unwrap $ mapB f (Beh (adv xs))))

sampleInterval :: O ()
sampleInterval = timer 20000

discretize :: Beh a -> C (Sig a)
discretize b = discretizeT b <$> time

discretizeT :: Beh a -> Time -> Sig a
discretizeT (Beh (K x ::: xs)) _ = x ::: withTime (delay (discretizeT (Beh (adv xs))))
discretizeT (Beh (Fun s f ::: xs)) t = discretizeFun s f xs t
  where
    discretizeFun :: (Stable s) => s -> Box (s -> Time -> (a :* Maybe' s)) -> O (Sig (Pull a)) -> Time -> Sig a
    discretizeFun s f xs t = cur ::: rest where
      (cur :* s') = unbox f s t
      rest = case s' of 
               Nothing' -> withTime (delay (discretizeT (Beh (adv xs))))
               Just' s'' -> withTime $ delay
                    ( case select xs sampleInterval of
                        Fst  x     _ -> discretizeT (Beh x)
                        Snd  beh'  _ -> discretizeT (Beh (Fun s'' f ::: beh'))
                        Both x     _ -> discretizeT (Beh x)
                    )
              


elapsedTime :: C (Beh DTime)
elapsedTime = do
  startTime <- time
  return $ Beh (Fun () (box (\s currentTime -> diffTime currentTime startTime :* Just' s)) ::: never)

switch :: Beh a -> O (Beh a) -> Beh a
switch (Beh (x ::: xs)) d =
  Beh $
    x
      ::: delay
        ( case select xs d of
            Fst xs' d' -> unwrap $ switch (Beh xs') d'
            Snd _ (Beh d') -> d'
            Both _ (Beh d') -> d'
        )

-- | This function is a variant of combines the values of two signals
-- using the function argument. @zipWith f xs ys@ produces a new value
-- @unbox f x y@ whenever @xs@ or @ys@ produce a new value, where @x@
-- and @y@ are the current values of @xs@ and @ys@, respectively.
--
-- Example:
--
-- >                      xs:  1 2 3     2
-- >                      ys:  1     0 5 2
-- > zipWith (box (+)) xs ys:  2 3 4 3 8 4
zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Beh a -> Beh b -> Beh c
zipWith f (Beh (x ::: xs)) (Beh (y ::: ys)) =
  Beh
    ( app x y
        ::: delay
          ( let (Beh rest) =
                  ( case select xs ys of
                      Fst xs' lys -> zipWith f (Beh xs') (Beh (y ::: lys))
                      Snd lxs ys' -> zipWith f (Beh (x ::: lxs)) (Beh ys')
                      Both xs' ys' -> zipWith f (Beh xs') (Beh ys')
                  )
             in rest
          )
    )
  where
    app (K x') (K y') = K (unbox f x' y')
    app (Fun xs x') (Fun ys y') =
      Fun
        (xs :* ys)
        ( box
            ( \(s :* s') t ->
                let (a :* xs') = unbox x' s t
                    (b :* ys') = unbox y' s' t
                    left = unbox f a b
                 in case (xs' :* ys') of
                      (Just' xs'' :* Just' ys'') -> left :* Just' (xs'' :* ys'')
                      _ -> left :* Nothing'
            )
        )
    app (Fun xs x') (K y') =
      Fun
        xs
        ( box
            ( \s t ->
                let (a :* xs') = unbox x' s t
                    left = unbox f a y'
                 in left :* xs'
            )
        )
    app (K x') (Fun ys y') =
      Fun
        ys
        ( box
            ( \s t ->
                let (b :* ys') = unbox y' s t
                    left = unbox f x' b
                 in left :* ys'
            )
        )

-- | Variant of 'zipWith' with three behaviours.
zipWith3 :: forall a b c d. (Stable a, Stable b, Stable c) => Box (a -> b -> c -> d) -> Beh a -> Beh b -> Beh c -> Beh d
zipWith3 f as bs cs = zipWith (box (\f' x -> unbox f' x)) cds cs
  where
    cds :: Beh (Box (c -> d))
    cds = zipWith (box (\a b -> box (\c -> unbox f a b c))) as bs

stop :: Box (a -> Bool) -> Beh a -> Beh a
stop p (Beh b) = Beh (run b)
  where
    run (K x ::: xs) = K x ::: if unbox p x then never else delay (run (adv xs))
    run (Fun s f ::: xs) =
      Fun
        s
        ( box
            ( \s' t ->
                let (a :* s'') = unbox f s' t
                 in let b = unbox p a
                     in (if b then a :* Nothing' else a :* s'')
            )
        )
        ::: delay (run (adv xs))

stopWith :: Box (a -> Maybe' a) -> Beh a -> Beh a
stopWith p (Beh b) = Beh (run b)
  where
    run (K x ::: xs) =
      case unbox p x of
        Just' a -> K a ::: never
        Nothing' -> K x ::: delay (run (adv xs))
    run (Fun s f ::: xs) =
      Fun
        s
        ( box
            ( \s' t ->
                let (a :* s'') = unbox f s' t
                 in case unbox p a of
                      Just' a' -> a' :* Nothing'
                      Nothing' -> a :* s''
            )
        )
        ::: delay (run (adv xs))

integral ::  Float  -> Beh Float -> C (Beh Float)
integral cur (Beh xs) = Beh <$> int xs cur <$> time where
  int :: Sig (Pull Float) -> Float -> Time -> Sig (Pull Float)
  int (K a ::: xs) cur t = curF ::: rest where
    rest = withTime ( delay (\t' -> int (adv xs) (cur + a * (t' <-> t)) t'))
    curF = Fun () (box (\s t' -> cur + a * (t' <-> t) :* Just' s))

  int (Fun s f ::: xs) cur t = intFun s f xs cur t where
    intFun :: forall s. (Stable s) => s -> Box (s -> Time -> (Float :* Maybe' s)) -> O (Sig (Pull Float)) -> Float -> Time -> Sig (Pull Float)
    intFun s f xs cur t = curF ::: rest where
      rest = withTime (delay (\ t'-> int (adv xs) (cur + (fst' (unbox f s t')) * (t' <-> t)) t'))
      curF = Fun (cur :* t :* Left' s)
                       (box ( \(lv :* lt :* ls) t' -> 
                          let v :* s' = case ls of Right' v  -> v :* Right' v
                                                   Left' ls' -> case unbox f ls' t' of 
                                                              v :* Nothing' -> v :* Right' v
                                                              v :* Just' s -> v :* Left' s
                          in lv + v * (t' <-> lt) :* Just' (lv + v * (t' <-> lt) :* t' :* s')))



derivative :: Beh Float -> C (Beh Float)
derivative (Beh (x ::: xs)) = (\t ->  Beh (der (at x t) (x ::: xs) t)) <$> time where
    der :: Float -> Sig (Pull Float) -> Time -> Sig (Pull Float)
    der last (Fun s f ::: xs) t = derFun last s f xs t
      where
        derFun :: forall s. (Stable s) => Float -> s -> Box (s -> Time -> (Float :* Maybe' s)) -> O (Sig (Pull Float)) -> Time -> (Sig (Pull Float))
        derFun last s f xs t = curF ::: rest where
          rest = withTime ( delay (\ t' -> der (fst' (unbox f s t')) (adv xs) t'))
          curF =
                Fun (last :* t :* s) $ box
                    ( \(last :* t :* s) t' -> case unbox f s t of
                              v :* Just' s' -> (v - last) / (t' <-> t) :* Just' (v :* t' :* s')
                              _ :* Nothing' -> 0 :* Nothing')
    der last (K x ::: xs) t = curF ::: rest where
      rest = withTime (delay (der x (adv xs)))
      curF = Fun (last :* t) $ box (\(last :* t) t' -> (x - last) / (t' <-> t) :* Just' (x :* t'))

instance (Continuous a) => Continuous (Beh a) where
  progressInternal inp (Beh (x ::: xs@(Delay cl _))) =
    if inputInClock inp cl
      then Beh (adv' xs inp)
      else progressInternal inp (Beh (x ::: xs))
  progressAndNext inp (Beh (x ::: xs@(Delay cl _))) =
    if inputInClock inp cl
      then let n = adv' xs inp in (Beh n, nextProgress n)
      else let (n, cl') = progressAndNext inp x in (Beh (n ::: xs), cl `clockUnion` cl')
  nextProgress (Beh (x ::: (Delay cl _))) = nextProgress x `clockUnion` cl

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

{-# NOINLINE [1] mapB #-}

{-# NOINLINE [1] const #-}

{-# NOINLINE [1] switch #-}

{-# RULES
"beh.map/beh.map" forall f g xs.
  mapB f (mapB g xs) =
    mapB (box (unbox f . unbox g)) xs
"beh.const/beh.map" forall (f :: (Stable b) => Box (a -> b)) x.
  mapB f (const x) =
    let x' = unbox f x in const x'
"beh.const/beh.switch" forall x xs.
  switch (const x) xs =
    Beh (K x ::: delay (unwrap (adv xs)))
  #-}