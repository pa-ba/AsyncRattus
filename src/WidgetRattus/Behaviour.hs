{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module WidgetRattus.Behaviour where

import Data.Ratio
import WidgetRattus
import WidgetRattus.InternalPrimitives (Continuous (..), O (Delay), adv', advC', clockUnion, inputInClock)
import WidgetRattus.Signal (Sig(..))
import Prelude hiding (const, map, zipWith)
import qualified Prelude (const, map, zipWith)


data Fun a where
  K :: !a -> Fun a
  Fun :: !(Box (Time -> (a :* Bool))) -> Fun a

continuous ''Fun

apply :: Fun a -> (Time -> a)
apply (K a) = Prelude.const a
apply (Fun f) = \t -> let (a :* _) = unbox f t in a

mapF :: Box (a -> b) -> Fun a -> Fun b
mapF f (K a) = K (unbox f a)
mapF f (Fun f') = Fun (box (\t -> let (a :* b) = unbox f' t in (unbox f a :* b)))

mapFBool :: Box (a -> Bool) -> Fun a -> Fun a
mapFBool _ (K a) = K a
mapFBool g (Fun f') = Fun (box (\t -> let (a :* _) = unbox f' t in (a :* unbox g a)))

delayCF :: O(a -> C b) -> O(a -> b)
delayCF (Delay c f) = Delay c (\inp a -> advC' (f inp a) inp)

newtype Beh a = Beh (Sig (Fun a))

unwrap :: Beh a -> Sig (Fun a)
unwrap (Beh a) = a

const :: Fun a -> Beh a
const x = Beh (x ::: never)

constK :: a -> Beh a
constK x = Beh (K x ::: never)

timeBehaviour :: Beh Time
timeBehaviour = const (Fun (box (\t -> t :* False)))

map :: forall a b . Box (a -> b) -> Beh a -> Beh b
map f (Beh s) = Beh (run s) where
    run :: Sig (Fun a) -> Sig (Fun b)
    run (x ::: xs) = mapF f x ::: delay (run (adv xs))

sampleInterval :: O ()
sampleInterval = timer 20000

discretize :: Beh a -> C (Sig a)
discretize (Beh sig) = discr sig <$> time
  where
    discr :: Sig (Fun a) -> Time -> Sig a
    discr (K x ::: xs) _ = x ::: withTime (delay (discr (adv xs)))
    discr (Fun f ::: xs) t = cur ::: rest where
      cur :* b = unbox f t
      rest | b = withTime $ delay (discr (adv xs))
           | otherwise = withTime $ delay
                    ( case select xs sampleInterval of
                        Fst x _ -> discr x
                        Snd beh' _ -> discr (Fun f ::: beh')
                        Both x _ -> discr x
                    )

elapsedTime :: C (Beh NominalDiffTime)
elapsedTime = do
  startTime <- time
  return $ Beh (Fun (box (\currentTime -> diffTime currentTime startTime :* False)) ::: never)

withTime :: O (Time -> a) -> O a
withTime delayed =
  delayC $ delay (let f = adv delayed in do f <$> time)

switch :: forall a . Beh a -> O (Beh a) -> Beh a
switch (Beh s) d = Beh (run s d) where
    run :: Sig (Fun a) -> O (Beh a) -> Sig (Fun a)
    run (x ::: xs) d = x ::: delay
        ( case select xs d of
            Fst xs' d' -> run xs' d'
            Snd _  (Beh d') -> d'
            Both _ (Beh d') -> d'
        )


zipFun :: (Stable a, Stable b) => Box (a -> b -> c) -> Fun a -> Fun b -> Fun c
zipFun f (K x') (K y') = K (unbox f x' y')
zipFun f (Fun x') (Fun y') =
    Fun
        ( box
            ( \t ->
                let (a :* ab) = unbox x' t
                    (b :* bb) = unbox y' t
                    left = unbox f a b
                in left :* ab || bb
            )
        )
zipFun f (Fun x') (K y') =
    Fun
        ( box
            ( \t ->
                let (a :* ab) = unbox x' t
                    left = unbox f a y'
                in left :* ab
            )
        )
zipFun f (K x') (Fun y') =
    Fun
        ( box
            ( \t ->
                let (b :* bb) = unbox y' t
                    left = unbox f x' b
                in left :* bb
            )
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
-- >
-- > zipWith (box (+)) xs ys:  2 3 4 3 8 4
zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Beh a -> Beh b -> Beh c
zipWith f (Beh s1) (Beh s2) = Beh (run s1 s2) where
    run (x ::: xs) (y ::: ys) = zipFun f x y ::: delay
          ( case select xs ys of
                Fst xs' lys -> run xs' (y ::: lys)
                Snd lxs ys' -> run (x ::: lxs) ys'
                Both xs' ys' -> run xs' ys'
          ) 
        where


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
    run (Fun f ::: xs) = Fun (box (\t -> let (a :* b) = unbox f t in (a :* (unbox p a || b)))) ::: delay (run (adv xs))

stopWith :: Box (a -> Maybe' a) -> Beh a -> Beh a
stopWith p (Beh b) = Beh (run b)
  where
    run (K x ::: xs) =
      case unbox p x of
        Just' a -> K a ::: never
        Nothing' -> K x ::: delay (run (adv xs))
    run (Fun f ::: xs) =
      Fun
        ( box
            ( \t ->
                let (a :* b) = unbox f t
                 in case unbox p a of
                      Just' a' -> a' :* True
                      Nothing' -> a :* b
            )
        )
        ::: delay (run (adv xs))

dt :: Int
dt = 20000

dtf :: Float
dtf = fromRational (fromIntegral dt % 1000000)

integral :: Float -> Beh Float -> C (Beh Float)
integral cur (Beh s) = Beh <$> run cur s where
    run :: Float -> Sig (Fun Float) -> C (Sig (Fun Float))
    run cur (K x ::: xs) = do
        t <- time
        let rest = delayC $ delay
                    ( do
                        t' <- time
                        let tDiff = diffTime t' t
                        let r = cur + x * fromRational (toRational tDiff)
                        run r (adv xs)
                    )
        let curF = Fun ( box
                    ( \t' ->
                        let tDiff = diffTime t' t
                        in cur + x * fromRational (toRational tDiff) :* False
                    )
                )
        return (curF ::: rest)
    run cur (x ::: xs) = do
        let rest = delayC $ delay
                    ( let d = select xs (timer dt)
                      in do t <- time
                            case d of
                                Fst xs' _ -> run cur xs'
                                Snd xs' _ -> run (cur + apply x t * dtf) (x ::: xs')
                                Both (x' ::: xs') _ -> run (cur + apply x' t * dtf) (x' ::: xs')
                    )

        return (K cur ::: rest)


derivative :: Beh Float -> C (Beh Float)
derivative (Beh (x ::: xs)) = do
  t <- time
  Beh <$> der 0 (apply x t) (x ::: xs)
  where
    der :: Float -> Float -> Sig (Fun Float) -> C (Sig (Fun Float))
    der 0 _ (x ::: xs) =
      do
        t <- time
        return
          ( K 0
              ::: delayC
                ( delay
                    ( let x' ::: xs' = adv xs
                       in do
                            t' <- time
                            der ((apply x' t' - apply x t) / dtf) (apply x t) (x' ::: xs')
                    )
                )
          )
    der d last (x ::: xs) =
      do
        t <- time
        return
          ( K d
              ::: delayC
                ( delay
                    ( let ticker = select xs (timer dt)
                       in do
                            t' <- time
                            case ticker of
                              Fst xs' _ -> der d last xs'
                              Snd xs' _ -> der ((apply x t - last) / dtf) (apply x t) (x ::: xs')
                              Both (x' ::: xs') _ -> der ((apply x' t' - last) / dtf) (apply x' t') (x' ::: xs')
                    )
                )
          )

intergral' :: Float -> Beh Float -> C (Beh Float)
intergral' cur (Beh (x ::: xs)) = do
  t <- time
  let rest =
        delayC
          ( delay
              ( do
                  t' <- time
                  let tDiff = diffTime t' t
                  let dt = fromRational (toRational tDiff)
                  unwrap <$> intergral' (cur + apply x t' * dt) (Beh (adv xs))
              )
          )
  let curF =
        Fun
          ( box
              ( \t' ->
                  let tDiff = diffTime t' t
                      dt = fromRational (toRational tDiff)
                   in cur + apply x t' * dt :* False
              )
          )
  return $ Beh (curF ::: rest)

derivative' :: Beh Float -> C (Beh Float)
derivative' (Beh (x ::: xs)) = do
  t <- time
  Beh <$> der (apply x t) (x ::: xs)
  where
    der :: Float -> Sig (Fun Float) -> C (Sig (Fun Float))
    der last (x ::: xs) = do
      t <- time
      let curF =
            Fun $
              box
                ( \t' ->
                    let tDiff = diffTime t' t
                        dt = fromRational (toRational tDiff)
                     in (apply x t - last) / dt :* False
                )
      let rest =
            delayC
              ( delay
                  ( do
                      t' <- time
                      der (apply x t') (adv xs)
                  )
              )
      return (curF ::: rest)

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

{-# NOINLINE [1] map #-}
{-# NOINLINE [1] const #-}
{-# NOINLINE [1] constK #-}
{-# NOINLINE [1] switch #-}

{-# RULES

  "beh.map/beh.map" forall f g xs.
    map f (map g xs) = map (box (unbox f . unbox g)) xs ;

  "beh.constK/beh.map" forall (f :: Stable b => Box (a -> b))  x.
    map f (constK x) = let x' = unbox f x in constK x' ;

  "beh.const/beh.switch" forall x xs.
  switch (const x) xs = Beh (x ::: delay (unwrap (adv xs)));

  "beh.constK/beh.switch" forall x xs.
  switch (constK x) xs = Beh (K x ::: delay (unwrap (adv xs)));

#-}