{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WidgetRattus.Event where

import WidgetRattus.Behaviour

import WidgetRattus
import WidgetRattus.Signal hiding (buffer, interleave, interleaveAll, map, scan, switchR, switchS)
import qualified WidgetRattus.Signal as Sig
import Prelude hiding (filter, map)

data Ev a
  = Dense !(O (Sig a))
  | Sparse !(O (Sig (Maybe' a)))

mkEv :: Box (O a) -> Ev a
mkEv = Dense . run where
    run :: Box (O a) -> O (Sig a)
    run a = delay (adv (unbox a) ::: run a)

mkEv' :: Box (O (C a)) -> Ev a
mkEv' = Dense . run where
    run :: Box (O (C a)) -> O (Sig a)
    run b = delayC (delay ((::: run b) <$> adv (unbox b)))


mapE :: forall a b . Box (a -> b) -> Ev a -> Ev b
mapE f (Dense sig) = Dense (run sig) where
    run :: O (Sig a) -> O (Sig b)
    run sig = delay ( let x ::: xs = adv sig
                      in unbox f x ::: run xs )
mapE f (Sparse sig) = Sparse (run sig) where
    run :: O (Sig (Maybe' a)) -> O (Sig (Maybe' b))
    run sig = delay ( let x ::: xs = adv sig
                      in (unbox f <$> x) ::: run xs)

removeC :: Ev (C a) -> Ev a
removeC (Dense sig) =
  Dense
    ( delayC
        ( delay
            ( let x ::: xs = adv sig
                  (Dense rest) = removeC (Dense xs)
               in ( do
                      x' <- x
                      return (x' ::: rest)
                  )
            )
        )
    )

stepper :: (Stable a) => a -> Ev a -> Beh a
stepper initial event =
  Beh (K initial ::: delay (unwrap (adv (aux initial event))))
  where
    aux :: (Stable a) => a -> Ev a -> O (Beh a)
    aux _ (Dense ev) =
      delay (let (x ::: xs) = adv ev in Beh (K x ::: delay (unwrap (adv (aux x (Dense xs))))))
    aux initial (Sparse ev) =
      delay
        ( let (x ::: xs) = adv ev
           in case x of
                Just' x' ->
                  Beh (K x' ::: delay (unwrap (adv (aux x' (Sparse xs)))))
                Nothing' -> Beh (K initial ::: delay (unwrap (adv (aux initial (Sparse xs)))))
        )

trigger :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> Ev c
trigger f (Sparse ev) (Beh beh) = Sparse (run ev beh) where
  run as (b ::: bs) = withTime $ delay
    ( let d = select as bs
      in \ t -> case d of
        Fst (Just' a'' ::: as') bs' -> Just' (unbox f a'' (at b t)) ::: run as' (b ::: bs')
        Fst (Nothing' ::: as') bs' -> Nothing' ::: run as' (b ::: bs')
        Snd as' bs' -> Nothing' ::: run as' bs'
        Both (Just' a'' ::: as') (b' ::: bs') -> Just' (unbox f a'' (at b' t)) ::: run as' (b' ::: bs')
        Both (Nothing' ::: as') (b' ::: bs') -> Nothing' ::: run as' (b' ::: bs'))
trigger f (Dense ev) (Beh beh) = Sparse (run ev beh) where
  run as (b ::: bs) =  withTime $ delay
    ( let d = select as bs
      in \ t -> case d of
          Fst (a' ::: as') bs' -> Just' (unbox f a' (at b t)) ::: run as' (b ::: bs')
          Snd as' bs' -> Nothing' ::: run as' bs'
          Both (a' ::: as') (b' ::: bs') -> Just' (unbox f a' (at b' t)) ::: run as' (b' ::: bs') )

interleave :: Box (a -> a -> a) -> Ev a -> Ev a -> Ev a
interleave f (Dense xs) (Dense ys) = Dense (run xs ys) where
  run xs ys = delay ( case select xs ys of
            Fst (x ::: xs') ys' -> x ::: run xs' ys'
            Snd xs' (y ::: ys') -> y ::: run xs' ys'
            Both (x ::: xs') (y ::: ys') -> unbox f x y ::: run xs' ys' )
interleave f (Sparse xs) (Sparse ys) = Sparse (run xs ys) where
  run xs ys = delay ( case select xs ys of
          Fst (x ::: xs') ys' -> x ::: run xs' ys'
          Snd xs' (y ::: ys') -> y ::: run xs' ys'
          Both (Just' x ::: xs') (Nothing' ::: ys') -> Just' x ::: run xs' ys'
          Both (Nothing' ::: xs') (y ::: ys') -> y ::: run xs' ys'
          Both (Just' x ::: xs') (Just' y ::: ys') -> Just' (unbox f x y) ::: run xs' ys')
interleave f (Sparse xs) (Dense ys) = Sparse (run xs ys) where
  run xs ys = delay ( case select xs ys of
          Fst (x ::: xs') ys' -> x ::: run xs' ys'
          Snd xs' (y ::: ys') -> Just' y ::: run xs' ys'
          Both (Nothing' ::: xs') (y ::: ys') -> Just' y ::: run xs' ys'
          Both (Just' x ::: xs') (y ::: ys') -> Just' (unbox f x y) ::: run xs' ys')

interleave f (Dense xs) (Sparse ys) = Sparse (run xs ys) where
  run xs ys = delay ( case select xs ys of
          Fst (x ::: xs') ys' -> Just' x ::: run xs' ys'
          Snd xs' (y ::: ys') -> y ::: run xs' ys'
          Both (x ::: xs') (Nothing' ::: ys') -> Just' x ::: run xs' ys'
          Both (x ::: xs') (Just' y ::: ys') -> Just' (unbox f x y) ::: run xs' ys')


{-# ANN interleaveAll AllowRecursion #-}
interleaveAll :: Box (a -> a -> a) -> List (Ev a) -> Ev a
interleaveAll _ Nil = error "interleaveAll: List must be nonempty"
interleaveAll _ [s] = s
interleaveAll f (x :! xs) = interleave f x (interleaveAll f xs)

scan :: Stable b => Box (b -> a -> b) -> b -> Ev a -> Ev b
scan f acc (Dense ev) = Dense (delay (Sig.scan f acc (adv ev))) where
scan f acc (Sparse ev) = Sparse (delay (scanSparse f acc (adv ev))) where

scanSparse :: Stable b => Box (b -> a -> b) -> b -> Sig (Maybe' a) -> Sig (Maybe' b)
scanSparse f acc (Just' x ::: xs) = let acc' = unbox f acc x in Just' acc' ::: delay (scanSparse f acc' (adv xs))
scanSparse f acc (Nothing' ::: xs) = Nothing' ::: delay (scanSparse f acc (adv xs))

filterMap :: Box (a -> Maybe' b) -> Ev a -> Ev b
filterMap f (Dense ev) = Sparse (run ev) where
  run ev = delay (let (x ::: xs) = adv ev in unbox f x ::: run xs)
filterMap f (Sparse ev) = Sparse (run ev) where
  run ev = delay (case adv ev of
                Just' x' ::: xs -> unbox f x' ::: run xs
                Nothing' ::: xs -> Nothing' ::: run xs)

-- filter f = filterMap (box (\x -> if unbox f x then Just' x else Nothing'))

filter :: Box (a -> Bool) -> Ev a -> Ev a
filter p (Dense ev) = Sparse (run ev) where
  run ev = delay (let x ::: xs = adv ev 
                  in (if unbox p x then Just' x else Nothing') ::: run xs)
filter p (Sparse ev) = Sparse (run ev) where
  run ev = delay (case adv ev of 
                    Nothing' ::: xs -> Nothing' ::: run xs
                    Just' x  ::: xs -> (if unbox p x then Just' x else Nothing') ::: run xs)
  
  
switchS :: (Stable a) => Beh a -> O (a -> Beh a) -> Beh a
switchS (Beh (x ::: xs)) d = Beh (x ::: withTime (delay (
              let ticker = select xs d
              in \ t -> case ticker of
                            Fst xs' d' -> unwrap $ switchS (Beh xs') d'
                            Snd _ f -> unwrap $ f (at x t)
                            Both _ f -> unwrap $ f (at x t))))

switchS' :: (Stable a) => Beh a -> O (a -> C (Beh a)) -> Beh a
switchS' (Beh (x ::: xs)) d = Beh (x ::: delayC (delay
              ( let ticker = select xs d
                 in do
                      t <- time
                      let result =
                            ( case ticker of
                                Fst xs' d' -> do
                                  return $ switchS' (Beh xs') d'
                                Snd _ f -> f (at x t)
                                Both _ f -> f (at x t)
                            )
                      unwrap <$> result
              )
          ))

switchSM :: (Stable a) => Beh a -> O (Maybe' (a -> Beh a)) -> Beh a
switchSM (Beh (x ::: xs)) d =
  let rest =
        delayC
          ( delay
              ( let ticker = select xs d
                 in do
                      t <- time
                      return
                        ( case ticker of
                            Fst xs' d' -> unwrap $ switchSM (Beh xs') d'
                            Snd _ (Just' f) -> unwrap $ f (at x t)
                            Snd xs' Nothing' -> x ::: xs'
                            Both _ (Just' f) -> unwrap $ f (at x t)
                            Both xs' Nothing' -> xs'
                        )
              )
          )
   in Beh (x ::: rest)

switchSM' :: (Stable a) => Beh a -> O (Maybe' (a -> C (Beh a))) -> Beh a
switchSM' (Beh (x ::: xs)) d =
  let rest =
        delayC
          ( delay
              ( let ticker = select xs d
                 in do
                      t <- time
                      let result =
                            ( case ticker of
                                Fst xs' d' -> do return $ switchSM' (Beh xs') d'
                                Snd _ (Just' f) -> f (at x t)
                                Snd xs' Nothing' -> do return $ Beh (x ::: xs')
                                Both _ (Just' f) -> f (at x t)
                                Both xs' Nothing' -> do return $ Beh xs'
                            )

                      unwrap <$> result
              )
          )
   in Beh (x ::: rest)

switchR :: (Stable a) => Beh a -> Ev (a -> Beh a) -> Beh a
switchR beh (Dense steps) =
  switchS beh (delay (let step ::: steps' = adv steps in (\x -> switchR (step x) (Dense steps'))))
switchR beh (Sparse steps) =
  switchSM
    beh
    ( delay
        ( let step ::: steps' = adv steps
           in case step of
                Just' a -> Just' (\x -> switchR (a x) (Sparse steps'))
                Nothing' -> Nothing'
        )
    )

switchR' :: (Stable a) => Beh a -> Ev (a -> C (Beh a)) -> Beh a
switchR' beh (Dense steps) =
  switchS'
    beh
    ( delay
        ( let step ::: steps' = adv steps
           in ( \x -> do
                  x' <- step x
                  return $ switchR' x' (Dense steps')
              )
        )
    )
switchR' beh (Sparse steps) =
  switchSM'
    beh
    ( delay
        ( let step ::: steps' = adv steps
           in case step of
                Just' a ->
                  Just'
                    ( \x -> do
                        x' <- a x
                        return $ switchR' x' (Sparse steps')
                    )
                Nothing' -> Nothing'
        )
    )

buffer :: (Stable a) => a -> Ev a -> Ev a
buffer x (Dense ys) = Dense (delay (let (y ::: ys') = adv ys in (x ::: let (Dense rest) = buffer y (Dense ys') in rest)))
buffer x (Sparse ys) =
  Dense
    ( delay
        ( let (y ::: ys') = adv ys
           in case y of
                Just' y' -> x ::: let (Dense rest) = buffer y' (Sparse ys') in rest
                Nothing' -> x ::: let (Dense rest) = buffer x (Sparse ys') in rest
        )
    )

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

{-# NOINLINE [1] mapE #-}

{-# NOINLINE [1] filter #-}

{-# RULES
"ev.map/ev.map" forall f g xs.
  mapE f (mapE g xs) =
    mapE (box (unbox f . unbox g)) xs
"ev.map/ev.filter" forall f g xs.
  mapE f (filter g xs) =
    filterMap (box (\x -> if unbox g x then Just' (unbox f x) else Nothing')) xs
"ev.filter/ev.map" forall f g xs.
  filter f (mapE g xs) =
    filterMap (box (\x -> if (unbox f . unbox g) x then Just' $ unbox g x else Nothing')) xs
"ev.filter/ev.filter" forall f g xs.
  filter f (filter g xs) =
    filterMap (box (\x -> if unbox f x && unbox g x then Just' x else Nothing')) xs
  #-}