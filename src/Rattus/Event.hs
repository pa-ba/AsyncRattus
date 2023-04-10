{-# OPTIONS -fplugin=Rattus.Plugin #-}

-- | Programming with many-shot events, i.e. events that may occur zero
-- or more times.


module Rattus.Event
  ( map
  , never
  , switch
  , switchTrans
  , dswitchTrans
  , combine
  , Event
  , trigger
  , triggerMap
  )

where

import Rattus
import Rattus.Stream hiding (map)
import qualified Rattus.Stream as Str

import Prelude hiding (map,zipWith)

-- | Events are simply streams of 'Maybe''s.
type Event a = Str (Maybe' a)

-- all functions in this module are in Rattus 
--{-# ANN module Rattus #-}

-- | Apply a function to the values of the event (every time it occurs).
{-# NOINLINE [1] map #-}
map :: Box (a -> b) -> Event a -> Event b
map f (Just' x ::: xs) =  (Just' (unbox f x)) ::: delay (map f (adv xs))
map f (Nothing' ::: xs) = Nothing' ::: delay (map f (adv xs))

-- | An event that will never occur.
never :: Event a
never = Nothing' ::: delay never

-- | @switch s e@ will behave like @s@ but switches to @s'$ every time
-- the event 'e' occurs with some value @s'@.
switch :: Str a -> Event (Str a) -> Str a
switch (x ::: xs) (Nothing' ::: fas) = x ::: delay (switch (adv xs)  (adv fas))
switch  _xs       (Just' (a ::: as) ::: fas)   = a ::: (delay switch <#> as <#> fas)


-- | @combine f s e@ is similar to @switch s e@, but instead of
-- switching to new streams @s'@ every time the event 'e' occurs with
-- some value @s'@, the new stream @s'@ is combined with the current
-- stream @s@ using @zipWith f s' s@.
combine :: Box (a -> a -> a) -> Str a -> Event (Str a) -> Str a
combine f (x ::: xs) (Nothing' ::: fas) = x  ::: delay (combine f (adv xs) (adv fas))
combine f xs         (Just' as ::: fas) = x' ::: delay (combine f (adv xs') (adv fas))
  where (x' ::: xs') = zipWith f xs as


-- | Like 'switch' but works on stream functions instead of
-- streams. That is, @switchTrans s e@ will behave like @s@ but
-- switches to @s'$ every time the event 'e' occurs with some value
-- @s'@.
switchTrans :: (Str a -> Str b) -> Event (Str a -> Str b) -> (Str a -> Str b)
switchTrans f es as = switchTrans' (f as) es as

-- | Helper function for 'switchTrans'.
switchTrans' :: Str b -> Event (Str a -> Str b) -> Str a -> Str b
switchTrans' (b ::: bs) (Nothing' ::: fs) (_:::as) = b ::: (delay switchTrans' <#> bs <#> fs <#> as)
switchTrans' _xs        (Just' f ::: fs)  as@(_:::as') = b' ::: (delay switchTrans' <#> bs' <#> fs <#> as')
  where (b' ::: bs') = f as



-- | Like 'switchTrans' but takes a delayed event as input, which
-- allows the switch to incorporate feedback from itself.
dswitchTrans :: (Str a -> Str b) -> O (Event (Str a -> Str b)) -> (Str a -> Str b)
dswitchTrans f es as = dswitchTrans' (f as) es as

-- | Helper function for 'dswitchTrans'.
dswitchTrans' :: Str b -> O (Event (Str a -> Str b)) -> Str a -> Str b
dswitchTrans' (b ::: bs) de (_:::as) = b ::: (delay switchTrans' <#> bs <#> de <#> as)


-- | Trigger an event as every time the given predicate turns true on
-- the given stream. The value of the event is the same as that of the
-- stream at that time.
trigger :: Box (a -> Bool) -> Str a -> Event a
trigger p (x ::: xs) = x' ::: (delay (trigger p) <#> xs)
  where x' = if unbox p x then Just' x else Nothing'

-- | Trigger an event every time the given function produces a 'Just''
-- value.
triggerMap :: Box (a -> Maybe' b) -> Str a -> Event b
triggerMap = Str.map


{-# RULES

  "map/map" forall f g xs.
    map f (map g xs) = map (box (unbox f . unbox g)) xs ;

#-}
