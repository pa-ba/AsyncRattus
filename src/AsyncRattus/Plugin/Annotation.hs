{-# LANGUAGE DeriveDataTypeable #-}
module AsyncRattus.Plugin.Annotation (AsyncRattus(..), InternalAnn (..)) where

import Data.Data

-- | By default all Async Rattus functions are checked for use of lazy
-- data types, since these may cause memory leaks. If any lazy data
-- types are used, a warning is issued. These warnings can be disabled
-- by annotating the module or the function with 'AllowLazyData'
--
-- > {-# ANN myFunction AllowLazyData #-}
-- >
-- > {-# ANN module AllowLazyData #-}
--
-- Async Rattus only allows guarded recursion, i.e. recursive calls
-- must occur in the scope of a tick. Structural recursion over strict
-- data types is safe as well, but is currently not checked. To
-- disable the guarded recursion check, annotate the module or
-- function with 'AllowRecursion'.
-- 
-- > {-# ANN myFunction AllowRecursion #-}
-- >
-- > {-# ANN module AllowRecursion #-}


data AsyncRattus = AllowLazyData | AllowRecursion deriving (Typeable, Data, Show, Ord, Eq)


-- | This annotation type is for internal use only.
data InternalAnn = ExpectError | ExpectWarning deriving (Typeable, Data, Show, Eq, Ord)
