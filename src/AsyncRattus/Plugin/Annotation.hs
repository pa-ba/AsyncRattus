{-# LANGUAGE DeriveDataTypeable #-}
module AsyncRattus.Plugin.Annotation (AsyncRattus(..), InternalAnn (..)) where

import Data.Data

-- | Use this type to mark a Haskell function definition as an
-- Asynchronous Rattus function:
--
-- > {-# ANN myFunction AsyncRattus #-}
-- 
-- Or mark a whole module as consisting of Asynchronous Rattus functions only:
--
-- > {-# ANN module AsyncRattus #-}
--
-- If you use the latter option, you can mark exceptions
-- (i.e. functions that should be treated as ordinary Haskell function
-- definitions) as follows:
--
-- > {-# ANN myFunction NotAsyncRattus #-}
--
-- By default all Asynchronous Rattus functions are checked for use of lazy data
-- types, since these may cause memory leaks. If any lazy data types
-- are used, a warning is issued. These warnings can be disabled by
-- annotating the module or the function with 'AllowLazyData'
--
-- > {-# ANN myFunction AllowLazyData #-}
-- >
-- > {-# ANN module AllowLazyData #-}
--
-- Asynchronous Rattus only allows guarded recursion, i.e. recursive calls must
-- occur in the scope of a tick. Structural recursion over strict data
-- types is safe as well, but is currently not checked. To disable the
-- guarded recursion check, annotate the module or function with
-- 'AllowRecursion'.

data AsyncRattus = AsyncRattus | NotAsyncRattus | AllowLazyData | AllowRecursion deriving (Typeable, Data, Show, Ord, Eq)


-- | This annotation type is for internal use only.
data InternalAnn = ExpectTcError | ExpectCoreError | ExpectWarning deriving (Typeable, Data, Show, Eq, Ord)
