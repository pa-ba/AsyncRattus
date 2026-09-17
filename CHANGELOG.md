# 0.???

 - Support GHC 9.10, 9.12, and 9.14.
 - Fix the multiplicity of the binder that the plugin generates for
   `delay`.
 - Newtypes are now recognised as stable if their underlying type is
   stable.
 - The strictness checker no longer warns about the lazy arguments of
   `fromString`, `fromList`/`fromListN` and `Data.Text.pack`. These
   functions consume their argument immediately, so it cannot cause a
   space leak.
 - `Item l`, the type family of the `IsList` class, is now recognised
   as strict whenever `l` is.
 - New strict sum type `:+` in `AsyncRattus.Strict`.
 - New `Functor` instance for `Maybe'`.

# 0.2.1.1

 - The constraint solver for stable types can now handle data types
   with existential variables that have a `Stable` constraint, e.g. a
   GADT with constructor `mkFoo :: Stable a => !a -> Foo` is now
   recognised as stable.

# 0.2.1
=======
- More signal combinators

# 0.2.0.2

- Fix strictness/stable checker: It now recognises `Word8/16/32/64` and
  `Int8/16/32/64` as strict and stable.
- Fix documentation for signal combinators `trigger` and
  `triggerAwait`.

# 0.2.0.1

Fix bug in elaboration of delay, adv, select

# 0.2

Instead of marking individual function definitions as Async Rattus
code, all function definitions are treated as Async Rattus if the
Async Rattus plugin is enabled. In practice, this means that a whole
module is declared as Async Rattus code by including the line
```
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
```
at the top of the file.

# 0.1.0.3

Fix concurrency bug in the interaction of output and input channels.
This occurred when using `mkInput` (and thus also filter functions on
signals).

# 0.1.0.2

Make Integer and Text stable types

# 0.1.0.1

Compatibility with GHC 9.8

# 0.1

First release.
