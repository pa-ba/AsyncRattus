# 0.5

 - Include push-pull-style behaviours and events (modules
   `WidgetRattus.Behaviour` and `WidgetRattus.Event`) along with a
   variant of the widget library based on behaviours and events
   (module `WidgetRattus.PushPull.Widgets`).
 - Rename `trigger`, `triggerM`, `triggerAwait`, and `triggerAwaitM`
   in `WidgetRattus.Signal` to `sample`, `sampleM`, `sampleAwait`,
   and `sampleAwaitM`, respectively, and `trigger` and `triggerAwait`
   in `WidgetRattus.Future` to `sample` and `sampleAwait`.
 - Support GHC 9.10, 9.12, and 9.14. With GHC 9.10 and later, monomer
   and some of its dependencies need `allow-newer` for `containers`
   (see README).
 - Fix the multiplicity of the binder that the plugin generates for
   `delay`.
 - Scope checking now also accounts for `Stable` constraints brought
   into scope by patterns that the type checker wraps in a coercion,
   e.g. when matching on a constructor of a data family instance.
 - New signal combinators `parallel` and `parallelWith`, and their
   variants `parallelAwait` and `parallelWithAwait` for delayed
   signals.
 - New function `chanSig` in `WidgetRattus.Signal`, which turns a
   channel into a delayed signal.
 - New function `withTime` in `WidgetRattus`, which gives a delayed
   computation access to the time at which it ticks.
 - New type synonym `DTime` and operator `<->` in `WidgetRattus.Time`
   for time differences.
 - New strict sum type `:+` in `WidgetRattus.Strict`.
 - New `Functor` instance for `Maybe'`.

# 0.4.0.1

 - The constraint solver for stable types can now handle data types
   with existential variables that have a `Stable` constraint, e.g. a
   GADT with constructor `MkFoo :: Stable a => !a -> Foo` is now
   recognised as stable.
 - Scope checking of variable now accounts for pattern matching with
   existential types. So pattern matching against the type `Foo`
   defined above accounts for the stable constraint. For instance, a
   function definition `fun (MkFoo x) = box x` now type checks.

# 0.4

- The C monad can now be discharged under the O modality via delayC.
- The C monad can now also query the current time.
- Remove Producer class.
- Remove Channels module; channels primitives are now in Primitives module.

# 0.3

- Include the Widgets library.
- Replace module names from AsyncRattus to WidgetRattus

# 0.2

Extend continuous types so that they can track their channel
dependencies.

# 0.1.1

Multiple channels can fire simultaneously now. This makes filter
functions better behaved.

# 0.1.0.1

Fix elaboration bug that cause a compiler panic

# 0.1

First release.
