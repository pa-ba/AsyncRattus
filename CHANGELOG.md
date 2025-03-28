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
