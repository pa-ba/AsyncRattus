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
