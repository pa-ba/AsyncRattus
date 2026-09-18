# Introduction

This library implements the [Async
Rattus](http://dx.doi.org/10.1007/978-3-031-52038-9_2) programming
language as an embedded DSL. To this end, the library provides a GHC
plugin that checks the stricter typing rules of Async Rattus. In
addition, the library also provides a GUI framework, called [Widget
Rattus](http://dx.doi.org/10.1007/978-3-031-99751-8_5).

This branch of the library additionally contains an experimental
implementation of [push-pull
FRP](https://doi.org/10.1145/1596638.1596643) in Async Rattus. The
Widget Rattus GUI library is ported to this push-pull FRP approach,
see module
[WidgetRattus.PushPull.Widgets](src/WidgetRattus/PushPull/Widgets.hs).

# Examples

This repository also contains examples that use the the push-pull FRP
GUI library in [examples/simple-push-pull](examples/push-pull-gui/):
- [Calculator](examples/push-pull-gui/src/Calculator.hs)
- [Counter](examples/push-pull-gui/src/Counter.hs)
- [FlightBooker](examples/push-pull-gui/src/FlightBooker.hs)
- [Stopwatch](examples/push-pull-gui/src/Stopwatch.hs)
- [TemperatureConverter](examples/push-pull-gui/src/TemperatureConverter.hs)
- [Timer](examples/push-pull-gui/src/Timer.hs)
- [SimpleTimer](examples/push-pull-gui/src/SimpleTimer.hs)

The examples folder also contains a simple implementation of push-pull
FRP in Async Rattus (from section 3.2 of the accompanying paper) in
[examples/simple-push-pull](examples/simple-push-pull/src/PushPull.hs).


# Usage

This library has been tested with GHC versions 9.2 to 9.14. To install
it, issue the following command:

	cabal install
	
The GUI library is built on top of
[monomer](https://hackage.haskell.org/package/monomer). Monomer and
some of its dependencies have upper bounds on `containers` that
exclude the versions that ship with GHC 9.10 and later. This
repository's `cabal.project` files lift these bounds. If you use GHC
9.10 or later in your own project, add the following to its
`cabal.project` file:

	allow-newer: monomer:containers, nanovg:containers, OpenGLRaw:containers

The `examples` folder contains example projects written in Async
Rattus. In particular, the push-pull example GUIs can be run as
follows:

	cd examples/push-pull-gui
	cabal run timer
	cabal run calculator
	...
