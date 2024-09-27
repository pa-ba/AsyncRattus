# Introduction

This library implements the Widget Rattus programming language as an
embedded DSL. To this end the library provides a GHC plugin that
checks the stricter typing rules of Widget Rattus.
            
Widget Rattus is an experimental functional reactive programming (FRP)
language for GUI programming that uses modal types to express temporal
dependencies. In return, the language will guarantee that programs are
productive (in each computation step, the program makes progress),
causal (output depends only on current and earlier input), and have no
space leaks (programs do not implicitly retain memory over time).

Widget Rattus is an extension of Async Rattus to support GUI
programming. A more detailed introduction to the Async Rattus language
can be found in this [paper](docs/paper.pdf).

# Usage

This library has been tested with GHC versions 9.2 to 9.6. To install
it, issue the following command:

	cabal install
	

The `examples` folder contains example projects written in Widget
Rattus. These can be used as a template to start Widget Rattus
projects. For instance, [gui.cabal](examples/gui/gui.cabal) implements
a number of small GUI applications, which you can run as follows:

	cd examples/gui
	cabal run timer
