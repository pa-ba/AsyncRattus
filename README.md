# Introduction

This library implements the Async Rattus programming language as an
embedded DSL. To this end the library provides a GHC plugin that
checks the stricter typing rules of Async Rattus.
            
Async Rattus is a functional reactive programming (FRP) language that
uses modal types to express temporal dependencies. In return the
language will guarantee that programs are productive (in each
computation step, the program makes progress), causal (output depends
only on current and earlier input), and have no space leaks (programs
do not implicitly retain memory over time).

A more detailed introduction to the language can be found in this
[paper](docs/paper.pdf).

# Usage

This library has been tested with GHC versions 9.2 to 9.6. To install
it, issue the following command:

	cabal install
	

The `examples` folder contains example projects written in Async
Rattus. These can be used as a template to start Async Rattus
projects. For instance,
[console.cabal](examples/console/console.cabal) implements a simple
console application, which you can run as follows:

	cd examples/console
	cabal run signal
