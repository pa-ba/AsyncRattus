# Introduciton

Asynchronous Rattus is a programming language embedded into GHC. It is built on top of [Rattus](https://github.com/pa-ba/Rattus) by utilizing the [Async RaTT](https://arxiv.org/abs/2303.03170) calculus.

# Usage

Asynchronous Rattus has been tested with GHC version 9.2.5. To install
it, run the following command:

	cabal install
	
Asynchronous Rattus also includes two different examples; a simple spreadsheet and a text writer. They can be accessed through following commands:
```
	cd examples/simple-sheet
	cabal run
```
and
```
	cd examples/textwriter
	cabal run
```

To run the test-suite of Asynchronous Rattus, simply run the the command `cabal test`
