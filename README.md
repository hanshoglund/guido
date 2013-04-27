
# guido

Haskell bindings to the GUIDO Engine.

## Installation

### OS X

* Download and install the GUIDO Engine as a framework in
  `/Library/Frameworks/GUIDOEngine.framework`.

* Compile `libguidoc.dylib` using the Makefile.

* **Note:** If you want to use GHCI, you must start it using `make test`, or pass
  the framework and library manually.

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
