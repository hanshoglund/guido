
# guido

Haskell bindings to the [GUIDO Engine][guido-engine].

## Installation

### OS X

* Download and install the GUIDO Engine as a framework in `/Library/Frameworks/GUIDOEngine.framework`.
* Compile the `guido-c` library using the Makefile.
* Add the following to your `.cabal` file:

&nbsp;

    frameworks:
        GUIDOEngine
    extra-libraries:
        guidoc
    

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install

[guido-engine]: http://guidolib.sourceforge.net/