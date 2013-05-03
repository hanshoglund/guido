
# guido

Haskell bindings to the [GUIDO Engine][guido-engine].

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

### OS X

Download and install the GUIDO Engine as a framework in `/Library/Frameworks/GUIDOEngine.framework`.

Then compile the `guido-c` library using the Makefile.

Install the library with.

    cabal configure
    cabal install

You may need to add the following to the `.cabal` file in your project:

    frameworks:
        GUIDOEngine
    extra-libraries:
        guidoc
    

[guido-engine]: http://guidolib.sourceforge.net/