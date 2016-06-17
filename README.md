# safe-exceptions

*Safe, consistent, and easy exception handling*

Runtime exceptions - as exposed in `base` by the `Control.Exception`
module - have long been an intimidating part of the Haskell
ecosystem. This package, and this README for the package, are intended
to overcome this. By providing an API that encourages best practices,
and explaining the corner cases clearly, the hope is to turn what was
previously something scary into an aspect of Haskell everyone feels
safe using.

## Caveats

### Checked vs unchecked

### Explicit vs implicit

## Terminology

* Synchronous
* Asynchronous
* Impure

## Determining sync vs async

* Type based

## Exception, error, and panic

* All synchronous exceptions should be recoverable by someone
* All asynchronous exceptions should not be recoverable
* In both cases, need cleanup code to work reliably

## Exceptions in cleanup code

## Typeclasses

* Leverage `exceptions` package for uniformity
