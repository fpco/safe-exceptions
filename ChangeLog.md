# ChangeLog for safe-exceptions

## 0.1.7.4

* Add `HasCallStack` when `exceptions >= 0.10.6` [#41](https://github.com/fpco/safe-exceptions/issues/41)

## 0.1.7.3

* Allow transformers 0.6 [#39](https://github.com/fpco/safe-exceptions/issues/39)

## 0.1.7.2

* Changed `bracketWithError` and `bracketOnError` to use `generalBracket` from `MonadMask` [#36](https://github.com/fpco/safe-exceptions/issues/36)
* Raised dependency `exceptions` from `>= 0.8` to `>= 0.10`

## 0.1.7.1

* Doc update

## 0.1.7.0

* Add `bracketWithError`

## 0.1.6.0

* Reuse the `Handler` definition from `Control.Monad.Catch`

## 0.1.5.0

* Re-export `Control.Exception.assert`
* Add `throwString`

## 0.1.4.0

* Add `catchJust`, `handleJust`, and `tryJust`

## 0.1.3.0

* Add `catchIO`, `handleIO`, and `tryIO`

## 0.1.2.0

* Added `catches` [#13](https://github.com/fpco/safe-exceptions/issues/13)

## 0.1.1.0

* Add missing `toSyncException` inside `impureThrow`
* Conditionally export `displayException` for older GHCs
* Re-export `Typeable`
* Add the deepseq variants of catch/handle/try functions

## 0.1.0.0

* Initial releae
