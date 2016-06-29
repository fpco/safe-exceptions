This is a cookbook for the usage of `safe-exceptions`. You should
start off by
[reading the README](https://github.com/fpco/safe-exceptions#readme),
or at least
[the quickstart section](https://github.com/fpco/safe-exceptions#quickstart).

_Request to readers_: if there are specific workflows that you're
unsure of how to accomplish with this library, please ask so we can
add them here. Issues and pull requests very much welcome!

## User-defined async exceptions

In order to define an async exception, you must leverage the
extensible exception machinery, as demonstrated below. Try running the
program, and then comment out the implementation of `toException` and
`fromException` to see the difference in behavior.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.4 runghc --package safe-exceptions-0.1.0.0
import Control.Concurrent     (forkIO, newEmptyMVar, putMVar, takeMVar,
                               threadDelay)
import Control.Exception.Safe
import Data.Typeable          (Typeable, cast)

data MyAsyncException = MyAsyncException
    deriving (Show, Typeable)

instance Exception MyAsyncException where
    toException = toException . SomeAsyncException
    fromException se = do
        SomeAsyncException e <- fromException se
        cast e

main :: IO ()
main = do
    baton <- newEmptyMVar -- give the handler a chance to run
    tid <- forkIO $ threadDelay maxBound
        `withException`
            (\e -> print ("Inside withException", e :: MyAsyncException))
        `finally` putMVar baton ()
    throwTo tid MyAsyncException
    takeMVar baton
    putStrLn "Done!"
```

The reason the `Inside withException` message isn't printed without
the implementation of `toException` and `fromException` given above is
that `throwTo` wraps `MyAsyncException` inside a different async
exception type, which foils the exception handler from firing.

*NOTE*: The above code is _not_ recommended concurrency code. If you
have to do something like this, _please use the async package_.
