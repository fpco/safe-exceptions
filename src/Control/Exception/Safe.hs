{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
-- | Please see the README.md file in the safe-exceptions repo for
-- information on how to use this module. Relevant links:
--
-- * https://github.com/fpco/safe-exceptions#readme
--
-- * https://www.stackage.org/package/safe-exceptions
module Control.Exception.Safe
    ( -- * Throwing
      throw
    , throwIO
    , throwM
    , throwString
    , StringException (..)
    , throwTo
    , impureThrow
      -- * Catching (with recovery)
    , catch
    , catchIO
    , catchAny
    , catchDeep
    , catchAnyDeep
    , catchAsync
    , catchJust

    , handle
    , handleIO
    , handleAny
    , handleDeep
    , handleAnyDeep
    , handleAsync
    , handleJust

    , try
    , tryIO
    , tryAny
    , tryDeep
    , tryAnyDeep
    , tryAsync
    , tryJust

    , Handler(..)
    , catches
    , catchesDeep
    , catchesAsync

      -- * Cleanup (no recovery)
    , onException
    , bracket
    , bracket_
    , finally
    , withException
    , bracketOnError
    , bracketOnError_
    , bracketWithError

      -- * Coercion to sync and async
    , SyncExceptionWrapper (..)
    , toSyncException
    , AsyncExceptionWrapper (..)
    , toAsyncException

      -- * Check exception type
    , isSyncException
    , isAsyncException
      -- * Reexports
    , C.MonadThrow
    , C.MonadCatch
    , C.MonadMask (..)
    , C.mask_
    , C.uninterruptibleMask_
    , C.catchIOError
    , C.handleIOError
    -- FIXME , C.tryIOError
    , Exception (..)
    , Typeable
    , SomeException (..)
    , SomeAsyncException (..)
    , E.IOException
    , E.assert
#if !MIN_VERSION_base(4,8,0)
    , displayException
#endif
    ) where

import Control.Concurrent (ThreadId)
import Control.DeepSeq (($!!), NFData)
import Control.Exception (Exception (..), SomeException (..), SomeAsyncException (..))
import qualified Control.Exception as E
import qualified Control.Monad.Catch as C
import Control.Monad.Catch (Handler (..))
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable, cast)

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (prettySrcLoc)
import GHC.Stack.Types (HasCallStack, CallStack, getCallStack)
#endif

-- | Synchronously throw the given exception
--
-- @since 0.1.0.0
throw :: (C.MonadThrow m, Exception e) => e -> m a
throw = C.throwM . toSyncException

-- | Synonym for 'throw'
--
-- @since 0.1.0.0
throwIO :: (C.MonadThrow m, Exception e) => e -> m a
throwIO = throw

-- | Synonym for 'throw'
--
-- @since 0.1.0.0
throwM :: (C.MonadThrow m, Exception e) => e -> m a
throwM = throw

-- | A convenience function for throwing a user error. This is useful
-- for cases where it would be too high a burden to define your own
-- exception type.
--
-- This throws an exception of type 'StringException'. When GHC
-- supports it (base 4.9 and GHC 8.0 and onward), it includes a call
-- stack.
--
-- @since 0.1.5.0
#if MIN_VERSION_base(4,9,0)
throwString :: (C.MonadThrow m, HasCallStack) => String -> m a
throwString s = throwM (StringException s ?callStack)
#else
throwString :: C.MonadThrow m => String -> m a
throwString s = throwM (StringException s ())
#endif

-- | Exception type thrown by 'throwString'.
--
-- Note that the second field of the data constructor depends on
-- GHC/base version. For base 4.9 and GHC 8.0 and later, the second
-- field is a call stack. Previous versions of GHC and base do not
-- support call stacks, and the field is simply unit (provided to make
-- pattern matching across GHC versions easier).
--
-- @since 0.1.5.0
#if MIN_VERSION_base(4,9,0)
data StringException = StringException String CallStack
  deriving Typeable

instance Show StringException where
    show (StringException s cs) = concat
        $ "Control.Exception.Safe.throwString called with:\n\n"
        : s
        : "\nCalled from:\n"
        : map go (getCallStack cs)
      where
        go (x, y) = concat
          [ "  "
          , x
          , " ("
          , prettySrcLoc y
          , ")\n"
          ]
#else
data StringException = StringException String ()
  deriving Typeable

instance Show StringException where
    show (StringException s _) = "Control.Exception.Safe.throwString called with:\n\n" ++ s
#endif
instance Exception StringException

-- | Throw an asynchronous exception to another thread.
--
-- Synchronously typed exceptions will be wrapped into an
-- `AsyncExceptionWrapper`, see
-- <https://github.com/fpco/safe-exceptions#determining-sync-vs-async>
--
-- It's usually a better idea to use the async package, see
-- <https://github.com/fpco/safe-exceptions#quickstart>
--
-- @since 0.1.0.0
throwTo :: (Exception e, MonadIO m) => ThreadId -> e -> m ()
throwTo tid = liftIO . E.throwTo tid . toAsyncException

-- | Generate a pure value which, when forced, will synchronously
-- throw the given exception
--
-- Generally it's better to avoid using this function and instead use 'throw',
-- see <https://github.com/fpco/safe-exceptions#quickstart>
--
-- @since 0.1.0.0
impureThrow :: Exception e => e -> a
impureThrow = E.throw . toSyncException

-- | Same as upstream 'C.catch', but will not catch asynchronous
-- exceptions
--
-- @since 0.1.0.0
catch :: (C.MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catch f g = f `C.catch` \e ->
    if isSyncException e
        then g e
        -- intentionally rethrowing an async exception synchronously,
        -- since we want to preserve async behavior
        else C.throwM e

-- | 'C.catch' specialized to only catching 'E.IOException's
--
-- @since 0.1.3.0
catchIO :: C.MonadCatch m => m a -> (E.IOException -> m a) -> m a
catchIO = C.catch

-- | 'catch' specialized to catch all synchronous exception
--
-- @since 0.1.0.0
catchAny :: C.MonadCatch m => m a -> (SomeException -> m a) -> m a
catchAny = catch

-- | Same as 'catch', but fully force evaluation of the result value
-- to find all impure exceptions.
--
-- @since 0.1.1.0
catchDeep :: (C.MonadCatch m, MonadIO m, Exception e, NFData a)
          => m a -> (e -> m a) -> m a
catchDeep = catch . evaluateDeep

-- | Internal helper function
evaluateDeep :: (MonadIO m, NFData a) => m a -> m a
evaluateDeep action = do
    res <- action
    liftIO (E.evaluate $!! res)

-- | 'catchDeep' specialized to catch all synchronous exception
--
-- @since 0.1.1.0
catchAnyDeep :: (C.MonadCatch m, MonadIO m, NFData a) => m a -> (SomeException -> m a) -> m a
catchAnyDeep = catchDeep

-- | 'catch' without async exception safety
--
-- Generally it's better to avoid using this function since we do not want to
-- recover from async exceptions, see
-- <https://github.com/fpco/safe-exceptions#quickstart>
--
-- @since 0.1.0.0
catchAsync :: (C.MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catchAsync = C.catch

-- | 'catchJust' is like 'catch' but it takes an extra argument which
-- is an exception predicate, a function which selects which type of
-- exceptions we're interested in.
--
-- @since 0.1.4.0
catchJust :: (C.MonadCatch m, Exception e) => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust f a b = a `catch` \e -> maybe (throwM e) b $ f e

-- | Flipped version of 'catch'
--
-- @since 0.1.0.0
handle :: (C.MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch

-- | 'C.handle' specialized to only catching 'E.IOException's
--
-- @since 0.1.3.0
handleIO :: C.MonadCatch m => (E.IOException -> m a) -> m a -> m a
handleIO = C.handle


-- | Flipped version of 'catchAny'
--
-- @since 0.1.0.0
handleAny :: C.MonadCatch m => (SomeException -> m a) -> m a -> m a
handleAny = flip catchAny

-- | Flipped version of 'catchDeep'
--
-- @since 0.1.1.0
handleDeep :: (C.MonadCatch m, Exception e, MonadIO m, NFData a) => (e -> m a) -> m a -> m a
handleDeep = flip catchDeep

-- | Flipped version of 'catchAnyDeep'
--
-- @since 0.1.1.0
handleAnyDeep :: (C.MonadCatch m, MonadIO m, NFData a) => (SomeException -> m a) -> m a -> m a
handleAnyDeep = flip catchAnyDeep

-- | Flipped version of 'catchAsync'
--
-- Generally it's better to avoid using this function since we do not want to
-- recover from async exceptions, see
-- <https://github.com/fpco/safe-exceptions#quickstart>
--
-- @since 0.1.0.0
handleAsync :: (C.MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
handleAsync = C.handle

-- | Flipped 'catchJust'.
--
-- @since 0.1.4.0
handleJust :: (C.MonadCatch m, Exception e) => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJust f = flip (catchJust f)

-- | Same as upstream 'C.try', but will not catch asynchronous
-- exceptions
--
-- @since 0.1.0.0
try :: forall e m a. (C.MonadCatch m, E.Exception e) => m a -> m (Either e a)
try f = catch (liftM Right f) (return . Left)

-- | 'C.try' specialized to only catching 'E.IOException's
--
-- @since 0.1.3.0
tryIO :: C.MonadCatch m => m a -> m (Either E.IOException a)
tryIO = C.try

-- | 'try' specialized to catch all synchronous exceptions
--
-- @since 0.1.0.0
tryAny :: C.MonadCatch m => m a -> m (Either SomeException a)
tryAny = try

-- | Same as 'try', but fully force evaluation of the result value
-- to find all impure exceptions.
--
-- @since 0.1.1.0
tryDeep :: forall e m a. (C.MonadCatch m, MonadIO m, E.Exception e, NFData a) => m a -> m (Either e a)
tryDeep f = catch (liftM Right (evaluateDeep f)) (return . Left)

-- | 'tryDeep' specialized to catch all synchronous exceptions
--
-- @since 0.1.1.0
tryAnyDeep :: (C.MonadCatch m, MonadIO m, NFData a) => m a -> m (Either SomeException a)
tryAnyDeep = tryDeep

-- | 'try' without async exception safety
--
-- Generally it's better to avoid using this function since we do not want to
-- recover from async exceptions, see
-- <https://github.com/fpco/safe-exceptions#quickstart>
--
-- @since 0.1.0.0
tryAsync :: (C.MonadCatch m, E.Exception e) => m a -> m (Either e a)
tryAsync = C.try

-- | A variant of 'try' that takes an exception predicate to select
-- which exceptions are caught.
--
-- @since 0.1.4.0
tryJust :: (C.MonadCatch m, Exception e) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust f a = catch (Right `liftM` a) (\e -> maybe (throwM e) (return . Left) (f e))

-- | Async safe version of 'E.onException'
--
-- @since 0.1.0.0
onException :: C.MonadMask m => m a -> m b -> m a
onException thing after = withException thing (\(_ :: SomeException) -> after)

-- | Like 'onException', but provides the handler the thrown
-- exception.
--
-- @since 0.1.0.0
withException :: (C.MonadMask m, E.Exception e) => m a -> (e -> m b) -> m a
withException thing after = C.uninterruptibleMask $ \restore -> do
    res1 <- C.try $ restore thing
    case res1 of
        Left e1 -> do
            -- see explanation in bracket
            _ :: Either SomeException b <- C.try $ after e1
            C.throwM e1
        Right x -> return x

-- | Async safe version of 'E.bracket'
--
-- @since 0.1.0.0
bracket :: forall m a b c. C.MonadMask m
        => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after = bracketWithError before (const after)

-- | Async safe version of 'E.bracket_'
--
-- @since 0.1.0.0
bracket_ :: C.MonadMask m => m a -> m b -> m c -> m c
bracket_ before after thing = bracket before (const after) (const thing)

-- | Async safe version of 'E.finally'
--
-- @since 0.1.0.0
finally :: C.MonadMask m => m a -> m b -> m a
finally thing after = C.uninterruptibleMask $ \restore -> do
    res1 <- C.try $ restore thing
    case res1 of
        Left (e1 :: SomeException) -> do
            -- see bracket for explanation
            _ :: Either SomeException b <- C.try after
            C.throwM e1
        Right x -> do
            _ <- after
            return x

-- | Async safe version of 'E.bracketOnError'
--
-- @since 0.1.0.0
bracketOnError :: forall m a b c. C.MonadMask m
               => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError before after thing = C.mask $ \restore -> do
    x <- before
    res1 <- C.try $ restore (thing x)
    case res1 of
        Left (e1 :: SomeException) -> do
            -- ignore the exception, see bracket for explanation
            _ :: Either SomeException b <-
                C.try $ C.uninterruptibleMask_ $ after x
            C.throwM e1
        Right y -> return y

-- | A variant of 'bracketOnError' where the return value from the first
-- computation is not required.
--
-- @since 0.1.0.0
bracketOnError_ :: C.MonadMask m => m a -> m b -> m c -> m c
bracketOnError_ before after thing = bracketOnError before (const after) (const thing)

-- | Async safe version of 'E.bracket' with access to the exception in the
-- cleanup action.
--
-- @since 0.1.7.0
bracketWithError :: forall m a b c. C.MonadMask m
        => m a -> (Maybe SomeException -> a -> m b) -> (a -> m c) -> m c
bracketWithError before after thing = C.mask $ \restore -> do
    x <- before
    res1 <- C.try $ restore (thing x)
    case res1 of
        Left (e1 :: SomeException) -> do
            -- explicitly ignore exceptions from after. We know that
            -- no async exceptions were thrown there, so therefore
            -- the stronger exception must come from thing
            --
            -- https://github.com/fpco/safe-exceptions/issues/2
            _ :: Either SomeException b <-
                C.try $ C.uninterruptibleMask_ $ after (Just e1) x
            C.throwM e1
        Right y -> do
            _ <- C.uninterruptibleMask_ $ after Nothing x
            return y

-- | Wrap up an asynchronous exception to be treated as a synchronous
-- exception
--
-- This is intended to be created via 'toSyncException'
--
-- @since 0.1.0.0
data SyncExceptionWrapper = forall e. Exception e => SyncExceptionWrapper e
    deriving Typeable
instance Show SyncExceptionWrapper where
    show (SyncExceptionWrapper e) = show e
instance Exception SyncExceptionWrapper where
#if MIN_VERSION_base(4,8,0)
    displayException (SyncExceptionWrapper e) = displayException e
#endif

-- | Convert an exception into a synchronous exception
--
-- For synchronous exceptions, this is the same as 'toException'.
-- For asynchronous exceptions, this will wrap up the exception with
-- 'SyncExceptionWrapper'
--
-- @since 0.1.0.0
toSyncException :: Exception e => e -> SomeException
toSyncException e =
    case fromException se of
        Just (SomeAsyncException _) -> toException (SyncExceptionWrapper e)
        Nothing -> se
  where
    se = toException e

-- | Wrap up a synchronous exception to be treated as an asynchronous
-- exception
--
-- This is intended to be created via 'toAsyncException'
--
-- @since 0.1.0.0
data AsyncExceptionWrapper = forall e. Exception e => AsyncExceptionWrapper e
    deriving Typeable
instance Show AsyncExceptionWrapper where
    show (AsyncExceptionWrapper e) = show e
instance Exception AsyncExceptionWrapper where
    toException = toException . SomeAsyncException
    fromException se = do
        SomeAsyncException e <- fromException se
        cast e
#if MIN_VERSION_base(4,8,0)
    displayException (AsyncExceptionWrapper e) = displayException e
#endif

-- | Convert an exception into an asynchronous exception
--
-- For asynchronous exceptions, this is the same as 'toException'.
-- For synchronous exceptions, this will wrap up the exception with
-- 'AsyncExceptionWrapper'
--
-- @since 0.1.0.0
toAsyncException :: Exception e => e -> SomeException
toAsyncException e =
    case fromException se of
        Just (SomeAsyncException _) -> se
        Nothing -> toException (AsyncExceptionWrapper e)
  where
    se = toException e

-- | Check if the given exception is synchronous
--
-- @since 0.1.0.0
isSyncException :: Exception e => e -> Bool
isSyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> False
        Nothing -> True

-- | Check if the given exception is asynchronous
--
-- @since 0.1.0.0
isAsyncException :: Exception e => e -> Bool
isAsyncException = not . isSyncException
{-# INLINE isAsyncException #-}

#if !MIN_VERSION_base(4,8,0)
-- | A synonym for 'show', specialized to 'Exception' instances.
--
-- Starting with base 4.8, the 'Exception' typeclass has a method @displayException@, used for user-friendly display of exceptions. This function provides backwards compatibility for users on base 4.7 and earlier, so that anyone importing this module can simply use @displayException@.
--
-- @since 0.1.1.0
displayException :: Exception e => e -> String
displayException = show
#endif

-- | Same as upstream 'C.catches', but will not catch asynchronous
-- exceptions
--
-- @since 0.1.2.0
catches :: (C.MonadCatch m, C.MonadThrow m) => m a -> [Handler m a] -> m a
catches io handlers = io `catch` catchesHandler handlers

-- | Same as 'catches', but fully force evaluation of the result value
-- to find all impure exceptions.
--
-- @since 0.1.2.0
catchesDeep :: (C.MonadCatch m, C.MonadThrow m, MonadIO m, NFData a) => m a -> [Handler m a] -> m a
catchesDeep io handlers = evaluateDeep io `catch` catchesHandler handlers

-- | 'catches' without async exception safety
--
-- Generally it's better to avoid using this function since we do not want to
-- recover from async exceptions, see
-- <https://github.com/fpco/safe-exceptions#quickstart>
--
-- @since 0.1.2.0
catchesAsync :: (C.MonadCatch m, C.MonadThrow m) => m a -> [Handler m a] -> m a
catchesAsync io handlers = io `catchAsync` catchesHandler handlers

catchesHandler :: (C.MonadThrow m) => [Handler m a] -> SomeException -> m a
catchesHandler handlers e = foldr tryHandler (C.throwM e) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res
