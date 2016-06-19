{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Exception.Safe
    ( -- * Throwing
      throw
    , throwIO
    , throwM
    , throwTo
    , impureThrow
      -- * Catching (with recovery)
    , catch
    , catchAny
    , catchAsync

    , handle
    , handleAny
    , handleAsync

    , try
    , tryAny
    , tryAsync

      -- * Cleanup (no recovery)
    , onException
    , bracket
    , bracket_
    , finally
    , withException
    , bracketOnError
    , bracketOnError_

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
    , C.Handler (..)
    , Exception (..)
    , SomeException (..)
    , SomeAsyncException (..)
    , E.IOException
    ) where

import Control.Concurrent (ThreadId)
import Control.Exception (Exception (..), SomeException (..), SomeAsyncException (..))
import qualified Control.Exception as E
import qualified Control.Monad.Catch as C
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable, cast)

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

-- | Throw an asynchronous exception to another thread
--
-- @since 0.1.0.0
throwTo :: (Exception e, MonadIO m) => ThreadId -> e -> m ()
throwTo tid = liftIO . E.throwTo tid . toAsyncException

-- | Generate a pure value which, when forced, will synchronously
-- throw the given exception
--
-- @since 0.1.0.0
impureThrow :: Exception e => e -> a
impureThrow = E.throw

-- | Flipped version of 'catch'
--
-- @since 0.1.0.0
catch :: (C.MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catch f g = f `C.catch` \e ->
    if isSyncException e
        then g e
        -- intentionally rethrowing an async exception synchronously,
        -- since we want to preserve async behavior
        else C.throwM e

-- | 'catch' specialized to catch all synchronous exception
--
-- @since 0.1.0.0
catchAny :: C.MonadCatch m => m a -> (SomeException -> m a) -> m a
catchAny = catch

-- | 'catch' without async exception safety
--
-- @since 0.1.0.0
catchAsync :: (C.MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catchAsync = C.catch

-- | Flipped version of 'catch'
--
-- @since 0.1.0.0
handle :: (C.MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch

-- | Flipped version of 'catchAny'
--
-- @since 0.1.0.0
handleAny :: C.MonadCatch m => (SomeException -> m a) -> m a -> m a
handleAny = flip catchAny

-- | Flipped version of 'catchAsync'
--
-- @since 0.1.0.0
handleAsync :: (C.MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
handleAsync = C.handle

-- | Same as upstream 'C.try', but will not catch asynchronous
-- exceptions
--
-- @since 0.1.0.0
try :: (C.MonadCatch m, E.Exception e) => m a -> m (Either e a)
try f = catch (liftM Right f) (return . Left)

-- | 'try' specialized to catch all synchronous exceptions
--
-- @since 0.1.0.0
tryAny :: C.MonadCatch m => m a -> m (Either SomeException a)
tryAny = try

-- | 'try' without async exception safety
--
-- @since 0.1.0.0
tryAsync :: (C.MonadCatch m, E.Exception e) => m a -> m (Either e a)
tryAsync = C.try

-- | Async safe version of 'E.onException'
--
-- @since 0.1.0.0
onException :: C.MonadMask m => m a -> m b -> m a
onException f g = withException f (\(_ :: SomeException) -> g)

-- | Like 'onException', but provides the handler the thrown
-- exception.
--
-- @since 0.1.0.0
withException :: (C.MonadMask m, E.Exception e) => m a -> (e -> m b) -> m a
withException f g = C.uninterruptibleMask $ \restore -> do
    res1 <- C.try $ restore f
    case res1 of
        Left e1 -> do
            res2 <- C.try $ g e1
            case res2 of
                Left e2 | isAsyncException (e2 :: SomeException) -> C.throwM e2
                _ -> C.throwM e1
        Right x -> return x

-- | Async safe version of 'E.bracket'
--
-- @since 0.1.0.0
bracket :: C.MonadMask m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket f g h = C.uninterruptibleMask $ \restore -> do
    x <- restore f
    res1 <- C.try $ h x
    case res1 of
        Left (e1 :: SomeException) -> do
            res2 <- C.try $ g x
            case res2 of
                Left (e2 :: SomeException)
                    | isAsyncException e2 -> C.throwM e2
                _ -> C.throwM e1
        Right y -> do
            g x
            return y

-- | Async safe version of 'E.bracket_'
--
-- @since 0.1.0.0
bracket_ :: C.MonadMask m => m a -> m b -> m c -> m c
bracket_ f g h = bracket f (const g) (const h)

-- | Async safe version of 'E.finally'
--
-- @since 0.1.0.0
finally :: C.MonadMask m => m a -> m b -> m a
finally f g = C.uninterruptibleMask $ \restore -> do
    res1 <- C.try $ restore f
    case res1 of
        Left (e1 :: SomeException) -> do
            res2 <- C.try g
            case res2 of
                Left e2 | isAsyncException (e2 :: SomeException) -> C.throwM e2
                _ -> C.throwM e1
        Right x -> do
            g
            return x

-- | Async safe version of 'E.bracketOnError'
--
-- @since 0.1.0.0
bracketOnError :: C.MonadMask m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError f g h = C.uninterruptibleMask $ \restore -> do
    x <- restore f
    res1 <- C.try $ h x
    case res1 of
        Left (e1 :: SomeException) -> do
            res2 <- C.try $ g x
            case res2 of
                Left (e2 :: SomeException)
                    | isAsyncException e2 -> C.throwM e2
                _ -> C.throwM e1
        Right y -> return y

-- | Async safe version of 'E.bracketOnError_'
--
-- @since 0.1.0.0
bracketOnError_ :: C.MonadMask m => m a -> m b -> m c -> m c
bracketOnError_ f g h = bracketOnError f (const g) (const h)

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
