{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Control.Monad.Assertions
  ( -- * Interface
    MonadAssertions (assert)
    -- * Implementation strategies
    -- ** Ignoring assertions
  , IgnoreAssertionsT
  , ignoreAssertions
    -- ** Checking assertions
  , CheckAssertionsT
  , checkAssertions
    -- ** Collecting information about assertions
  , CollectAssertionsT
  , collectAssertions) where

import Control.Applicative (Applicative)
import Control.Monad.Error (MonadError, catchError)
import Control.Monad.Log (MonadLog, ConsoleLogT (ConsoleLogT))
import Control.Monad.Reader (MonadReader, ReaderT, mapReaderT)
import Control.Monad.State (MonadState, StateT (StateT))
import Control.Monad.Trans (MonadIO)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell, mapWriterT, censor)

import Data.Monoid (Monoid, mempty)

import Tools.Errors (MonadErrors (recover, annotate))

-- | Computations with embedded assertions.
class Monad m => MonadAssertions m where
  -- | Assert that some computation doesn't fail.
  assert
    :: String  -- ^ human-readable name
    -> m ()    -- ^ computation
    -> m ()

-- | Prevent Writer output by assertions.
instance (Monoid w, MonadAssertions m) => MonadAssertions (WriterT w m) where
  assert text = mapWriterT $ \p -> assert text (p >> return ()) >> return ((), mempty)

-- | Prevent logging state changes by assertions.
instance MonadAssertions m => MonadAssertions (ConsoleLogT m) where
  assert text (ConsoleLogT p) = ConsoleLogT (assert text p)

-- | Prevent state changes by assertions
instance MonadAssertions m => MonadAssertions (StateT s m) where
  assert text (StateT p) = StateT $ \s -> do
    assert text (p s >> return ())
    return ((), s)

instance MonadAssertions m => MonadAssertions (ReaderT r m) where
  assert text = mapReaderT (assert text)

-- | The strategy of ignoring optional assertions.
newtype IgnoreAssertionsT m a = IgnoreAssertionsT (m a)
  deriving (Functor, Applicative, Monad, MonadState s,
    MonadReader r, MonadWriter w, MonadError e, MonadErrors e,
    MonadLog, MonadIO)

-- | Run a computation and ignore the embedded assertions.
ignoreAssertions :: IgnoreAssertionsT m a -> m a
ignoreAssertions (IgnoreAssertionsT p) = p

-- | Ignore assertions.
instance Monad m => MonadAssertions (IgnoreAssertionsT m) where
  assert text action = return ()

-- | The strategy of failing if embedded assertions fail.
newtype CheckAssertionsT m a = CheckAssertionsT (m a)
  deriving (Functor, Applicative, Monad, MonadState s,
    MonadReader r, MonadWriter w, MonadError e, MonadErrors e,
    MonadLog, MonadIO)

-- | Run a computation and fail if embedded assertions fail.
checkAssertions :: CheckAssertionsT m a -> m a
checkAssertions (CheckAssertionsT p) = p

-- Run assertions.
instance Monad m => MonadAssertions (CheckAssertionsT m) where
  assert text action = action

-- | The strategy of collecting errors raised by assertions.
newtype CollectAssertionsT e m a = CollectAssertionsT (WriterT [(String, Maybe e)] m a)
  deriving (Functor, Applicative, Monad, MonadState s,
    MonadReader r, MonadError e, MonadLog, MonadIO)

-- | Annotate the collected errors raised by assertions.
instance MonadErrors e m => MonadErrors e (CollectAssertionsT e m) where
  recover result (CollectAssertionsT action) = CollectAssertionsT $ do
    recover result action
  annotate f (CollectAssertionsT action) = CollectAssertionsT $ do
    censor (fmap (fmap (fmap f))) $
      annotate f action

-- | Run a computation and collect errors raised by assertions.
collectAssertions :: CollectAssertionsT e m a -> m (a, [(String, Maybe e)])
collectAssertions (CollectAssertionsT p) =
  runWriterT p

-- | Catch errors raised by assertions and collect them.
instance MonadError e m => MonadAssertions (CollectAssertionsT e m) where
  assert text (CollectAssertionsT action) = CollectAssertionsT $
    (action >> tell [(text, Nothing)]) `catchError` \e -> tell [(text, Just e)]
