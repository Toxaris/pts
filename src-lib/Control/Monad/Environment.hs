{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}
module Control.Monad.Environment where

import Prelude ()

import Data.Eq (Eq)
import Data.Function (($))
import Data.List (map)
import qualified Data.List as List (lookup)
import Data.Ord (Ord)
import Data.Set (Set, fromList)
import Data.Tuple (fst)

import Control.Applicative (Applicative)
import Control.Monad (Functor, Monad (return))
import Control.Monad.Error (MonadError)
import Control.Monad.Errors (MonadErrors)
import Control.Monad.Log (MonadLog (..))
import Control.Monad.Reader (MonadReader (ask, local), asks, ReaderT, runReaderT, mapReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans (lift), MonadIO)

import Data.Maybe (Maybe)

class (Eq k, Ord k, Monad m) => MonadEnvironment k v m | m -> k v where
  bind :: k -> v -> m a -> m a
  lookup :: Eq k => k -> m (Maybe v)
  keys :: m (Set k)
  getEnvironment :: m (Env k v)
  withEnvironment :: Env k v -> m a -> m a

instance MonadEnvironment k v m => MonadEnvironment k v (ReaderT r m) where
  bind k v = mapReaderT (bind k v)
  lookup k = lift (lookup k)
  keys = lift keys
  getEnvironment = lift getEnvironment
  withEnvironment env = mapReaderT (withEnvironment env)

type Env k v = [(k, v)]

newtype EnvironmentT k v m a = EnvironmentT (ReaderT (Env k v) m a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadErrors e, MonadState s, MonadTrans, MonadIO)

runEnvironmentT :: EnvironmentT k v m a -> Env k v -> m a
runEnvironmentT (EnvironmentT p) = runReaderT p

mapEnvironmentT f (EnvironmentT p) = EnvironmentT (mapReaderT f p)

instance (Eq k, Ord k, Monad m) => MonadEnvironment k v (EnvironmentT k v m) where
  bind k v (EnvironmentT p) = EnvironmentT (local ((k, v) : ) p)
  lookup k = EnvironmentT $ do
    env <- ask
    return $ List.lookup k $ env
  keys = EnvironmentT $ do
    env <- ask
    return $ fromList $ map fst env
  getEnvironment = EnvironmentT $ do
    ask
  withEnvironment env (EnvironmentT p) = EnvironmentT $ do
    local (\_ -> env) p

instance MonadReader r m => MonadReader r (EnvironmentT k v m) where
  ask = lift ask
  local f = mapEnvironmentT (local f)

instance MonadLog m => MonadLog (EnvironmentT k v m) where
  enter m = lift (enter m)
  exit = lift exit
  log m = lift (log m)
  stacktrace = lift stacktrace
  enableLogging = lift enableLogging
  disableLogging = lift disableLogging
