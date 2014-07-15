{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
module Control.Monad.Log where

import Prelude (String)

import Control.Applicative (Applicative)
import Control.Arrow (left)
import Control.Monad (Monad (return), when, Functor ())
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Errors ()
import Control.Monad.Errors.Class (MonadErrors)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState (get, put))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Writer (WriterT)

import Data.Bool (Bool (True, False))
import Data.Function (($))
import Data.Int (Int)
import Data.List (replicate, length, (++))
import Data.Monoid (Monoid)

import System.IO (putStrLn)

type Message
  = String

class Monad m => MonadLog m where
  enter :: Message -> m ()
  exit :: m ()
  log :: Message -> m ()
  stacktrace :: m [Message]
  enableLogging :: m ()
  disableLogging :: m ()

depth :: MonadLog m => m Int
depth = do
  trace <- stacktrace
  return (length trace)

newtype ConsoleLogT m a = ConsoleLogT (StateT (Bool, [Message]) m a)
  deriving (Functor, Applicative, Monad, MonadReader r, MonadWriter w, MonadIO, MonadTrans, MonadError e, MonadErrors e)

runConsoleLogT :: Monad m => ConsoleLogT m a -> Bool -> m a
runConsoleLogT (ConsoleLogT p) enabled = evalStateT p (enabled, [])

instance MonadState s m => MonadState s (ConsoleLogT m) where
  get = ConsoleLogT (lift get)
  put s = ConsoleLogT (lift (put s))

instance MonadIO m => MonadLog (ConsoleLogT m) where
{-
  enter text = do
    (enabled, trace) <- ConsoleLogT $ get
    log $ " >>> " ++ text
    ConsoleLogT $ put (enabled, text : trace)

  exit = do
    (enabled, trace) <- ConsoleLogT $ get
    case trace of
      [] -> do
        log "LOG ERROR: called exit without enter."
      (text : rest) -> do
        ConsoleLogT $ put (enabled, rest)
        log $ " <<< " ++ text

  log text = ConsoleLogT $ do
    (enabled, trace) <- get
    when enabled $
      liftIO $ putStrLn $ replicate (length trace) '-' ++ " " ++ text
-}
  enter _ = return ()
  exit = return ()
  log _ = return ()
  stacktrace = ConsoleLogT $ do
    (_, trace) <- get
    return trace

  enableLogging = ConsoleLogT $ do
    (_, trace) <- get
    put (True, trace)

  disableLogging = ConsoleLogT $ do
    (_, trace) <- get
    put (False, trace)

instance (Monoid w, MonadLog m) => MonadLog (WriterT w m) where
  enter text = lift (enter text)
  exit = lift exit
  log text = lift (log text)
  stacktrace = lift stacktrace
  enableLogging = lift enableLogging
  disableLogging = lift disableLogging

