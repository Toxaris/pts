{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, RankNTypes #-}
module Control.Monad.Errors
  ( ErrorsT, runErrorsT, module Control.Monad.Errors.Class ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.Error
import Control.Monad.Errors.Class
import Control.Monad.Writer
import Control.Monad.Reader(ReaderT, mapReaderT)
import Control.Monad.Reader.Class
import Control.Monad.State (StateT (StateT), mapStateT)
import Control.Monad.State.Class

newtype ErrorsT e m a
  =  ErrorsT (forall r . (a -> m r) -> (e -> a -> m r) -> (e -> m r) -> m r)

unErrorsT (ErrorsT p) = p

instance Functor (ErrorsT e m) where
  fmap f p = ErrorsT p' where
    p' result errors fatal
      =  unErrorsT p
           (\x    ->  result (f x))
           (\e x  ->  errors e (f x))
           fatal

returnErrorsT :: a -> ErrorsT e m a
returnErrorsT x = ErrorsT p where
  p result errors fatal
    = result x

instance Monoid e => Applicative (ErrorsT e m) where
  pure x = returnErrorsT x

  p <*> q = ErrorsT r where
    r result errors fatal
      =  unErrorsT p
           (\f    ->  unErrorsT q
                        (\x     ->  result (f x))
                        (\e x   ->  errors e (f x))
                        fatal)
           (\e f  ->  unErrorsT q
                        (\x     ->  errors e (f x))
                        (\e' x  ->  errors (mappend e e') (f x))
                        (\e'    ->  fatal (mappend e e')))
           fatal

instance (Error e, Monoid e) => Monad (ErrorsT e m) where
  return = returnErrorsT

  (>>=) = bindErrorsT

  fail m = ErrorsT p where
    p result errors fatal = fatal (strMsg m)

bindErrorsT :: Monoid e => ErrorsT e m a -> (a -> ErrorsT e m b) -> ErrorsT e m b
bindErrorsT p f = ErrorsT r where
    r result errors fatal
      =  unErrorsT p
           (\x    ->  unErrorsT (f x)
                        result
                        errors
                        fatal)
           (\e x  ->  unErrorsT (f x)
                        (\x     ->  errors e x)
                        (\e' y  ->  errors (mappend e e') y)
                        (\e'    ->  fatal (mappend e e')))
           fatal

instance (Error e, Monoid e) => MonadError e (ErrorsT e m) where
  throwError e = ErrorsT p where
    p result errors fatal = fatal e

  catchError p h = ErrorsT p' where
    p' result errors fatal
      =  unErrorsT p
           result
           (\e x  ->  unErrorsT (h e)
                        result
                        errors
                        fatal)
           (\e    ->  unErrorsT (h e)
                        result
                        errors
                        fatal)

instance (Error e, Monoid e) => MonadErrors e (ErrorsT e m) where
  annotate f p = ErrorsT p' where
    p' result errors fatal
      =  unErrorsT p
           result
           (\e x  ->  errors (f e) x)
           (\e    ->  fatal (f e))

  recover x p = ErrorsT p' where
    p' result errors fatal
      =  unErrorsT p
           result
           errors
           (\e -> errors e x)

runErrorsT :: (Error e, Monad m) =>  ErrorsT e m a -> m (Either e a)
runErrorsT p
  =  unErrorsT p
       (\x    ->  return (Right x))
       (\e x  ->  return (Left e))
       (\e    ->  return (Left e))

instance MonadTrans (ErrorsT e) where
  lift p = ErrorsT p' where
    p' result errors fatal
      =  p >>= result

instance (Monoid e, Error e, MonadIO m) => MonadIO (ErrorsT e m) where
  liftIO p = ErrorsT p' where
    p' result errors fatal
      =  liftIO p >>= result

instance (Monoid e, Error e, MonadReader r m) => MonadReader r (ErrorsT e m) where
  ask = lift ask
  local f p = ErrorsT p' where
    p' result errors fatal
      =  unErrorsT p
           (\x    ->  local f (result x))
           (\e x  ->  local f (errors e x))
           (\e    ->  local f (fatal e))

instance (Monoid e, Error e, MonadState s m) => MonadState s (ErrorsT e m) where
  get = lift get
  put = lift . put
