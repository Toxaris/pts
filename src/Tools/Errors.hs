{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, RankNTypes #-}
module Tools.Errors 
  ( ErrorsT, runErrorsT, module Tools.Errors.Class ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Reader(ReaderT, mapReaderT)
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Tools.Instances
import Tools.Errors.Class

type ErrorsT = ErrorsT'
runErrorsT = runErrorsT'

-- -- the old Errors
-- data Errors e a
--   = Result a
--   | Errors e a
--   | Fatal e 
-- 
-- -- Errors is a bifunctor!
-- mapErrors :: (e -> e') -> (a -> a') -> Errors e a -> Errors e' a'
-- mapErrors f g (Result a)   = Result (g a)
-- mapErrors f g (Errors e a) = Errors (f e) (g a)
-- mapErrors f g (Fatal e)    = Fatal (f e)
-- 
-- instance Functor (Errors e) where
--   fmap f (Result x) = Result (f x)
--   fmap f (Errors e x) = Errors e (f x)
--   fmap f (Fatal e) = Fatal e
-- 
-- instance (Monoid e, Error e) => Applicative (Errors e) where
--   pure = return
--   (<*>) = ap
--   
-- instance (Monoid e, Error e) => Monad (Errors e) where
--   return x = Result x
-- 
--   Result x >>= f = 
--     f x
--   Errors e x >>= f = 
--     case f x of
--       Result y -> Errors e y
--       Errors e' y -> Errors (e `mappend` e') y
--       Fatal e' -> Fatal (e `mappend` e')
--   Fatal e >>= f = 
--     Fatal e
--   
--   fail msg = Fatal (strMsg msg)
-- 
-- instance (Monoid e, Error e) => MonadError e (Errors e) where
--   throwError = Fatal
--   Result x `catchError` h = Result x
--   Errors e a `catchError` h = h e 
--   Fatal e `catchError` h = h e  
-- 
-- instance (Monoid e, Error e) => MonadErrors e (Errors e) where
--   annotate f (Result x) = Result x
--   annotate f (Errors e x) = Errors (f e) x
--   annotate f (Fatal e) = Fatal (f e)
--   
--   recover x' (Result x) = Result x
--   recover x' (Errors e x) = Errors e x
--   recover x' (Fatal e) = Errors e x'
-- 
-- runErrors (Result x) = Right x
-- runErrors (Errors e x) = Left e
-- runErrors (Fatal e) = Left e
-- 
-- -- a (hopefully faster) Errors
-- newtype Errors' e a
--   = Errors' (forall r . (a -> r) -> (e -> a -> r) -> (e -> r) -> r)
-- 
-- unErrors' (Errors' p) = p
-- 
-- mapErrors' :: (e -> e') -> (a -> a') -> Errors' e a -> Errors' e' a'
-- mapErrors' f g (Errors' p) = Errors' p' where
--   p' result errors fatal = p (\x -> result (g x))
--                              (\e x -> errors (f e) (g x))
--                              (\e -> fatal (f e))
-- 
-- instance Functor (Errors' e) where
--   fmap f (Errors' p) = Errors' p' where
--     p' result errors fatal = p (\x -> result (f x))
--                                (\e x -> errors e (f x))
--                                fatal
-- 
-- instance (Monoid e, Error e) => Applicative (Errors' e) where
--   pure = return
--   (<*>) = ap 
--   
-- instance (Monoid e, Error e) => Monad (Errors' e) where
--   return x = Errors' p where
--     p result errors fatal = result x
-- 
--   Errors' p >>= f = Errors' p' where
--     p' result errors fatal = p (\x -> unErrors' (f x) 
--                                         result 
--                                         errors 
--                                         fatal)
--                                (\e x -> unErrors' (f x) 
--                                           (errors e)
--                                           (\e' y -> errors (mappend e e') y)
--                                           (\e' -> fatal (mappend e e')))
--                                fatal
--   
--   fail msg = Errors' p where
--     p result errors fatal = fatal (strMsg msg)
--   
-- instance (Monoid e, Error e) => MonadError e (Errors' e) where
--   throwError e = Errors' p where
--     p result errors fatal = fatal e
--   
--   catchError (Errors' p) h = Errors' p' where
--     p' result errors fatal = p result 
--                                (\e x -> unErrors' (h e) result errors fatal)
--                                (\e -> unErrors' (h e) result errors fatal)
-- 
-- instance (Monoid e, Error e) => MonadErrors e (Errors' e) where
--   annotate f (Errors' p) = Errors' p' where
--     p' result errors fatal = p result 
--                                (\e x -> errors (f e) x)
--                                (\e -> fatal (f e))
--   
--   recover x (Errors' p) = Errors' p' where
--     p' result errors fatal = p result errors (\e -> errors e x)
-- 
-- runErrors' (Errors' p) = p Right (\e x -> Left e) Left
-- 
-- -- The errors transformer
-- 
-- newtype ErrorsT e m a = ErrorsT (m (Errors e a))
-- 
-- unErrorsT (ErrorsT x) = x
-- 
-- mapErrorsT :: (m (Errors e a) -> m' (Errors e' a')) -> ErrorsT e m a -> ErrorsT e' m' a'
-- mapErrorsT f = ErrorsT . f . unErrorsT 
-- 
-- instance (Error e, Monoid e, Monad m) => Functor (ErrorsT e m) where
--   fmap = liftM
-- 
-- instance (Monoid e, Error e, Functor m, Monad m) => Applicative (ErrorsT e m) where
--   pure = return
--   (<*>) = ap
-- 
-- instance (Monoid e, Error e, Monad m) => Monad (ErrorsT e m) where
--   return x = {-# SCC "return_ErrorsT" #-} ErrorsT (return (return x))
--   
--   ErrorsT p >>= f = 
--     ErrorsT $ p >>= left where
--       left (Result x) = unErrorsT (f x)
--       left (Errors e x) = unErrorsT (f x) >>= right where 
--         right (Result y)    = return (Errors e y)
--         right (Errors e' y) = return (Errors (mappend e e') y)
--         right (Fatal e')    = return (Fatal (mappend e e'))
--       left (Fatal e) = return (Fatal e)
--     
--   fail = ErrorsT . return . fail
-- 
-- instance (Monoid e, Error e, Monad m) => MonadError e (ErrorsT e m) where
--   throwError = ErrorsT . return . Fatal
--   
--   ErrorsT p `catchError` h = ErrorsT $ p >>= foo where
--     foo (Result x) = return (Result x)
--     foo (Errors e a) = unErrorsT (h e)
--     foo (Fatal e) = unErrorsT (h e)
-- 
-- instance (Monoid e, Error e, Monad m) => MonadErrors e (ErrorsT e m) where
--   annotate f (ErrorsT p) = ErrorsT $ annotate f `liftM` p
--   recover x (ErrorsT p) = ErrorsT $ recover x `liftM` p
-- 
-- runErrorsT :: (Error e, Monad m) =>  ErrorsT e m a -> m (Either e a) 
-- runErrorsT (ErrorsT p) = liftM runErrors p
-- 
-- instance Monoid e => MonadTrans (ErrorsT e) where
--   lift = ErrorsT . liftM Result 
-- 
-- instance (Monoid e, Error e, MonadIO m) => MonadIO (ErrorsT e m) where
--   liftIO = lift . liftIO

instance (Monoid e, Error e, MonadErrors e m) => MonadErrors e (ReaderT r m) where
  annotate f = mapReaderT (annotate f)
  recover x = mapReaderT (recover x)

-- instance (Monoid e, Error e, MonadReader r m) => MonadReader r (ErrorsT e m) where
--   ask = lift ask
--   local f = mapErrorsT (local f)
-- 
-- instance (Monoid e, Error e, MonadState s m) => MonadState s (ErrorsT e m) where
--   get = lift get
--   put = lift . put

-- a (hopefully faster) alternative to ErrorsT

newtype ErrorsT' e m a
  =  ErrorsT' (forall r . (a -> m r) -> (e -> a -> m r) -> (e -> m r) -> m r)

unErrorsT' (ErrorsT' p) = p

instance Functor (ErrorsT' e m) where
  fmap f p = ErrorsT' p' where
    p' result errors fatal
      =  unErrorsT' p
           (\x    ->  result (f x))
           (\e x  ->  errors e (f x))
           fatal

returnErrorsT' :: a -> ErrorsT' e m a
returnErrorsT' x = ErrorsT' p where
  p result errors fatal
    = result x

instance Monoid e => Applicative (ErrorsT' e m) where
  pure x = returnErrorsT' x
  
  p <*> q = ErrorsT' r where
    r result errors fatal
      =  unErrorsT' p 
           (\f    ->  unErrorsT' q 
                        (\x     ->  result (f x))
                        (\e x   ->  errors e (f x))
                        fatal)
           (\e f  ->  unErrorsT' q
                        (\x     ->  errors e (f x))
                        (\e' x  ->  errors (mappend e e') (f x))
                        (\e'    ->  fatal (mappend e e')))
           fatal

instance (Error e, Monoid e) => Monad (ErrorsT' e m) where
  return = returnErrorsT'
  
  (>>=) = bindErrorsT'
  
  fail m = ErrorsT' p where
    p result errors fatal = fatal (strMsg m)

bindErrorsT' :: Monoid e => ErrorsT' e m a -> (a -> ErrorsT' e m b) -> ErrorsT' e m b
bindErrorsT' p f = ErrorsT' r where
    r result errors fatal
      =  unErrorsT' p
           (\x    ->  unErrorsT' (f x)
                        result
                        errors
                        fatal)
           (\e x  ->  unErrorsT' (f x)
                        (\x     ->  errors e x)
                        (\e' y  ->  errors (mappend e e') y)
                        (\e'    ->  fatal (mappend e e')))
           fatal    
    
instance (Error e, Monoid e) => MonadError e (ErrorsT' e m) where
  throwError e = ErrorsT' p where
    p result errors fatal = fatal e
  
  catchError p h = ErrorsT' p' where
    p' result errors fatal 
      =  unErrorsT' p
           result  
           (\e x  ->  unErrorsT' (h e) 
                        result 
                        errors 
                        fatal)
           (\e    ->  unErrorsT' (h e)
                        result
                        errors
                        fatal)

instance (Error e, Monoid e) => MonadErrors e (ErrorsT' e m) where
  annotate f p = ErrorsT' p' where
    p' result errors fatal
      =  unErrorsT' p
           result
           (\e x  ->  errors (f e) x)
           (\e    ->  fatal (f e))
  
  recover x p = ErrorsT' p' where
    p' result errors fatal
      =  unErrorsT' p
           result
           errors
           (\e -> errors e x)

runErrorsT' :: (Error e, Monad m) =>  ErrorsT' e m a -> m (Either e a) 
runErrorsT' p
  =  unErrorsT' p
       (\x    ->  return (Right x))
       (\e x  ->  return (Left e))
       (\e    ->  return (Left e))

instance MonadTrans (ErrorsT' e) where
  lift p = ErrorsT' p' where
    p' result errors fatal
      =  p >>= result

instance (Monoid e, Error e, MonadIO m) => MonadIO (ErrorsT' e m) where
  liftIO p = ErrorsT' p' where
    p' result errors fatal
      =  liftIO p >>= result

instance (Monoid e, Error e, MonadReader r m) => MonadReader r (ErrorsT' e m) where
  ask = lift ask
  local f p = ErrorsT' p' where
    p' result errors fatal
      =  unErrorsT' p
           (\x    ->  local f (result x))
           (\e x  ->  local f (errors e x))
           (\e    ->  local f (fatal e))

instance (Monoid e, Error e, MonadState s m) => MonadState s (ErrorsT' e m) where
  get = lift get
  put = lift . put
