{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Result (Result (..), (>>>=), isErr, ResultT (runResultT), throwError) where

data Result t
  = Ok t
  | Err String
  deriving (Eq, Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f (Ok x) = Ok (f x)
  fmap _ (Err msg) = Err msg

instance Applicative Result where
  pure :: a -> Result a
  pure = Ok

  (<*>) :: Result (a -> b) -> Result a -> Result b
  Ok a <*> Ok b = Ok (a b)
  _ <*> Err msg = Err msg
  Err msg <*> _ = Err msg

instance Monad Result where
  (>>=) :: Result a -> (a -> Result b) -> Result b
  Ok a >>= f = f a
  Err msg >>= _ = Err msg

(>>>=) :: IO (Result a) -> (a -> IO (Result b)) -> IO (Result b)
x >>>= f =
  x >>= go f
  where
    go _ (Err msg) = return $ Err msg
    go f (Ok x) = f x

isErr :: Result t -> Bool
isErr (Err _) = True
isErr (Ok _) = False

-----------------------------------------------------------------------------
-- Monad transform
-----------------------------------------------------------------------------

newtype ResultT m a = ResultT {runResultT :: m (Result a)} deriving (Functor)

instance Monad m => Applicative (ResultT m) where
  pure :: Monad m => a -> ResultT m a
  pure = ResultT . return . Ok

  (<*>) :: Monad m => ResultT m (a -> b) -> ResultT m a -> ResultT m b
  ResultT wa <*> ResultT wb = ResultT $ do
    a <- wa
    case a of
      Err msg -> return $ Err msg
      Ok f -> do
        b <- wb
        case b of
          Err msg -> return $ Err msg
          Ok b -> return $ Ok $ f b

instance Monad m => Monad (ResultT m) where
  (>>=) :: Monad m => ResultT m a -> (a -> ResultT m b) -> ResultT m b
  wrapped >>= f = ResultT $ do
    result <- runResultT wrapped
    case result of
      Err msg -> return $ Err msg
      Ok x -> runResultT $ f x

-- instance MonadTrans ResultT where
--   lift = ResultT . fmap Ok

throwError :: Monad m => String -> ResultT m a
throwError = ResultT . return . Err
