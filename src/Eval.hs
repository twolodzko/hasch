module Eval (eval, evalEach, evalFile) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Envir (EnvRef, lookup)
import FileReader (new)
import Parser (parse)
import Text.Printf (printf)
import Types (Error (..), Result, Sexpr (..))

type Env = EnvRef Sexpr

eval :: Sexpr -> Env -> Result
eval (Symbol name) env = do
  result <- liftIO $ Envir.lookup name env
  case result of
    Just val -> return val
    Nothing -> throwE $ Undefined name
eval (Quote sexpr) _ =
  return sexpr
eval (List list) env = do
  result <- liftIO $ runExceptT $ evalList list env
  case result of
    Right val -> return val
    Left msg -> withTraceback msg list
eval sexpr _ =
  return sexpr

evalList :: [Sexpr] -> Env -> Result
evalList [] _ =
  return $ List []
evalList (x : xs) env = do
  fn <- liftIO $ runExceptT $ eval x env
  case fn of
    Right (Func f) ->
      f xs env
    Right sexpr ->
      throwE $ NotCallable sexpr
    Left msg ->
      throwE msg

evalEach :: [Sexpr] -> Env -> ExceptT Error IO [Sexpr]
evalEach args env =
  go args []
  where
    go (x : xs) acc =
      eval x env >>= \v ->
        go xs (v : acc)
    go [] acc =
      return $ reverse acc

evalFile :: String -> Env -> Result
evalFile name env = do
  reader <- liftIO $ FileReader.new name
  go reader Nil
  where
    go reader prev = do
      sexpr <- liftIO $ runExceptT $ parse reader
      case sexpr of
        Right (Just sexpr) ->
          eval sexpr env >>= go reader
        Right Nothing -> return prev
        Left msg -> throwE msg

withTraceback :: Error -> [Sexpr] -> Result
withTraceback (Traceback trace err) caller =
  throwE $ Traceback (List caller : trace) err
withTraceback err caller =
  throwE $ Traceback [List caller] err
