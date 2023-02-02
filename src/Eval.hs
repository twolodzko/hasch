module Eval (eval, evalEach, evalFile) where

import Envir (EnvRef, lookup)
import FileReader (new)
import Parser (parse)
import Result (Result (..), (>>>=))
import Text.Printf (printf)
import Types (Sexpr (..))

type EvalResult = IO (Result Sexpr)

type Env = EnvRef Sexpr

eval :: Sexpr -> Env -> EvalResult
eval (Symbol name) env = do
  result <- Envir.lookup name env
  return $ case result of
    Just val -> Ok val
    Nothing -> Err $ printf "%s was not found" name
eval (Quote sexpr) _ =
  return $ Ok sexpr
eval (List list) env = do
  result <- evalList list env
  return $ case result of
    Ok val -> Ok val
    Err msg -> withTraceback msg list
eval sexpr _ =
  return $ Ok sexpr

evalList :: [Sexpr] -> Env -> EvalResult
evalList [] _ =
  return $ Ok $ List []
evalList (x : xs) env = do
  fn <- eval x env
  case fn of
    Ok (Func f) ->
      f xs env
    Ok sexpr ->
      return $ Err $ printf "%s is not callable" sexpr
    Err msg ->
      return $ Err msg

evalEach :: [Sexpr] -> Env -> IO (Result [Sexpr])
evalEach args env =
  go args []
  where
    go (x : xs) acc =
      eval x env >>>= \v ->
        go xs (v : acc)
    go [] acc =
      return $ Ok $ reverse acc

evalFile :: String -> Env -> IO (Result Sexpr)
evalFile name env = do
  reader <- FileReader.new name
  go reader Nil
  where
    go reader prev = do
      sexpr <- parse reader
      case sexpr of
        Ok (Just sexpr) ->
          eval sexpr env >>>= go reader
        Ok Nothing -> return $ Ok prev
        Err msg -> return $ Err msg

withTraceback :: String -> [Sexpr] -> Result Sexpr
withTraceback msg caller =
  Err $ printf "%s \n ↪ %s" (List caller) msg
