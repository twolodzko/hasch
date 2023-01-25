module Eval (eval, evalEach, evalFile) where

import Envir (EnvRef, lookup)
import FileReader (new)
import Parser (parse)
import Text.Printf (printf)
import Types (Result (Err, Ok), Sexpr (..))

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
eval (List list) env = evalList list env
-- do
-- result <- evalList list env
-- return $ case result of
--   Ok val -> Ok val
--   Err msg -> Err $ printf "=> %s \n %s" (List list) msg
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

evalEach :: [Sexpr] -> Env -> [Sexpr] -> IO (Result [Sexpr])
evalEach (x : xs) env acc = do
  result <- eval x env
  case result of
    Ok v -> evalEach xs env (v : acc)
    Err msg -> return $ Err msg
evalEach [] _ acc =
  return $ Ok $ reverse acc

evalFile :: String -> Env -> IO (Result Sexpr)
evalFile name env = do
  reader <- FileReader.new name
  go reader Nil
  where
    go reader prev = do
      sexpr <- parse reader
      case sexpr of
        Ok (Just sexpr) -> do
          result <- eval sexpr env
          case result of
            Ok this -> go reader this
            Err msg -> return $ Err msg
        Ok Nothing -> return $ Ok prev
        Err msg -> return $ Err msg
