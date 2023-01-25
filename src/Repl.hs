module Repl (loop) where

import Envir (EnvRef)
import Eval (eval)
import Parser (parse)
import StdinReader (StdinReader)
import Text.Printf (printf)
import Types (Result (Err, Ok), Sexpr)

type Env = EnvRef Sexpr

loop :: StdinReader -> Env -> IO ()
loop r env = do
  result <- Parser.parse r
  case result of
    Ok (Just sexpr) -> do
      evalAndPrint sexpr env
      loop r env
    Ok Nothing ->
      loop r env
    Err msg -> do
      printErr msg
      loop r env

evalAndPrint :: Sexpr -> Env -> IO ()
evalAndPrint sexpr env = do
  result <- eval sexpr env
  case result of
    Ok sexpr -> print sexpr
    Err msg -> printErr msg

printErr :: String -> IO ()
printErr = printf "Error: %s\n"
