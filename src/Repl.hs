module Repl (loop) where

import Control.Monad.Trans.Except (runExceptT)
import Envir (EnvRef)
import Eval (eval)
import Parser (parse)
import StdinReader (StdinReader)
import Text.Printf (printf)
import Types (Error (..), Sexpr, printErr)

type Env = EnvRef Sexpr

loop :: StdinReader -> Env -> IO ()
loop r env = do
  result <- runExceptT $ Parser.parse r
  case result of
    Right (Just sexpr) -> do
      evalAndPrint sexpr env
      loop r env
    Right Nothing ->
      loop r env
    Left msg -> do
      printErr msg
      loop r env

evalAndPrint :: Sexpr -> Env -> IO ()
evalAndPrint sexpr env = do
  result <- runExceptT $ eval sexpr env
  case result of
    Right sexpr -> print sexpr
    Left msg -> printErr msg
