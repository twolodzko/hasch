module Main where

import Envir (EnvRef)
import Eval (evalFile)
import Repl (loop)
import Scheme (root)
import StdinReader (new)
import System.Environment (getArgs)
import Text.Printf (printf)
import Types (Result (Err, Ok), Sexpr)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then repl
    else do
      env <- root
      evalFiles args env

repl :: IO ()
repl = do
  printf "Press ^C to exit.\n\n"
  env <- root
  reader <- StdinReader.new
  loop reader env

evalFiles :: [String] -> EnvRef Sexpr -> IO ()
evalFiles (x : xs) env = do
  result <- Eval.evalFile x env
  case result of
    Ok _ -> evalFiles xs env
    Err msg -> printf "Error: %s\n" msg
evalFiles [] _ =
  return ()
