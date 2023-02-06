module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (runExceptT)
import Envir (EnvRef)
import Eval (evalFile)
import Repl (loop)
import Scheme (root)
import StdinReader (new)
import System.Environment (getArgs)
import Text.Printf (printf)
import Types (Sexpr)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then repl
    else evalFiles args =<< root

repl :: IO ()
repl = do
  printf "Press ^C to exit.\n\n"
  reader <- StdinReader.new
  loop reader =<< root

evalFiles :: [String] -> EnvRef Sexpr -> IO ()
evalFiles (x : xs) env = do
  result <- liftIO $ runExceptT $ Eval.evalFile x env
  case result of
    Right _ -> evalFiles xs env
    Left msg -> printf "Error: %s\n" msg
evalFiles [] _ = return ()
