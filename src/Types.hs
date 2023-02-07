{-# LANGUAGE InstanceSigs #-}

module Types (Sexpr (..), Error (..), Result, printErr) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (intercalate)
import Envir (EnvRef)
import Text.Printf (FieldFormatter, PrintfArg, formatArg, printf)

type Result = ExceptT Error IO Sexpr

data Sexpr
  = Bool Bool
  | Int Int
  | Float Float
  | String String
  | Symbol String
  | Quote Sexpr
  | List [Sexpr]
  | Func ([Sexpr] -> EnvRef Sexpr -> Result)
  | Nil

instance Show Sexpr where
  show :: Sexpr -> String
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Int v) = show v
  show (Float v) = show v
  show (String v) = show v
  show (Symbol v) = printf "%s" v
  show (Quote v) = printf "'%s" v
  show (List list) = printf "(%s)" $ unwords $ map show list
  show (Func _) = "<Func>"
  show Nil = "<nil>"

instance PrintfArg Sexpr where
  formatArg :: Sexpr -> FieldFormatter
  formatArg sexpr = formatArg (show sexpr)

instance Eq Sexpr where
  (==) :: Sexpr -> Sexpr -> Bool
  (==) (Bool a) (Bool b) = a == b
  (==) (Int a) (Int b) = a == b
  (==) (Float a) (Float b) = a == b
  (==) (String a) (String b) = a == b
  (==) (Symbol a) (Symbol b) = a == b
  (==) (Quote a) (Quote b) = a == b
  (==) (List a) (List b) = a == b
  (==) Nil Nil = True
  (==) _ _ = False

data Error
  = WrongArg Sexpr
  | NotASymbol Sexpr
  | NotANumber Sexpr
  | CannotParse String
  | WrongArgNum Int
  | InvalidArgs
  | Undefined String
  | NotCallable Sexpr
  | ParserUnexpected String
  | ParserMissing String
  | NotImplemented
  | CustomErr String
  | Traceback [Sexpr] Error
  deriving (Eq)

instance Exception Error

instance Show Error where
  show :: Error -> String
  show (WrongArg s) = printf "invalid argument: %s" s
  show (NotASymbol s) = printf "%s is not a symbol" s
  show (NotANumber s) = printf "%s is not a number" s
  show (CannotParse s) = printf "cannot parse: %s" s
  show (WrongArgNum n) = printf "wrong number of arguments: %d" n
  show InvalidArgs = "invalid arguments"
  show (Undefined s) = printf "%s was not defined" s
  show (NotCallable s) = printf "%s is not callable" s
  show (ParserUnexpected s) = printf "unexpected '%s'" s
  show (ParserMissing s) = printf "missing '%s'" s
  show NotImplemented = "not implemented"
  show (CustomErr s) = s
  show (Traceback trace err) =
    let elems = "Traceback:" : map show trace ++ ["Error:"]
     in printf "%s %s" (intercalate " \n ↪ " elems) err

instance PrintfArg Error where
  formatArg :: Error -> FieldFormatter
  formatArg msg = formatArg (show msg)

printErr :: Error -> IO ()
printErr err@(Traceback _ _) = printf "%s\n" $ show err
printErr err = printf "Error: %s\n" err
