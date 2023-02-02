{-# LANGUAGE InstanceSigs #-}

module Types (Sexpr (..), Result (..), (>>>=), isErr) where

import Envir (EnvRef)
import Text.Printf (FieldFormatter, PrintfArg, formatArg, printf)

data Sexpr
  = Bool Bool
  | Int Int
  | Float Float
  | String String
  | Symbol String
  | Quote Sexpr
  | List [Sexpr]
  | Func ([Sexpr] -> EnvRef Sexpr -> IO (Result Sexpr))
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

data Result t
  = Ok t
  | Err String
  deriving (Eq, Show)

(>>>=) :: IO (Result a) -> (a -> IO (Result b)) -> IO (Result b)
x >>>= f =
  x >>= go f
  where
    go f (Ok x) = f x
    go _ (Err msg) = return $ Err msg

isErr :: Result t -> Bool
isErr (Err _) = True
isErr (Ok _) = False
