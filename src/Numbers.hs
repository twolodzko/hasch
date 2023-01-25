{-# LANGUAGE InstanceSigs #-}

module Numbers where

import Control.Exception (Exception, throw)
import Text.Printf (printf)
import Types (Sexpr (Float, Int))

newtype NaN = NaN (Sexpr, Sexpr)

instance Exception NaN

instance Show NaN where
  show :: NaN -> String
  show (NaN (a, b)) = printf "operation cannot be applied to %s and %s" a b

instance Num Sexpr where
  negate :: Sexpr -> Sexpr
  negate (Float x) = Float (-x)
  negate (Int x) = Int (-x)

  (+) :: Sexpr -> Sexpr -> Sexpr
  (+) (Int a) (Int b) = Int (a + b)
  (+) (Int a) (Float b) = Float (fromIntegral a + b)
  (+) (Float a) (Int b) = Float (a + fromIntegral b)
  (+) (Float a) (Float b) = Float (a + b)
  (+) a b = throw $ NaN (a, b)

  (-) :: Sexpr -> Sexpr -> Sexpr
  (-) (Int a) (Int b) = Int (a - b)
  (-) (Int a) (Float b) = Float (fromIntegral a - b)
  (-) (Float a) (Int b) = Float (a - fromIntegral b)
  (-) (Float a) (Float b) = Float (a - b)
  (-) a b = throw $ NaN (a, b)

  (*) :: Sexpr -> Sexpr -> Sexpr
  (*) (Int a) (Int b) = Int (a * b)
  (*) (Int a) (Float b) = Float (fromIntegral a * b)
  (*) (Float a) (Int b) = Float (a * fromIntegral b)
  (*) (Float a) (Float b) = Float (a * b)
  (*) a b = throw $ NaN (a, b)

  abs :: Sexpr -> Sexpr
  abs _ = error "undefined"
  signum :: Sexpr -> Sexpr
  signum _ = error "undefined"
  fromInteger :: Integer -> Sexpr
  fromInteger _ = error "undefined"

instance Fractional Sexpr where
  (/) :: Sexpr -> Sexpr -> Sexpr
  (/) (Int a) (Int b) = Float (fromIntegral a / fromIntegral b)
  (/) (Int a) (Float b) = Float (fromIntegral a / b)
  (/) (Float a) (Int b) = Float (a / fromIntegral b)
  (/) (Float a) (Float b) = Float (a / b)
  (/) a b = throw $ NaN (a, b)

  fromRational :: Rational -> Sexpr
  fromRational _ = error "undefined"

instance Ord Sexpr where
  compare :: Sexpr -> Sexpr -> Ordering
  compare (Int a) (Int b) = compare a b
  compare (Int a) (Float b) = compare (fromIntegral a) b
  compare (Float a) (Int b) = compare a (fromIntegral b)
  compare (Float a) (Float b) = compare a b
  compare a b = throw $ NaN (a, b)
