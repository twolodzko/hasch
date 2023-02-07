{-# LANGUAGE InstanceSigs #-}

module Numbers where

import Control.Exception (throw)
import Text.Printf (printf)
import Types (Error (NotANumber), Sexpr (Float, Int))

instance Num Sexpr where
  negate :: Sexpr -> Sexpr
  negate (Float x) = Float (-x)
  negate (Int x) = Int (-x)

  (+) :: Sexpr -> Sexpr -> Sexpr
  (+) (Int a) (Int b) = Int (a + b)
  (+) (Int a) (Float b) = Float (fromIntegral a + b)
  (+) (Float a) (Int b) = Float (a + fromIntegral b)
  (+) (Float a) (Float b) = Float (a + b)
  (+) a b = throw $ errNaN a b

  (-) :: Sexpr -> Sexpr -> Sexpr
  (-) (Int a) (Int b) = Int (a - b)
  (-) (Int a) (Float b) = Float (fromIntegral a - b)
  (-) (Float a) (Int b) = Float (a - fromIntegral b)
  (-) (Float a) (Float b) = Float (a - b)
  (-) a b = throw $ errNaN a b

  (*) :: Sexpr -> Sexpr -> Sexpr
  (*) (Int a) (Int b) = Int (a * b)
  (*) (Int a) (Float b) = Float (fromIntegral a * b)
  (*) (Float a) (Int b) = Float (a * fromIntegral b)
  (*) (Float a) (Float b) = Float (a * b)
  (*) a b = throw $ errNaN a b

  abs :: Sexpr -> Sexpr
  abs _ = undefined
  signum :: Sexpr -> Sexpr
  signum _ = undefined
  fromInteger :: Integer -> Sexpr
  fromInteger _ = undefined

instance Fractional Sexpr where
  (/) :: Sexpr -> Sexpr -> Sexpr
  (/) (Int a) (Int b) = Float (fromIntegral a / fromIntegral b)
  (/) (Int a) (Float b) = Float (fromIntegral a / b)
  (/) (Float a) (Int b) = Float (a / fromIntegral b)
  (/) (Float a) (Float b) = Float (a / b)
  (/) a b = throw $ errNaN a b

  fromRational :: Rational -> Sexpr
  fromRational _ = undefined

instance Ord Sexpr where
  compare :: Sexpr -> Sexpr -> Ordering
  compare (Int a) (Int b) = compare a b
  compare (Int a) (Float b) = compare (fromIntegral a) b
  compare (Float a) (Int b) = compare a (fromIntegral b)
  compare (Float a) (Float b) = compare a b
  compare a b = throw $ errNaN a b

errNaN :: Sexpr -> Sexpr -> Error
errNaN (Int _) b = NotANumber b
errNaN (Float _) b = NotANumber b
errNaN a (Int _) = NotANumber a
errNaN a (Float _) = NotANumber a
errNaN a _ = NotANumber a
