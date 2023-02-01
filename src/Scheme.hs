{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant compare" #-}

module Scheme (root) where

import Control.Exception (evaluate, try)
import Data.List (group)
import Envir (EnvRef, branch, findEnv, fromList, insert)
import Eval (eval, evalEach, evalFile)
import Numbers (NaN)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Types (Result (Err, Ok), Sexpr (..))

type Env = EnvRef Sexpr

root :: IO (EnvRef Sexpr)
root =
  Envir.fromList
    [ ("-", Func $ numReduce (-) (Int 0)),
      ("*", Func $ numReduce (*) (Int 1)),
      ("/", Func $ numReduce (/) (Float 1)),
      ("+", Func $ numReduce (+) (Int 0)),
      ("<", Func $ numCompare (<)),
      ("=", Func $ numCompare $ \a b -> compare a b == EQ), -- this is needed so we don't use Eq but Ord
      (">", Func $ numCompare (>)),
      ("->integer", Func $ evalEachAnd $ oneArg toInt),
      ("->float", Func $ evalEachAnd $ oneArg toFloat),
      ("and", Func $ evalEachAnd $ Ok . andFn),
      ("begin", Func $ evalEachAnd $ Ok . lastOrNil),
      ("bool?", Func $ evalEachAnd $ oneArg isBool),
      ("car", Func $ evalEachAnd $ oneArg car),
      ("cdr", Func $ evalEachAnd $ oneArg cdr),
      ("cond", Func cond),
      ("cons", Func cons),
      ("define", Func define),
      ("display", Func display),
      ("else", Bool True),
      ("eq?", Func equal),
      ("equal?", Func equal),
      ("error", Func $ evalEachAnd $ Err . toString),
      ("eval", Func evalFn),
      ("float?", Func $ evalEachAnd $ oneArg isFloat),
      ("if", Func ifFn),
      ("integer?", Func $ evalEachAnd $ oneArg isInt),
      ("lambda", Func lambda),
      ("let", Func letFn),
      ("let*", Func letStarFn),
      ("list", Func $ evalEachAnd $ Ok . List),
      ("load", Func load),
      ("not", Func $ evalEachAnd $ oneArg $ Ok . Bool . not . isTrue),
      ("null?", Func $ evalEachAnd $ oneArg isNull),
      ("number?", Func $ evalEachAnd $ oneArg isNumber),
      ("or", Func $ evalEachAnd $ Ok . orFn),
      ("pair?", Func $ evalEachAnd $ oneArg isPair),
      ("procedure?", Func $ evalEachAnd $ oneArg isProcedure),
      ("quote", Func quote),
      ("set!", Func setBang),
      ("string?", Func $ evalEachAnd $ oneArg isString),
      ("string", Func $ evalEachAnd $ Ok . String . toString),
      ("symbol?", Func $ evalEachAnd $ oneArg isSymbol)
    ]

quote :: [Sexpr] -> Env -> IO (Result Sexpr)
quote [sexpr] _ = return $ Ok sexpr
quote args _ = return $ Err $ wrongArgNum args

car :: Sexpr -> Result Sexpr
car (List list) = Ok $ head list
car sexpr = Err $ wrongArg sexpr

cdr :: Sexpr -> Result Sexpr
cdr (List list) = Ok $ List $ tail list
cdr sexpr = Err $ wrongArg sexpr

cons :: [Sexpr] -> Env -> IO (Result Sexpr)
cons (lhs : [rhs]) env = do
  left <- eval lhs env
  left ?> \l -> do
    right <- eval rhs env
    right ?> \r ->
      return $ Ok $ List (l : list r)
  where
    list (List s) = s
    list s = [s]
cons args _ =
  return $ Err $ wrongArgNum args

define :: [Sexpr] -> Env -> IO (Result Sexpr)
define ((Symbol k) : [v]) env = do
  result <- eval v env
  result ?> \v -> do
    Envir.insert k v env
    return $ Ok v
define (k : [_]) _ =
  return $ Err $ notASymbol k
define args _ =
  return $ Err $ wrongArgNum args

setBang :: [Sexpr] -> Env -> IO (Result Sexpr)
setBang ((Symbol k) : [v]) env = do
  result <- Envir.findEnv k env
  case result of
    Just env -> do
      result <- eval v env
      result ?> \v -> do
        Envir.insert k v env
        return $ Ok v
    Nothing -> return $ Err $ printf "%s was not defined" k
setBang (k : [_]) _ =
  return $ Err $ notASymbol k
setBang args _ =
  return $ Err $ wrongArgNum args

lambda :: [Sexpr] -> Env -> IO (Result Sexpr)
lambda (List vars : body) parentEnv =
  return $ case extractVars vars [] of
    Ok vars -> Ok $ Func $ \args callEnv -> do
      local <- Envir.branch parentEnv
      lambdaInit vars args callEnv local
      evalEachAnd (Ok . lastOrNil) body local
    Err msg -> Err msg
lambda (sexpr : _) _ = return $ Err $ wrongArg sexpr
lambda args _ = return $ Err $ wrongArgNum args

extractVars :: [Sexpr] -> [String] -> Result [String]
extractVars (Symbol x : xs) acc = extractVars xs (x : acc)
extractVars [] acc = Ok $ reverse acc
extractVars (sexpr : _) _ = Err $ notASymbol sexpr

lambdaInit :: [String] -> [Sexpr] -> Env -> EnvRef Sexpr -> IO (Result Sexpr)
lambdaInit (v : vars) (a : args) evalEnv saveEnv = do
  result <- eval a evalEnv
  result ?> \x -> do
    Envir.insert v x saveEnv
    lambdaInit vars args evalEnv saveEnv
lambdaInit [] [] _ _ = return $ Ok Nil

letFn :: [Sexpr] -> Env -> IO (Result Sexpr)
letFn args env =
  letImpl args env =<< Envir.branch env

letStarFn :: [Sexpr] -> Env -> IO (Result Sexpr)
letStarFn args env = do
  local <- Envir.branch env
  letImpl args local local

letImpl :: [Sexpr] -> Env -> Env -> IO (Result Sexpr)
letImpl (List list : body) evalEnv saveEnv = do
  result <- letInit list evalEnv saveEnv
  result ?> \_ ->
    (evalEachAnd $ Ok . lastOrNil) body saveEnv
letImpl (sexpr : _) _ _ =
  return $ Err $ wrongArg sexpr
letImpl _ _ _ =
  return $ Err "invalid arguments"

letInit :: [Sexpr] -> Env -> EnvRef Sexpr -> IO (Result Sexpr)
letInit (List (Symbol key : [val]) : xs) evalEnv saveEnv = do
  result <- eval val evalEnv
  result ?> \s -> do
    Envir.insert key s saveEnv
    letInit xs evalEnv saveEnv
letInit (sexpr : _) _ _ =
  return $ Err $ notASymbol sexpr
letInit [] _ _ =
  return $ Ok Nil

display :: [Sexpr] -> Env -> IO (Result Sexpr)
display args env = do
  result <- evalEach args env []
  case result of
    Ok l -> do
      printf "%s\n" $ toString l
      return $ Ok Nil
    Err msg -> return $ Err msg

evalFn :: [Sexpr] -> Env -> IO (Result Sexpr)
evalFn [sexpr] env = do
  result <- eval sexpr env
  result ?> \s -> eval s env

ifFn :: [Sexpr] -> Env -> IO (Result Sexpr)
ifFn (cond : ifTrue : [ifFalse]) env = do
  result <- eval cond env
  result ?> go
  where
    go c | isTrue c = eval ifTrue env
    go _ = eval ifFalse env
ifFn args _ = return $ Err $ wrongArgNum args

cond :: [Sexpr] -> Env -> IO (Result Sexpr)
cond ((List (condition : body)) : xs) env = do
  result <- eval condition env
  result ?> go
  where
    go s | isTrue s =
      case body of
        [] -> return $ Ok s
        body -> (evalEachAnd $ Ok . last) body env
    go s = cond xs env
cond (sexpr : _) _ = return $ Err $ wrongArg sexpr
cond [] _ = return $ Ok Nil

andFn :: [Sexpr] -> Sexpr
andFn [x] = x
andFn (x : xs) | isTrue x = andFn xs
andFn (_ : _) = Bool False
andFn [] = Bool True

orFn :: [Sexpr] -> Sexpr
orFn (x : _) | isTrue x = x
orFn (_ : xs) = orFn xs
orFn [] = Bool False

isBool :: Sexpr -> Result Sexpr
isBool (Bool _) = Ok $ Bool True
isBool _ = Ok $ Bool False

isNumber :: Sexpr -> Result Sexpr
isNumber (Int _) = Ok $ Bool True
isNumber (Float _) = Ok $ Bool True
isNumber _ = Ok $ Bool False

isInt :: Sexpr -> Result Sexpr
isInt (Int _) = Ok $ Bool True
isInt _ = Ok $ Bool False

isFloat :: Sexpr -> Result Sexpr
isFloat (Float _) = Ok $ Bool True
isFloat _ = Ok $ Bool False

isString :: Sexpr -> Result Sexpr
isString (String _) = Ok $ Bool True
isString _ = Ok $ Bool False

isSymbol :: Sexpr -> Result Sexpr
isSymbol (Symbol _) = Ok $ Bool True
isSymbol _ = Ok $ Bool False

isNull :: Sexpr -> Result Sexpr
isNull (List []) = Ok $ Bool True
isNull _ = Ok $ Bool False

isPair :: Sexpr -> Result Sexpr
isPair (List []) = Ok $ Bool False
isPair (List _) = Ok $ Bool True
isPair _ = Ok $ Bool False

isProcedure :: Sexpr -> Result Sexpr
isProcedure (Func _) = Ok $ Bool True
isProcedure _ = Ok $ Bool False

toInt :: Sexpr -> Result Sexpr
toInt (Float num) = Ok $ Int $ round num
toInt (Int num) = Ok $ Int num
toInt (String str) =
  case (readMaybe str :: Maybe Int) of
    Just num -> Ok $ Int num
    Nothing -> Err $ printf "cannot parse: %s" str
toInt sexpr = Err $ wrongArg sexpr

toFloat :: Sexpr -> Result Sexpr
toFloat (Int num) = Ok $ Float $ fromIntegral num
toFloat (Float num) = Ok $ Float num
toFloat (String str) =
  case (readMaybe str :: Maybe Float) of
    Just num -> Ok $ Float num
    Nothing -> Err $ printf "cannot parse: %s" str
toFloat sexpr = Err $ wrongArg sexpr

allEqual :: [Sexpr] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual xs = all (== head xs) (tail xs)

equal :: [Sexpr] -> Env -> IO (Result Sexpr)
equal = evalEachAnd $ Ok . Bool . allEqual

numReduce :: (Sexpr -> Sexpr -> Sexpr) -> Sexpr -> [Sexpr] -> Env -> IO (Result Sexpr)
numReduce _ init [] _ = return $ Ok init
numReduce op init [x] env = numReduceImpl op [x] init env
numReduce op _ (x : xs) env = do
  result <- eval x env
  result ?> \acc -> numReduceImpl op xs acc env

numReduceImpl :: (Sexpr -> Sexpr -> Sexpr) -> [Sexpr] -> Sexpr -> Env -> IO (Result Sexpr)
numReduceImpl op (x : xs) acc env = do
  this <- eval x env
  this ?> \v -> do
    result <- try (evaluate $ op acc v) :: IO (Either NaN Sexpr)
    case result of
      Left err -> return $ Err $ show err
      Right new -> numReduceImpl op xs new env
numReduceImpl _ [] acc _ = return $ Ok acc

numCompare :: (Sexpr -> Sexpr -> Bool) -> [Sexpr] -> Env -> IO (Result Sexpr)
numCompare _ [] _ = return $ Ok $ Bool True
numCompare _ [x] env = do
  result <- eval x env
  result ?> \_ -> return $ Ok $ Bool True
numCompare op (x : xs) env = do
  result <- eval x env
  result ?> \v -> go xs v
  where
    go (x : xs) acc = do
      this <- eval x env
      this ?> \v -> do
        result <- try (evaluate $ op acc v) :: IO (Either NaN Bool)
        case result of
          Left err -> return $ Err $ show err
          Right True -> go xs v
          Right False -> return $ Ok $ Bool False
    go [] acc = return $ Ok $ Bool True

load :: [Sexpr] -> Env -> IO (Result Sexpr)
load [arg] env = do
  name <- eval arg env
  name ?> go
  where
    go (String name) = evalFile name env
    go sexpr = return $ Err $ wrongArg sexpr
load args _ = return $ Err $ wrongArgNum args

toString :: [Sexpr] -> String
toString = unwords . map show

(?>) :: Result Sexpr -> (Sexpr -> IO (Result Sexpr)) -> IO (Result Sexpr)
(?>) (Ok x) f = f x
(?>) (Err msg) _ = return $ Err msg

lastOrNil :: [Sexpr] -> Sexpr
lastOrNil [] = Nil
lastOrNil list = last list

oneArg :: (Sexpr -> Result Sexpr) -> [Sexpr] -> Result Sexpr
oneArg f [sexpr] = f sexpr
oneArg _ args = Err $ wrongArgNum args

evalEachAnd :: ([Sexpr] -> Result Sexpr) -> [Sexpr] -> Env -> IO (Result Sexpr)
evalEachAnd f args env = do
  result <- evalEach args env []
  return $ case result of
    Ok v -> f v
    Err err -> Err err

wrongArgNum :: [Sexpr] -> String
wrongArgNum args = printf "wrong number of arguments: %d" $ length args

wrongArg :: Sexpr -> String
wrongArg = printf "invalid argument: %s"

notASymbol :: Sexpr -> String
notASymbol = printf "%s is not a symbol"

isTrue :: Sexpr -> Bool
isTrue (Bool False) = False
isTrue _ = True
