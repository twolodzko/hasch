{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant compare" #-}
module Scheme (root) where

import Control.Exception (evaluate, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bifunctor (second)
import Data.List (group)
import Envir (EnvRef, branch, findEnv, fromList, insert)
import Eval (eval, evalEach, evalFile)
import Numbers (NaN)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Types (Error, Result, Sexpr (..))

type Env = EnvRef Sexpr

root :: IO (EnvRef Sexpr)
root = do
  env <-
    Envir.fromList $
      map
        (second Func)
        [ ("-", numReduce (-) (Int 0)),
          ("*", numReduce (*) (Int 1)),
          ("/", numReduce (/) (Float 1)),
          ("+", numReduce (+) (Int 0)),
          ("<", numCompare (<)),
          ("=", numCompare $ \a b -> compare a b == EQ), -- this is needed so we don't use Eq but Ord
          (">", numCompare (>)),
          ("->integer", evalEachAnd $ oneArg toInt),
          ("->float", evalEachAnd $ oneArg toFloat),
          ("and", evalEachAnd $ return . andFn),
          ("begin", evalEachAnd $ return . lastOrNil),
          ("bool?", evalEachAnd $ oneArg isBool),
          ("car", evalEachAnd $ oneArg car),
          ("cdr", evalEachAnd $ oneArg cdr),
          ("cond", cond),
          ("cons", evalEachAnd cons),
          ("define", define),
          ("display", display),
          ("eq?", equal),
          ("equal?", equal),
          ("error", evalEachAnd $ Left . toString),
          ("eval", evalFn),
          ("float?", evalEachAnd $ oneArg isFloat),
          ("if", ifFn),
          ("integer?", evalEachAnd $ oneArg isInt),
          ("lambda", lambda),
          ("let", letFn),
          ("let*", letStarFn),
          ("list", evalEachAnd $ return . List),
          ("load", load),
          ("not", evalEachAnd $ oneArg $ return . Bool . not . isTrue),
          ("null?", evalEachAnd $ oneArg isNull),
          ("number?", evalEachAnd $ oneArg isNumber),
          ("or", evalEachAnd $ return . orFn),
          ("pair?", evalEachAnd $ oneArg isPair),
          ("procedure?", evalEachAnd $ oneArg isProcedure),
          ("quote", quote),
          ("set!", setBang),
          ("string?", evalEachAnd $ oneArg isString),
          ("string", evalEachAnd $ return . String . toString),
          ("symbol?", evalEachAnd $ oneArg isSymbol)
        ]
  Envir.insert "else" (Bool True) env
  return env

quote :: [Sexpr] -> Env -> Result
quote [sexpr] _ = return sexpr
quote args _ = throwE $ wrongArgNum args

car :: Sexpr -> Either Error Sexpr
car (List list) = Right $ head list
car sexpr = Left $ wrongArg sexpr

cdr :: Sexpr -> Either Error Sexpr
cdr (List list) = Right $ List $ tail list
cdr sexpr = Left $ wrongArg sexpr

cons :: [Sexpr] -> Either Error Sexpr
cons (lhs : [List rhs]) = Right $ List (lhs : rhs)
cons (lhs : [rhs]) = Right $ List (lhs : [rhs])
cons args = Left $ wrongArgNum args

define :: [Sexpr] -> Env -> Result
define ((Symbol k) : [v]) env =
  eval v env >>= \v ->
    liftIO $ Envir.insert k v env
define (k : [_]) _ =
  throwE $ notASymbol k
define args _ =
  throwE $ wrongArgNum args

setBang :: [Sexpr] -> Env -> Result
setBang ((Symbol k) : [v]) env = do
  result <- liftIO $ Envir.findEnv k env
  case result of
    Just env ->
      eval v env >>= \v ->
        liftIO $ Envir.insert k v env
    Nothing -> throwE $ printf "%s was not defined" k
setBang (k : [_]) _ =
  throwE $ notASymbol k
setBang args _ =
  throwE $ wrongArgNum args

lambda :: [Sexpr] -> Env -> Result
lambda (List vars : body) parentEnv = do
  liftE (extractVars vars [] >>= Right . Func . go)
  where
    go vars args env = do
      local <- liftIO $ Envir.branch parentEnv
      lambdaInit vars args env local
      evalEachAnd (return . lastOrNil) body local
lambda (sexpr : _) _ = throwE $ wrongArg sexpr
lambda args _ = throwE $ wrongArgNum args

extractVars :: [Sexpr] -> [String] -> Either Error [String]
extractVars (Symbol x : xs) acc = extractVars xs (x : acc)
extractVars [] acc = Right $ reverse acc
extractVars (sexpr : _) _ = Left $ notASymbol sexpr

lambdaInit :: [String] -> [Sexpr] -> Env -> EnvRef Sexpr -> Result
lambdaInit (v : vars) (a : args) evalEnv saveEnv =
  eval a evalEnv >>= \x -> do
    liftIO $ Envir.insert v x saveEnv
    lambdaInit vars args evalEnv saveEnv
lambdaInit [] [] _ _ = return Nil

letFn :: [Sexpr] -> Env -> Result
letFn args env = do
  local <- liftIO $ Envir.branch env
  letImpl args env local

letStarFn :: [Sexpr] -> Env -> Result
letStarFn args env = do
  local <- liftIO $ Envir.branch env
  letImpl args local local

letImpl :: [Sexpr] -> Env -> Env -> Result
letImpl (List list : body) evalEnv saveEnv =
  letInit list evalEnv saveEnv
    >> (evalEachAnd $ return . lastOrNil) body saveEnv
letImpl (sexpr : _) _ _ =
  throwE $ wrongArg sexpr
letImpl _ _ _ =
  throwE "invalid arguments"

letInit :: [Sexpr] -> Env -> EnvRef Sexpr -> Result
letInit (List (Symbol key : [val]) : xs) evalEnv saveEnv =
  eval val evalEnv >>= \v -> do
    liftIO $ Envir.insert key v saveEnv
    letInit xs evalEnv saveEnv
letInit (sexpr : _) _ _ =
  throwE $ notASymbol sexpr
letInit [] _ _ =
  return Nil

display :: [Sexpr] -> Env -> Result
display args env =
  evalEach args env >>= \x -> do
    liftIO $ printf "%s\n" $ toString x
    return Nil

evalFn :: [Sexpr] -> Env -> Result
evalFn [sexpr] env =
  eval sexpr env >>= \x ->
    eval x env

ifFn :: [Sexpr] -> Env -> Result
ifFn (cond : ifTrue : [ifFalse]) env =
  eval cond env >>= \c ->
    eval (if isTrue c then ifTrue else ifFalse) env
ifFn args _ = throwE $ wrongArgNum args

cond :: [Sexpr] -> Env -> Result
cond ((List (condition : body)) : xs) env =
  eval condition env >>= go
  where
    go s | isTrue s =
      case body of
        [] -> return s
        body -> (evalEachAnd $ return . last) body env
    go s = cond xs env
cond (sexpr : _) _ = throwE $ wrongArg sexpr
cond [] _ = return Nil

andFn :: [Sexpr] -> Sexpr
andFn [x] = x
andFn (x : xs) | isTrue x = andFn xs
andFn (_ : _) = Bool False
andFn [] = Bool True

orFn :: [Sexpr] -> Sexpr
orFn (x : _) | isTrue x = x
orFn (_ : xs) = orFn xs
orFn [] = Bool False

isBool :: Sexpr -> Either Error Sexpr
isBool (Bool _) = return $ Bool True
isBool _ = return $ Bool False

isNumber :: Sexpr -> Either Error Sexpr
isNumber (Int _) = return $ Bool True
isNumber (Float _) = return $ Bool True
isNumber _ = return $ Bool False

isInt :: Sexpr -> Either Error Sexpr
isInt (Int _) = return $ Bool True
isInt _ = return $ Bool False

isFloat :: Sexpr -> Either Error Sexpr
isFloat (Float _) = return $ Bool True
isFloat _ = return $ Bool False

isString :: Sexpr -> Either Error Sexpr
isString (String _) = return $ Bool True
isString _ = return $ Bool False

isSymbol :: Sexpr -> Either Error Sexpr
isSymbol (Symbol _) = return $ Bool True
isSymbol _ = return $ Bool False

isNull :: Sexpr -> Either Error Sexpr
isNull (List []) = return $ Bool True
isNull _ = return $ Bool False

isPair :: Sexpr -> Either Error Sexpr
isPair (List []) = return $ Bool False
isPair (List _) = return $ Bool True
isPair _ = return $ Bool False

isProcedure :: Sexpr -> Either Error Sexpr
isProcedure (Func _) = return $ Bool True
isProcedure _ = return $ Bool False

toInt :: Sexpr -> Either Error Sexpr
toInt (Float num) = return $ Int $ round num
toInt (Int num) = return $ Int num
toInt (String str) =
  case (readMaybe str :: Maybe Int) of
    Just num -> return $ Int num
    Nothing -> Left $ printf "cannot parse: %s" str
toInt sexpr = Left $ wrongArg sexpr

toFloat :: Sexpr -> Either Error Sexpr
toFloat (Int num) = return $ Float $ fromIntegral num
toFloat (Float num) = return $ Float num
toFloat (String str) =
  case (readMaybe str :: Maybe Float) of
    Just num -> return $ Float num
    Nothing -> Left $ printf "cannot parse: %s" str
toFloat sexpr = Left $ wrongArg sexpr

allEqual :: [Sexpr] -> Bool
allEqual [] = True
allEqual xs = all (== head xs) (tail xs)

equal :: [Sexpr] -> Env -> Result
equal = evalEachAnd $ return . Bool . allEqual

numReduce :: (Sexpr -> Sexpr -> Sexpr) -> Sexpr -> [Sexpr] -> Env -> Result
numReduce _ init [] _ = return init
numReduce op init [x] env = numReduceImpl op [x] env init
numReduce op _ (x : xs) env =
  eval x env >>= numReduceImpl op xs env

numReduceImpl :: (Sexpr -> Sexpr -> Sexpr) -> [Sexpr] -> Env -> Sexpr -> Result
numReduceImpl op (x : xs) env acc =
  eval x env >>= \v -> do
    result <- liftIO (try (evaluate $ op acc v) :: IO (Either NaN Sexpr))
    case result of
      Left msg -> throwE $ show msg
      Right new -> numReduceImpl op xs env new
numReduceImpl _ [] _ acc = return acc

numCompare :: (Sexpr -> Sexpr -> Bool) -> [Sexpr] -> Env -> Result
numCompare _ [] _ = return $ Bool True
numCompare _ [x] env =
  eval x env >> return (Bool True)
numCompare op (x : xs) env =
  eval x env >>= go xs
  where
    go (x : xs) acc =
      eval x env >>= \v -> do
        result <- liftIO (try (evaluate $ op acc v) :: IO (Either NaN Bool))
        case result of
          Left msg -> throwE $ show msg
          Right True -> go xs v
          Right False -> return $ Bool False
    go [] acc = return $ Bool True

load :: [Sexpr] -> Env -> Result
load [arg] env =
  eval arg env >>= go
  where
    go (String name) = evalFile name env
    go sexpr = throwE $ wrongArg sexpr
load args _ = throwE $ wrongArgNum args

toString :: [Sexpr] -> String
toString = unwords . map show

lastOrNil :: [Sexpr] -> Sexpr
lastOrNil [] = Nil
lastOrNil list = last list

oneArg :: (Sexpr -> Either Error Sexpr) -> [Sexpr] -> Either Error Sexpr
oneArg f [sexpr] = f sexpr
oneArg _ args = Left $ wrongArgNum args

evalEachAnd :: ([Sexpr] -> Either Error Sexpr) -> [Sexpr] -> Env -> Result
evalEachAnd f args env =
  evalEach args env >>= liftE . f

wrongArgNum :: [Sexpr] -> String
wrongArgNum args = printf "wrong number of arguments: %d" $ length args

wrongArg :: Sexpr -> String
wrongArg = printf "invalid argument: %s"

notASymbol :: Sexpr -> String
notASymbol = printf "%s is not a symbol"

isTrue :: Sexpr -> Bool
isTrue (Bool False) = False
isTrue _ = True

liftE :: Monad m => Either e a -> ExceptT e m a
liftE (Right x) = return x
liftE (Left msg) = throwE msg
