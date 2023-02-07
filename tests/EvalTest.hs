module EvalTest where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Envir (insert, new)
import Eval (eval)
import Parser (parse)
import StringReader (new)
import Test.HUnit (Test (TestCase), Testable (test), assertEqual)
import Types (Error (..), Result, Sexpr (..))

parseEval :: String -> Result
parseEval str = do
  env <- liftIO Envir.new
  reader <- liftIO $ StringReader.new str
  result <- liftIO $ runExceptT $ parse reader
  case result of
    Right (Just sexpr) -> eval sexpr env
    Left msg -> throwE msg

assertParseEval :: String -> Either Error Sexpr -> String -> Test
assertParseEval desc expected str =
  TestCase
    ( do
        result <- liftIO $ runExceptT $ parseEval str
        assertEqual desc expected result
    )

testSymbol :: Test
testSymbol =
  TestCase
    ( do
        env <- liftIO Envir.new
        liftIO (Envir.insert "x" (Int 42) env)
        result <- liftIO $ runExceptT $ eval (Symbol "x") env
        assertEqual "evaluate saved symbol" (Right $ Int 42) result
    )

testMissingSymbol :: Test
testMissingSymbol =
  TestCase
    ( do
        env <- liftIO Envir.new
        liftIO (Envir.insert "foo" (Int 42) env)
        result <- liftIO $ runExceptT $ eval (Symbol "bar") env
        assertEqual "evaluate symbol that doesn't exist" (Left $ Undefined "bar") result
    )

tests :: Test
tests =
  test
    [ assertParseEval "evaluate integer" (Right $ Int 42) "42",
      assertParseEval "evaluate float" (Right $ Float 3.14) "3.14",
      assertParseEval "evaluate #t" (Right $ Bool True) "#t",
      assertParseEval "evaluate #f" (Right $ Bool False) "#f",
      assertParseEval "evaluate string" (Right $ String "hello world") "\"hello world\"",
      assertParseEval "evaluate null list" (Right $ List []) "()",
      assertParseEval "evaluate quoted symbol" (Right $ Symbol "foo") "'foo",
      testSymbol,
      testMissingSymbol
    ]
