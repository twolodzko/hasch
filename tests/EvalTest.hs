module EvalTest where

import Envir (insert, new)
import Eval (eval)
import Parser (parse)
import StringReader (new)
import Test.HUnit (Test (TestCase), Testable (test), assertEqual)
import Types (Result (Err, Ok), Sexpr (..))

parseEval :: String -> IO (Result Sexpr)
parseEval str = do
  env <- Envir.new
  reader <- StringReader.new str
  result <- parse reader
  case result of
    Ok (Just sexpr) -> eval sexpr env
    Err msg -> return $ Err msg

assertParseEval :: String -> Result Sexpr -> String -> Test
assertParseEval desc expected str =
  TestCase
    ( do
        result <- parseEval str
        assertEqual desc expected result
    )

testSymbol :: Test
testSymbol =
  TestCase
    ( do
        env <- Envir.new
        Envir.insert "x" (Int 42) env
        result <- eval (Symbol "x") env
        assertEqual "evaluate saved symbol" (Ok $ Int 42) result
    )

testMissingSymbol :: Test
testMissingSymbol =
  TestCase
    ( do
        env <- Envir.new
        Envir.insert "foo" (Int 42) env
        result <- eval (Symbol "bar") env
        assertEqual "evaluate symbol that doesn't exist" (Err "bar was not found") result
    )

tests :: Test
tests =
  test
    [ assertParseEval "evaluate integer" (Ok $ Int 42) "42",
      assertParseEval "evaluate float" (Ok $ Float 3.14) "3.14",
      assertParseEval "evaluate #t" (Ok $ Bool True) "#t",
      assertParseEval "evaluate #f" (Ok $ Bool False) "#f",
      assertParseEval "evaluate string" (Ok $ String "hello world") "\"hello world\"",
      assertParseEval "evaluate null list" (Ok $ List []) "()",
      assertParseEval "evaluate quoted symbol" (Ok $ Symbol "foo") "'foo",
      testSymbol,
      testMissingSymbol
    ]
