module ParserTest where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Parser (parse)
import StringReader (new)
import Test.HUnit (Test (TestCase), Testable (test), assertEqual)
import Types (Error, Sexpr (..))

assertParse :: String -> Either Error (Maybe Sexpr) -> String -> Test
assertParse desc expected str =
  TestCase
    ( do
        reader <- StringReader.new str
        result <- liftIO $ runExceptT $ parse reader
        assertEqual desc expected result
    )

tests :: Test
tests =
  test
    [ assertParse "parse empty input" (Right Nothing) "",
      assertParse "parse empty newline" (Right Nothing) "\n",
      assertParse "parse symbol delimited by space" (Right $ Just $ Symbol "hello") "hello world",
      assertParse "parse symbol delimited by (" (Right $ Just $ Symbol "hello") "hello(world",
      assertParse "parse symbol delimited by )" (Right $ Just $ Symbol "hello") "hello)world",
      assertParse "parse symbol delimited by \"" (Right $ Just $ Symbol "hello") "hello\"world",
      assertParse "parse symbol with spaces before" (Right $ Just $ Symbol "ok") " \t\n  ok",
      assertParse "parse #t" (Right $ Just $ Bool True) "#t tail",
      assertParse "parse #f" (Right $ Just $ Bool False) "#f tail",
      assertParse "parse float" (Right $ Just $ Float 3.14) "3.14 tail",
      assertParse "parse integer" (Right $ Just $ Int (-42)) "-42 tail",
      assertParse "parse integer delimited by )" (Right $ Just $ Int 123) "123)",
      assertParse "parse symbol" (Right $ Just $ Symbol "foo") "foo tail",
      assertParse "parse simple string" (Right $ Just $ String "hello world!") "\"hello world!\" tail",
      assertParse "parse quoted symbol" (Right $ Just $ Quote $ Symbol "foo") "'foo 'bar 'baz",
      assertParse "parse empty list 1" (Right $ Just $ List []) "() tail",
      assertParse "parse empty list 2" (Right $ Just $ List []) "()",
      assertParse "parse list" (Right $ Just $ List [Int 1, Int 2, Int 3]) "(1 2 3)",
      assertParse "parse list with spaces" (Right $ Just $ List [Int 1, Int 2, Int 3]) "( 1 \n 2 \n  3 \n )",
      assertParse "parse nested list" (Right $ Just $ List [List []]) "(())",
      assertParse
        "parse list of lists"
        (Right $ Just $ List [Int 1, List [Int 2, List [Int 3, Int 4], Int 5], List [Int 6]])
        "(1 (2 (3 4) 5) (6))",
      assertParse "parse empty quoted list" (Right $ Just $ Quote $ List []) "'()",
      assertParse "parse comment and symbol" (Right $ Just $ Quote $ Symbol "ok") ";; this is a comment\n\n'ok",
      assertParse
        "parse comment in list"
        (Right $ Just $ List [Symbol "+", Int 1, Int 2, Int 3])
        "(+ 1 ;; comment A\n 2\n 3 ; comment B  \n\n)",
      -- exceptions
      assertParse "missing ) error" (Left "missing )") "(+ 2 2",
      assertParse "missing \" error" (Left "missing \"") "\"hello world!",
      assertParse "unexpected )" (Left "unexpected )") ") (+ 2 2)",
      assertParse "missing quoted value" (Left "missing quoted value") "'   "
    ]
