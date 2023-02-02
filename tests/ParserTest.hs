module ParserTest where

import Parser (parse)
import StringReader (new)
import Test.HUnit (Test (TestCase), Testable (test), assertEqual)
import Types (Result (Err, Ok), Sexpr (..))

assertParse :: String -> Result (Maybe Sexpr) -> String -> Test
assertParse desc expected str =
  TestCase
    ( do
        result <- parse =<< StringReader.new str
        assertEqual desc expected result
    )

tests :: Test
tests =
  test
    [ assertParse "parse empty input" (Ok Nothing) "",
      assertParse "parse empty newline" (Ok Nothing) "\n",
      assertParse "parse symbol delimited by space" (Ok $ Just $ Symbol "hello") "hello world",
      assertParse "parse symbol delimited by (" (Ok $ Just $ Symbol "hello") "hello(world",
      assertParse "parse symbol delimited by )" (Ok $ Just $ Symbol "hello") "hello)world",
      assertParse "parse symbol delimited by \"" (Ok $ Just $ Symbol "hello") "hello\"world",
      assertParse "parse symbol with spaces before" (Ok $ Just $ Symbol "ok") " \t\n  ok",
      assertParse "parse #t" (Ok $ Just $ Bool True) "#t tail",
      assertParse "parse #f" (Ok $ Just $ Bool False) "#f tail",
      assertParse "parse float" (Ok $ Just $ Float 3.14) "3.14 tail",
      assertParse "parse integer" (Ok $ Just $ Int (-42)) "-42 tail",
      assertParse "parse integer delimited by )" (Ok $ Just $ Int 123) "123)",
      assertParse "parse symbol" (Ok $ Just $ Symbol "foo") "foo tail",
      assertParse "parse simple string" (Ok $ Just $ String "hello world!") "\"hello world!\" tail",
      assertParse "parse quoted symbol" (Ok $ Just $ Quote $ Symbol "foo") "'foo 'bar 'baz",
      assertParse "parse empty list 1" (Ok $ Just $ List []) "() tail",
      assertParse "parse empty list 2" (Ok $ Just $ List []) "()",
      assertParse "parse list" (Ok $ Just $ List [Int 1, Int 2, Int 3]) "(1 2 3)",
      assertParse "parse list with spaces" (Ok $ Just $ List [Int 1, Int 2, Int 3]) "( 1 \n 2 \n  3 \n )",
      assertParse "parse nested list" (Ok $ Just $ List [List []]) "(())",
      assertParse
        "parse list of lists"
        (Ok $ Just $ List [Int 1, List [Int 2, List [Int 3, Int 4], Int 5], List [Int 6]])
        "(1 (2 (3 4) 5) (6))",
      assertParse "parse empty quoted list" (Ok $ Just $ Quote $ List []) "'()",
      assertParse "parse comment and symbol" (Ok $ Just $ Quote $ Symbol "ok") ";; this is a comment\n\n'ok",
      assertParse
        "parse comment in list"
        (Ok $ Just $ List [Symbol "+", Int 1, Int 2, Int 3])
        "(+ 1 ;; comment A\n 2\n 3 ; comment B  \n\n)",
      -- exceptions
      assertParse "missing ) error" (Err "missing )") "(+ 2 2",
      assertParse "missing \" error" (Err "missing \"") "\"hello world!",
      assertParse "unexpected )" (Err "unexpected )") ") (+ 2 2)",
      assertParse "missing quoted value" (Err "missing quoted value") "'   "
    ]
