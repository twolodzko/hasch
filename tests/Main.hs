import EnvirTest (tests)
import EvalTest (tests)
import ParserTest (tests)
import ReaderTest (tests)
import SchemeTest (tests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestList),
    runTestTT,
  )
import TypesTest (tests)

main :: IO ()
main = do
  result <- runTestTT $ TestList [ParserTest.tests, EnvirTest.tests, EvalTest.tests, SchemeTest.tests, TypesTest.tests, ReaderTest.tests]
  if (errors result + failures result) > 0
    then exitFailure
    else exitSuccess
