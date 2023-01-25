module ReaderTest where

import Data.Maybe (isNothing)
import GHC.IO.Exception (assertError)
import Parser (Reader (peek, pop))
import StringReader (new)
import Test.HUnit
  ( Assertable (assert),
    Test (TestCase),
    Testable (test),
  )

testStringReaderEmpty :: Test
testStringReaderEmpty =
  TestCase
    ( do
        reader <- StringReader.new ""

        result <- peek reader
        assert $ isNothing result

        result <- pop reader
        assert $ isNothing result

        result <- pop reader
        assert $ isNothing result

        result <- peek reader
        assert $ isNothing result
    )

testStringReader :: Test
testStringReader =
  TestCase
    ( do
        reader <- StringReader.new "abc"

        result <- peek reader
        assert $ result == Just 'a'

        result <- pop reader
        assert $ result == Just 'a'

        result <- pop reader
        assert $ result == Just 'b'

        result <- pop reader
        assert $ result == Just 'c'

        result <- pop reader
        assert $ isNothing result
    )

tests :: Test
tests =
  test
    [testStringReaderEmpty, testStringReader]
