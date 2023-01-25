module EnvirTest where

import Data.Map.Strict (member)
import Envir (branch, findEnv, insert, lookup, new)
import Test.HUnit (Test (TestCase), Testable (test), assertEqual)

empty :: Test
empty =
  TestCase
    ( do
        env <- Envir.new
        result <- Envir.lookup "foo" env
        assertEqual "nothing to read from empty Env" Nothing (result :: Maybe Bool)
    )

nonEmpty :: Test
nonEmpty =
  TestCase
    ( do
        env <- Envir.new
        Envir.insert "foo" "ok" env
        result <- Envir.lookup "foo" env
        assertEqual "can read saved value from Env" (Just "ok") result
    )

replaceInserted :: Test
replaceInserted =
  TestCase
    ( do
        env <- Envir.new
        Envir.insert "foo" "wrong" env
        Envir.insert "foo" "ok" env
        result <- Envir.lookup "foo" env
        assertEqual "can read overwritten value from Env" (Just "ok") result
    )

branchingReadNothing :: Test
branchingReadNothing =
  TestCase
    ( do
        parent <- Envir.new
        child <- Envir.branch parent
        result <- Envir.lookup "y" child
        assertEqual "read nothing from branched Env" Nothing (result :: Maybe Bool)
    )

branchingRead :: Test
branchingRead =
  TestCase
    ( do
        parent <- Envir.new
        Envir.insert "x" "ok" parent
        child <- Envir.branch parent
        result <- Envir.lookup "x" child
        assertEqual "read recursively from the parent Env" (Just "ok") result
    )

repeatedNames :: Test
repeatedNames =
  TestCase
    ( do
        parent <- Envir.new
        Envir.insert "x" "wrong" parent
        child <- Envir.branch parent
        Envir.insert "x" "correct" child
        result <- Envir.lookup "x" child
        assertEqual
          "read recursively from the parent Env when names are repeated"
          (Just "correct")
          result
    )

doesntMutateParent :: Test
doesntMutateParent =
  TestCase
    ( do
        parent <- Envir.new
        Envir.insert "x" "correct" parent
        child <- Envir.branch parent
        Envir.insert "x" "wrong" child
        result <- Envir.lookup "x" parent
        assertEqual
          "writing to child does not mutate parent"
          (Just "correct")
          result
    )

tests :: Test
tests = test [empty, nonEmpty, replaceInserted, branchingReadNothing, branchingRead, repeatedNames, doesntMutateParent]
