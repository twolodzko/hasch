module SchemeTest where

import Data.Maybe (isJust, isNothing)
import Envir (EnvRef, branch, insert, lookup)
import Eval (eval, evalFile)
import Parser (parse)
import Result (Result (..), isErr)
import Scheme (root)
import StringReader (new)
import Test.HUnit
  ( Assertable (assert),
    Test (TestCase),
    Testable (test),
    assertBool,
    assertEqual,
  )
import Text.Printf (printf)
import Text.Regex
import Types (Sexpr (..))

parseEval :: String -> Envir.EnvRef Sexpr -> IO (Result Sexpr)
parseEval str env = do
  result <- parse =<< StringReader.new str
  case result of
    Ok (Just sexpr) -> eval sexpr env
    Err msg -> return $ Err msg

assertEvalEqual :: Sexpr -> String -> Test
assertEvalEqual expected str =
  TestCase
    ( do
        result <- parseEval str =<< root
        assertEqual
          (printf "expecting %s for %s" expected str)
          (Ok expected)
          result
    )

assertEvalThrows :: String -> String -> Test
assertEvalThrows err str =
  TestCase
    ( do
        result <- parseEval str =<< root
        case result of
          Ok _ -> error "didn't raise error"
          Err msg ->
            assertBool
              (printf "expecting error '%s' for: %s" err str)
              $ isJust
              $ matchRegex (mkRegex $ err ++ "$") msg
    )

setBangTest :: Test
setBangTest =
  TestCase
    ( do
        env <- Scheme.root
        child <- Envir.branch env
        Envir.insert "x" (Symbol "wrong") child
        grandchild <- Envir.branch child
        parseEval "(set! x 'ok)" grandchild

        result <- Envir.lookup "x" env
        assert $ isNothing result

        result <- Envir.lookup "x" child
        assertEqual
          "symbol has changed after set!"
          (Just $ Symbol "ok")
          result
    )

fiboTailRecursive :: Test
fiboTailRecursive =
  TestCase
    ( do
        env <- Scheme.root
        let code =
              "(define impl (lambda (it second first) \
              \  (if (= it 0) first \
              \      (impl (- it 1) (+ first second) second))))"
        assert $ not . isErr <$> parseEval code env
        assert $ not . isErr <$> parseEval "(define fibo (lambda (n) (impl n 1 0)))" env

        result <- parseEval "(fibo 0)" env
        assert $ Ok (Int 0) == result

        result <- parseEval "(fibo 1)" env
        assert $ Ok (Int 1) == result

        result <- parseEval "(fibo 2)" env
        assert $ Ok (Int 1) == result

        result <- parseEval "(fibo 3)" env
        assert $ Ok (Int 2) == result

        result <- parseEval "(fibo 7)" env
        assert $ Ok (Int 13) == result

        result <- parseEval "(fibo 9)" env
        assert $ Ok (Int 34) == result

        result <- parseEval "(fibo 10)" env
        assert $ Ok (Int 55) == result

        -- just run and not throw stack overflow error
        assert $ not . isErr <$> parseEval "(fibo 10000)" env
    )

tests :: Test
tests =
  test
    [ -- begin
      assertEvalEqual (Int 3) "(begin (car '(1 2 3)) (car '(3 4 5)))",
      assertEvalThrows "expected" "(begin (+ 2 2) (error 'expected) (+ 1 2 3))",
      -- car
      assertEvalEqual (Int 1) "(car '(1 2 3))",
      assertEvalEqual (Int 1) "(car (list 1 2 3))",
      assertEvalThrows "wrong number of arguments: 0" "(car)",
      assertEvalThrows "wrong number of arguments: 2" "(car '(1 2 3) '(4 5 6))",
      -- cdr
      assertEvalEqual (List [Int 2, Int 3]) "(cdr '(1 2 3))",
      assertEvalEqual (List [Int 2, Int 3]) "(cdr (list 1 2 3))",
      assertEvalThrows "wrong number of arguments: 0" "(cdr)",
      assertEvalThrows "wrong number of arguments: 2" "(cdr '(1 2 3) '(4 5 6))",
      -- cons
      assertEvalThrows "wrong number of arguments: 0" "(cons)",
      assertEvalThrows "wrong number of arguments: 1" "(cons 1)",
      assertEvalThrows "wrong number of arguments: 3" "(cons 1 '() '())",
      assertEvalEqual (List [Int 1]) "(cons 1 '())",
      assertEvalEqual (List [Int 1, Int 2]) "(cons 1 '(2))",
      assertEvalEqual (List [Int 1, Int 2]) "(cons 1 (list 2))",
      assertEvalEqual (List [Int 1, Int 2, Int 3]) "(cons 1 '(2 3))",
      assertEvalEqual (List [Int 3, Int 7]) "(cons (+ 1 2) (list (+ 3 4)))",
      -- lambda
      assertEvalEqual Nil "((lambda ()))",
      assertEvalEqual (Symbol "ok") "((lambda (x) x) 'ok)",
      assertEvalEqual (Int 6) "((lambda (x y z) (+ x y z)) 1 2 3)",
      assertEvalThrows "wrong number of arguments: 0" "(lambda)",
      assertEvalThrows "invalid argument: x" "(lambda x (+ x 1))",
      assertEvalThrows "#t is not a symbol" "(lambda (x #t z) (+ x z))",
      -- let, let*
      assertEvalEqual (Symbol "ok") "(let () 'ok)",
      assertEvalEqual (Int 3) "(let ((x 1) (y 2)) (+ x y))",
      assertEvalEqual (Int 4) "(let ((x 1) (y 2)) (+ x y) (* y y))",
      assertEvalEqual (Int 2) "(let ((x 1)) (let ((y x)) (+ x y)))",
      assertEvalThrows "invalid argument: 'wat" "(let 'wat '())",
      assertEvalThrows "y is not a symbol" "(let ((x 1) y 2) (+ x y))",
      assertEvalThrows "x was not found" "(let ((x 1) (y (+ x 1))) (+ x y))",
      assertEvalEqual (Symbol "ok") "(let* () 'ok)",
      assertEvalEqual (Int 3) "(let* ((x 1) (y 2)) (+ x y))",
      assertEvalEqual (Int 4) "(let* ((x 1) (y 2)) (+ x y) (* y y))",
      assertEvalEqual (Int 2) "(let* ((x 1)) (let* ((y x)) (+ x y)))",
      assertEvalEqual (Int 3) "(let* ((x 1) (y (- 3 x))) (car '(1 2 3)) (+ x y))",
      assertEvalEqual (Int 3) "(let* ((x 1) (y (+ x 1))) (+ x y))",
      -- closures
      assertEvalEqual (Float 0.5) "(((lambda (x) (lambda (y) (/ x y))) 1) 2)",
      assertEvalEqual (Int 4) "((lambda (x) (let ((x (+ x 1))) (* x 2))) 1)",
      assertEvalEqual (Int 5) "(let ((x 2)) ((lambda (y) (+ y (+ x 1))) x))",
      assertEvalEqual (Int 5) "((let ((x 2)) (lambda (y) (+ x y))) 3)",
      assertEvalEqual (Int 6) "((let* ((x 1) (z (+ x 1))) (lambda (y) (+ x y z))) 3)",
      -- ->integer, ->float
      assertEvalEqual (Int 42) "(->integer 42)",
      assertEvalEqual (Int 3) "(->integer 3.14)",
      assertEvalEqual (Int 100) "(->integer \"100\")",
      assertEvalEqual (Float 42) "(->float 42)",
      assertEvalEqual (Float 3.14) "(->float 3.14)",
      assertEvalEqual (Float 100) "(->float \"100\")",
      -- eval
      assertEvalEqual (Symbol "foo") "(eval ''foo)",
      assertEvalEqual (Quote $ Symbol "foo") "(eval '''foo)",
      -- list
      assertEvalEqual (List []) "(list)",
      assertEvalEqual (List [Symbol "foo", Int 42, Bool True]) "(list 'foo 42 #t)",
      -- not
      assertEvalEqual (Bool False) "(not #t)",
      assertEvalEqual (Bool True) "(not #f)",
      assertEvalEqual (Bool False) "(not '())",
      assertEvalEqual (Bool True) "(let ((x #f)) (not x))",
      -- and
      assertEvalEqual (Bool True) "(and)",
      assertEvalEqual (Bool True) "(and #t)",
      assertEvalEqual (Bool False) "(and #f)",
      assertEvalEqual (Int 1) "(and #t '() 1)",
      assertEvalEqual (Bool False) "(and #t '() #f 1)",
      assertEvalEqual (Int 3) "(and 1 2 3)",
      -- or
      assertEvalEqual (Bool False) "(or)",
      assertEvalEqual (Bool True) "(or #t)",
      assertEvalEqual (Bool False) "(or #f)",
      assertEvalEqual (Bool True) "(or #f #f #t #f)",
      assertEvalEqual (Int 100) "(or #f 100 200)",
      assertEvalEqual (Int 1) "(let ((x #f) (y 1) (z 2)) (or x y z)))",
      -- if
      assertEvalEqual (Int 1) "(if #t 1 2)",
      assertEvalEqual (Int 2) "(if #f 1 2)",
      assertEvalEqual (Int 1) "(if #t 1 (error 'wrong))",
      assertEvalEqual (Int 2) "(if #f (error 'wrong) 2)",
      assertEvalEqual (Int 4) "(let ((x 1) (y 2) (z 3)) (if (= x 1) (+ y y) (+ z z)))",
      assertEvalEqual (Int 6) "(let ((x 1) (y 2) (z 3)) (if (not (= x 1)) (+ y y) (+ z z)))",
      -- cond
      assertEvalEqual Nil "(cond)",
      assertEvalEqual (Int 1) "(cond (#t 1) (#f 2) (#t 3))",
      assertEvalEqual (Int 2) "(cond (#f 1) (#t 2) (#t 3))",
      assertEvalEqual Nil "(cond (#f 1) (#f 2) (#f 3))",
      assertEvalEqual (Int 2) "(cond (#f) (2) (#t (error 'wrong)))",
      assertEvalEqual (Int 4) "(cond (#f (error 'wrong)) (#t (+ 2 2)))",
      assertEvalThrows "invalid argument: #t" "(cond #t)",
      -- equal? eq?
      assertEvalEqual (Bool True) "(equal?)",
      assertEvalEqual (Bool True) "(equal? 1)",
      assertEvalEqual (Bool True) "(equal? 'x 'x 'x)",
      assertEvalEqual (Bool False) "(equal? 1 1.0 1)",
      assertEvalEqual (Bool False) "(equal? 1 #t #f #t)",
      assertEvalEqual (Bool True) "(let ((x 'foo)) (equal? x x))",
      -- string
      assertEvalEqual (String "") "(string)",
      assertEvalEqual (String "1 #t foo") "(string 1 #t 'foo)",
      -- error
      assertEvalThrows "" "(error)",
      assertEvalThrows "1 #t foo" "(error 1 #t 'foo)",
      -- quote
      assertEvalEqual (Symbol "foo") "(quote foo)",
      assertEvalThrows "wrong number of arguments: 0" "(quote)",
      assertEvalThrows "wrong number of arguments: 3" "(quote foo bar baz)",
      -- string?
      assertEvalEqual (Bool True) "(string? \"\")",
      assertEvalEqual (Bool True) "(string? \"hello\")",
      assertEvalEqual (Bool False) "(string? 'hello)",
      assertEvalEqual (Bool False) "(string? #t)",
      -- bool?
      assertEvalEqual (Bool True) "(bool? #t)",
      assertEvalEqual (Bool True) "(bool? #f)",
      assertEvalEqual (Bool False) "(bool? 1)",
      assertEvalEqual (Bool False) "(bool? '())",
      -- symbol?
      assertEvalEqual (Bool True) "(symbol? 'foo)",
      assertEvalEqual (Bool False) "(symbol? \"hello\")",
      assertEvalEqual (Bool False) "(symbol? #t)",
      -- number?
      assertEvalEqual (Bool True) "(number? 42)",
      assertEvalEqual (Bool True) "(number? 3.14)",
      assertEvalEqual (Bool False) "(number? \"42\")",
      assertEvalEqual (Bool False) "(number? #t)",
      -- integer?
      assertEvalEqual (Bool True) "(integer? 42)",
      assertEvalEqual (Bool False) "(integer? 42.0)",
      assertEvalEqual (Bool False) "(integer? \"42\")",
      assertEvalEqual (Bool False) "(integer? #t)",
      -- float?
      assertEvalEqual (Bool True) "(float? 3.14)",
      assertEvalEqual (Bool False) "(float? 3)",
      assertEvalEqual (Bool False) "(float? #t)",
      -- null?
      assertEvalEqual (Bool True) "(null? '())",
      assertEvalEqual (Bool True) "(null? (list))",
      assertEvalEqual (Bool False) "(null? '(1 2 3))",
      assertEvalEqual (Bool False) "(null? '(()))",
      assertEvalEqual (Bool False) "(null? #t)",
      -- pair?
      assertEvalEqual (Bool True) "(pair? '(1 2 3))",
      assertEvalEqual (Bool True) "(pair? (list 1 2 3))",
      assertEvalEqual (Bool True) "(pair? '(()))",
      assertEvalEqual (Bool False) "(pair? '())",
      assertEvalEqual (Bool False) "(pair? #t)",
      -- procedure?
      assertEvalEqual (Bool True) "(procedure? car)",
      assertEvalEqual (Bool True) "(procedure? procedure?)",
      assertEvalEqual (Bool False) "(procedure? #t)",
      -- math
      assertEvalEqual (Int 0) "(+)",
      assertEvalEqual (Int 2) "(+ 2)",
      assertEvalEqual (Int 6) "(+ 1 2 3)",
      assertEvalEqual (Float 6.5) "(+ 1 2 3.5)",
      assertEvalEqual (Int 0) "(-)",
      assertEvalEqual (Int (-2)) "(- 2)",
      assertEvalEqual (Int 0) "(- 3 2 1)",
      assertEvalEqual (Int 1) "(*)",
      assertEvalEqual (Int 2) "(* 2)",
      assertEvalEqual (Int 24) "(* 1 2 3 4)",
      assertEvalEqual (Float 1) "(/)",
      assertEvalEqual (Float 0.5) "(/ 2)",
      assertEvalEqual (Float 2) "(/ 100 10 5)",
      assertEvalEqual (Float 6) "(+ (- 2 1) (/ 4 2) (* (+ 2 3 1) 0.5))",
      assertEvalEqual (Int 4) "(let ((x 2)) (+ x x))",
      -- numerical comparisons
      assertEvalEqual (Bool True) "(<)",
      assertEvalEqual (Bool True) "(< 1)",
      assertEvalEqual (Bool True) "(< 1 2 3)",
      assertEvalEqual (Bool True) "(< 1 (+ 1 2) (+ 1 2 3))",
      assertEvalEqual (Bool True) "(=)",
      assertEvalEqual (Bool True) "(= 1)",
      assertEvalEqual (Bool True) "(= 1 1 1)",
      assertEvalEqual (Bool True) "(= 1 1 1.0)",
      assertEvalEqual (Bool False) "(= 1 1 (+ 2 2))",
      assertEvalThrows "operation cannot be applied to 1 and #t" "(= 1 #t)",
      assertEvalEqual (Bool True) "(>)",
      assertEvalEqual (Bool True) "(> 1)",
      assertEvalEqual (Bool True) "(> 3 2 1)",
      assertEvalEqual (Bool False) "(> 3 2 1 5)",
      assertEvalEqual (Bool True) "(let ((x 1)) (= x x))",
      -- set!
      setBangTest,
      -- load
      assertEvalEqual (Int 321) "(load \"examples/simple.scm\")",
      -- test tail recursion
      fiboTailRecursive
    ]
