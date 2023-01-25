module SchemeTest where

import Data.Maybe (isNothing)
import Envir (EnvRef, branch, insert, lookup)
import Eval (eval, evalFile)
import Parser (parse)
import Scheme (root)
import StringReader (new)
import Test.HUnit
  ( Assertable (assert),
    Test (TestCase),
    Testable (test),
    assertEqual,
  )
import Types (Result (Err, Ok), Sexpr (..))

parseEval :: String -> Envir.EnvRef Sexpr -> IO (Result Sexpr)
parseEval str env = do
  reader <- StringReader.new str
  result <- parse reader
  case result of
    Ok (Just sexpr) -> eval sexpr env
    Err msg -> return $ Err msg

assertParseEval :: Result Sexpr -> String -> Test
assertParseEval expected str =
  TestCase
    ( do
        env <- root
        result <- parseEval str env
        assertEqual str expected result
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

tests :: Test
tests =
  test
    [ -- begin
      assertParseEval (Ok $ Int 3) "(begin (car '(1 2 3)) (car '(3 4 5)))",
      assertParseEval (Err "expected") "(begin (+ 2 2) (error 'expected) (+ 1 2 3))",
      -- car
      assertParseEval (Ok $ Int 1) "(car '(1 2 3))",
      assertParseEval (Ok $ Int 1) "(car (list 1 2 3))",
      assertParseEval (Err "wrong number of arguments: 0") "(car)",
      assertParseEval (Err "wrong number of arguments: 2") "(car '(1 2 3) '(4 5 6))",
      -- cdr
      assertParseEval (Ok $ List [Int 2, Int 3]) "(cdr '(1 2 3))",
      assertParseEval (Ok $ List [Int 2, Int 3]) "(cdr (list 1 2 3))",
      assertParseEval (Err "wrong number of arguments: 0") "(cdr)",
      assertParseEval (Err "wrong number of arguments: 2") "(cdr '(1 2 3) '(4 5 6))",
      -- cons
      assertParseEval (Err "wrong number of arguments: 0") "(cons)",
      assertParseEval (Err "wrong number of arguments: 1") "(cons 1)",
      assertParseEval (Err "wrong number of arguments: 3") "(cons 1 '() '())",
      assertParseEval (Ok $ List [Int 1]) "(cons 1 '())",
      assertParseEval (Ok $ List [Int 1, Int 2]) "(cons 1 '(2))",
      assertParseEval (Ok $ List [Int 1, Int 2]) "(cons 1 (list 2))",
      assertParseEval (Ok $ List [Int 1, Int 2, Int 3]) "(cons 1 '(2 3))",
      assertParseEval (Ok $ List [Int 3, Int 7]) "(cons (+ 1 2) (list (+ 3 4)))",
      -- lambda
      assertParseEval (Ok Nil) "((lambda ()))",
      assertParseEval (Ok $ Symbol "ok") "((lambda (x) x) 'ok)",
      assertParseEval (Ok $ Int 6) "((lambda (x y z) (+ x y z)) 1 2 3)",
      assertParseEval (Err "wrong number of arguments: 0") "(lambda)",
      assertParseEval (Err "invalid argument: x") "(lambda x (+ x 1))",
      assertParseEval (Err "#t is not a symbol") "(lambda (x #t z) (+ x z))",
      -- let, let*
      assertParseEval (Ok $ Symbol "ok") "(let () 'ok)",
      assertParseEval (Ok $ Int 3) "(let ((x 1) (y 2)) (+ x y))",
      assertParseEval (Ok $ Int 4) "(let ((x 1) (y 2)) (+ x y) (* y y))",
      assertParseEval (Ok $ Int 2) "(let ((x 1)) (let ((y x)) (+ x y)))",
      assertParseEval (Err "invalid argument: 'wat") "(let 'wat '())",
      assertParseEval (Err "y is not a symbol") "(let ((x 1) y 2) (+ x y))",
      assertParseEval (Err "x was not found") "(let ((x 1) (y (+ x 1))) (+ x y))",
      assertParseEval (Ok $ Symbol "ok") "(let* () 'ok)",
      assertParseEval (Ok $ Int 3) "(let* ((x 1) (y 2)) (+ x y))",
      assertParseEval (Ok $ Int 4) "(let* ((x 1) (y 2)) (+ x y) (* y y))",
      assertParseEval (Ok $ Int 2) "(let* ((x 1)) (let* ((y x)) (+ x y)))",
      assertParseEval (Ok $ Int 3) "(let* ((x 1) (y (- 3 x))) (car '(1 2 3)) (+ x y))",
      assertParseEval (Ok $ Int 3) "(let* ((x 1) (y (+ x 1))) (+ x y))",
      -- closures
      assertParseEval (Ok $ Float 0.5) "(((lambda (x) (lambda (y) (/ x y))) 1) 2)",
      assertParseEval (Ok $ Int 4) "((lambda (x) (let ((x (+ x 1))) (* x 2))) 1)",
      assertParseEval (Ok $ Int 5) "(let ((x 2)) ((lambda (y) (+ y (+ x 1))) x))",
      assertParseEval (Ok $ Int 5) "((let ((x 2)) (lambda (y) (+ x y))) 3)",
      assertParseEval (Ok $ Int 6) "((let* ((x 1) (z (+ x 1))) (lambda (y) (+ x y z))) 3)",
      -- ->integer, ->float
      assertParseEval (Ok $ Int 42) "(->integer 42)",
      assertParseEval (Ok $ Int 3) "(->integer 3.14)",
      assertParseEval (Ok $ Int 100) "(->integer \"100\")",
      assertParseEval (Ok $ Float 42) "(->float 42)",
      assertParseEval (Ok $ Float 3.14) "(->float 3.14)",
      assertParseEval (Ok $ Float 100) "(->float \"100\")",
      -- eval
      assertParseEval (Ok $ Symbol "foo") "(eval ''foo)",
      assertParseEval (Ok $ Quote $ Symbol "foo") "(eval '''foo)",
      -- list
      assertParseEval (Ok $ List []) "(list)",
      assertParseEval (Ok $ List [Symbol "foo", Int 42, Bool True]) "(list 'foo 42 #t)",
      -- not
      assertParseEval (Ok $ Bool False) "(not #t)",
      assertParseEval (Ok $ Bool True) "(not #f)",
      assertParseEval (Ok $ Bool False) "(not '())",
      assertParseEval (Ok $ Bool True) "(let ((x #f)) (not x))",
      -- and
      assertParseEval (Ok $ Bool True) "(and)",
      assertParseEval (Ok $ Bool True) "(and #t)",
      assertParseEval (Ok $ Bool False) "(and #f)",
      assertParseEval (Ok $ Int 1) "(and #t '() 1)",
      assertParseEval (Ok $ Bool False) "(and #t '() #f 1)",
      assertParseEval (Ok $ Int 3) "(and 1 2 3)",
      -- or
      assertParseEval (Ok $ Bool False) "(or)",
      assertParseEval (Ok $ Bool True) "(or #t)",
      assertParseEval (Ok $ Bool False) "(or #f)",
      assertParseEval (Ok $ Bool True) "(or #f #f #t #f)",
      assertParseEval (Ok $ Int 100) "(or #f 100 200)",
      assertParseEval (Ok $ Int 1) "(let ((x #f) (y 1) (z 2)) (or x y z)))",
      -- if
      assertParseEval (Ok $ Int 1) "(if #t 1 2)",
      assertParseEval (Ok $ Int 2) "(if #f 1 2)",
      assertParseEval (Ok $ Int 1) "(if #t 1 (error 'wrong))",
      assertParseEval (Ok $ Int 2) "(if #f (error 'wrong) 2)",
      assertParseEval (Ok $ Int 4) "(let ((x 1) (y 2) (z 3)) (if (= x 1) (+ y y) (+ z z)))",
      assertParseEval (Ok $ Int 6) "(let ((x 1) (y 2) (z 3)) (if (not (= x 1)) (+ y y) (+ z z)))",
      -- cond
      assertParseEval (Ok Nil) "(cond)",
      assertParseEval (Ok $ Int 1) "(cond (#t 1) (#f 2) (#t 3))",
      assertParseEval (Ok $ Int 2) "(cond (#f 1) (#t 2) (#t 3))",
      assertParseEval (Ok Nil) "(cond (#f 1) (#f 2) (#f 3))",
      assertParseEval (Ok $ Int 2) "(cond (#f) (2) (#t (error 'wrong)))",
      assertParseEval (Ok $ Int 4) "(cond (#f (error 'wrong)) (#t (+ 2 2)))",
      assertParseEval (Err "invalid argument: #t") "(cond #t)",
      -- equal? eq?
      assertParseEval (Ok $ Bool True) "(equal?)",
      assertParseEval (Ok $ Bool True) "(equal? 1)",
      assertParseEval (Ok $ Bool True) "(equal? 'x 'x 'x)",
      assertParseEval (Ok $ Bool False) "(equal? 1 1.0 1)",
      assertParseEval (Ok $ Bool False) "(equal? 1 #t #f #t)",
      assertParseEval (Ok $ Bool True) "(let ((x 'foo)) (equal? x x))",
      -- string
      assertParseEval (Ok $ String "") "(string)",
      assertParseEval (Ok $ String "1 #t foo") "(string 1 #t 'foo)",
      -- error
      assertParseEval (Err "") "(error)",
      assertParseEval (Err "1 #t foo") "(error 1 #t 'foo)",
      -- quote
      assertParseEval (Ok $ Symbol "foo") "(quote foo)",
      assertParseEval (Err "wrong number of arguments: 0") "(quote)",
      assertParseEval (Err "wrong number of arguments: 3") "(quote foo bar baz)",
      -- string?
      assertParseEval (Ok $ Bool True) "(string? \"\")",
      assertParseEval (Ok $ Bool True) "(string? \"hello\")",
      assertParseEval (Ok $ Bool False) "(string? 'hello)",
      assertParseEval (Ok $ Bool False) "(string? #t)",
      -- bool?
      assertParseEval (Ok $ Bool True) "(bool? #t)",
      assertParseEval (Ok $ Bool True) "(bool? #f)",
      assertParseEval (Ok $ Bool False) "(bool? 1)",
      assertParseEval (Ok $ Bool False) "(bool? '())",
      -- symbol?
      assertParseEval (Ok $ Bool True) "(symbol? 'foo)",
      assertParseEval (Ok $ Bool False) "(symbol? \"hello\")",
      assertParseEval (Ok $ Bool False) "(symbol? #t)",
      -- number?
      assertParseEval (Ok $ Bool True) "(number? 42)",
      assertParseEval (Ok $ Bool True) "(number? 3.14)",
      assertParseEval (Ok $ Bool False) "(number? \"42\")",
      assertParseEval (Ok $ Bool False) "(number? #t)",
      -- integer?
      assertParseEval (Ok $ Bool True) "(integer? 42)",
      assertParseEval (Ok $ Bool False) "(integer? 42.0)",
      assertParseEval (Ok $ Bool False) "(integer? \"42\")",
      assertParseEval (Ok $ Bool False) "(integer? #t)",
      -- float?
      assertParseEval (Ok $ Bool True) "(float? 3.14)",
      assertParseEval (Ok $ Bool False) "(float? 3)",
      assertParseEval (Ok $ Bool False) "(float? #t)",
      -- null?
      assertParseEval (Ok $ Bool True) "(null? '())",
      assertParseEval (Ok $ Bool True) "(null? (list))",
      assertParseEval (Ok $ Bool False) "(null? '(1 2 3))",
      assertParseEval (Ok $ Bool False) "(null? '(()))",
      assertParseEval (Ok $ Bool False) "(null? #t)",
      -- pair?
      assertParseEval (Ok $ Bool True) "(pair? '(1 2 3))",
      assertParseEval (Ok $ Bool True) "(pair? (list 1 2 3))",
      assertParseEval (Ok $ Bool True) "(pair? '(()))",
      assertParseEval (Ok $ Bool False) "(pair? '())",
      assertParseEval (Ok $ Bool False) "(pair? #t)",
      -- procedure?
      assertParseEval (Ok $ Bool True) "(procedure? car)",
      assertParseEval (Ok $ Bool True) "(procedure? procedure?)",
      assertParseEval (Ok $ Bool False) "(procedure? #t)",
      -- math
      assertParseEval (Ok $ Int 0) "(+)",
      assertParseEval (Ok $ Int 2) "(+ 2)",
      assertParseEval (Ok $ Int 6) "(+ 1 2 3)",
      assertParseEval (Ok $ Float 6.5) "(+ 1 2 3.5)",
      assertParseEval (Ok $ Int 0) "(-)",
      assertParseEval (Ok $ Int (-2)) "(- 2)",
      assertParseEval (Ok $ Int 0) "(- 3 2 1)",
      assertParseEval (Ok $ Int 1) "(*)",
      assertParseEval (Ok $ Int 2) "(* 2)",
      assertParseEval (Ok $ Int 24) "(* 1 2 3 4)",
      assertParseEval (Ok $ Float 1) "(/)",
      assertParseEval (Ok $ Float 0.5) "(/ 2)",
      assertParseEval (Ok $ Float 2) "(/ 100 10 5)",
      assertParseEval (Ok $ Float 6) "(+ (- 2 1) (/ 4 2) (* (+ 2 3 1) 0.5))",
      assertParseEval (Ok $ Int 4) "(let ((x 2)) (+ x x))",
      -- numerical comparisons
      assertParseEval (Ok $ Bool True) "(<)",
      assertParseEval (Ok $ Bool True) "(< 1)",
      assertParseEval (Ok $ Bool True) "(< 1 2 3)",
      assertParseEval (Ok $ Bool True) "(< 1 (+ 1 2) (+ 1 2 3))",
      assertParseEval (Ok $ Bool True) "(=)",
      assertParseEval (Ok $ Bool True) "(= 1)",
      assertParseEval (Ok $ Bool True) "(= 1 1 1)",
      assertParseEval (Ok $ Bool True) "(= 1 1 1.0)",
      assertParseEval (Ok $ Bool False) "(= 1 1 (+ 2 2))",
      assertParseEval (Err "operation cannot be applied to 1 and #t") "(= 1 #t)",
      assertParseEval (Ok $ Bool True) "(>)",
      assertParseEval (Ok $ Bool True) "(> 1)",
      assertParseEval (Ok $ Bool True) "(> 3 2 1)",
      assertParseEval (Ok $ Bool False) "(> 3 2 1 5)",
      assertParseEval (Ok $ Bool True) "(let ((x 1)) (= x x))",
      -- set!
      setBangTest,
      -- load
      assertParseEval (Ok $ Int 321) "(load \"examples/simple.scm\")"
    ]
