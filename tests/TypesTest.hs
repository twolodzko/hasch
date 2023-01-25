module TypesTest where

import Test.HUnit (Test, Testable (test), (~:), (~=?))
import Text.Printf (printf)
import Types (Sexpr (..))

tests :: Test
tests =
  test
    [ "printf #t" ~: "#t" ~=? printf "%s" (Bool True),
      "printf #f" ~: "#f" ~=? printf "%s" (Bool False),
      "printf symbol" ~: "foo" ~=? printf "%s" (Symbol "foo"),
      "printf string" ~: "\"foo\"" ~=? printf "%s" (String "foo"),
      "printf quoted symbol" ~: "'foo" ~=? printf "%s" (Quote $ Symbol "foo"),
      "printf null list" ~: "()" ~=? printf "%s" (List []),
      "printf null list inside list" ~: "(())" ~=? printf "%s" (List [List []]),
      "printf list" ~: "(1 #t 'foo)" ~=? printf "%s" (List [Int 1, Bool True, Quote $ Symbol "foo"])
    ]
