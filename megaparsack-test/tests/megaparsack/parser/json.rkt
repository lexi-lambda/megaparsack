#lang racket/base

(require megaparsack
         megaparsack/parser/json
         racket/function
         rackunit
         rackunit/spec)

(describe "parse-json-string"
  (it "parses floating-point numbers"
    (check-= (parse-result! (parse-json-string "42")) 42 0.001)
    (check-= (parse-result! (parse-json-string ".3")) 0.3 0.001)
    (check-= (parse-result! (parse-json-string "12.75")) 12.75 0.001)
    (check-= (parse-result! (parse-json-string "1e+2")) 100 0.001)
    (check-= (parse-result! (parse-json-string "1.2e-3")) 0.0012 0.00001))

  (it "parses strings"
    (check-equal? (parse-result! (parse-json-string "\"hello, world\"")) "hello, world")
    (check-equal? (parse-result! (parse-json-string "\"this (\\\") is a double quote\""))
                  "this (\") is a double quote")
    (check-equal? (parse-result! (parse-json-string "\"escape \\u1a3f!\""))
                  "escape \u1a3f!"))

  (it "parses booleans"
    (check-true (parse-result! (parse-json-string "true")))
    (check-false (parse-result! (parse-json-string "false"))))

  (it "parses null as the symbol 'null"
    (check-equal? (parse-result! (parse-json-string "null")) 'null))

  (it "parses arrays as lists"
    (check-equal? (parse-result! (parse-json-string "[1, 2, 3]")) '(1 2 3))
    (check-equal? (parse-result! (parse-json-string "[true, null, 42]")) '(#t null 42)))

  (it "parses objects as hasheqs with symbol keys"
    (check-equal? (parse-result! (parse-json-string "{ \"a\": 1, \"b\": true }"))
                  #hasheq((a . 1) (b . #t))))

  (it "fails when it cannot parse the entire string"
    (check-exn #rx"unexpected: t\n  expected: end of input"
               (thunk (parse-result! (parse-json-string "{}true"))))))
