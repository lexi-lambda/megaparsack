#lang curly-fn racket/base

(require data/applicative
         data/either
         data/monad
         megaparsack
         megaparsack/text
         rackunit
         rackunit/spec)

(describe "or/p"
  (context "when no parsers consume input"
    (it "succeeds with the first successful parse"
      (check-equal? (parse-string (or/p (do eof/p (pure 'eof)) letter/p) "")
                    (success 'eof))))

  (it "fails when a parser consumes input and fails"
    (check-equal? (parse-string (or/p (do letter/p letter/p)
                                      (pure 'eof))
                                "a")
                  (failure (message (srcloc 'string 1 0 1 1) "end of input" '("letter")))))

  (it "preserves errors from other branches on success"
    (check-equal? (parse-string (do (or/p digit/p (pure #\0)) letter/p) "*")
                  (failure (message (srcloc 'string 1 0 1 1) #\* '("number" "letter"))))
    (check-equal? (parse-string (do (many/p (char/p #\r)) eof/p) "ra")
                  (failure (message (srcloc 'string 1 1 2 1) #\a '("'r'" "end of input"))))))

(describe "fail/p"
  (it "always fails"
    (let* ([loc (srcloc #f #f #f #f #f)]
           [msg (message loc
                         "something"
                         (list "something else"))]
           [p (fail/p msg)])
      (check-equal? (parse-string p "")
                    (failure msg)))))

(describe "noncommittal/p"
  (it "allows backtracking as if input was not consumed upon success"
    (check-equal? (parse-string (many/p letter/p #:sep (noncommittal/p space/p))
                                "a b c d .")
                  (success '(#\a #\b #\c #\d))))

  (it "still consumes input on failure"
    (check-equal? (parse-string (or/p (noncommittal/p (string/p "ab"))
                                      (char/p #\a))
                                "a")
                  (failure (message (srcloc 'string 1 0 1 1) "end of input" '("'b'"))))))

(describe "lookahead/p"
  (it "succeeds without consuming input"
    (check-equal? (parse-string (do (lookahead/p (char/p #\a))
                                    (char/p #\a)
                                    eof/p)
                                "a")
                  (success (void))))

  (it "still consumes input on failure"
    (check-equal? (parse-string (do (lookahead/p (string/p "ab"))
                                    (char/p #\a))
                                "a")
                  (failure (message (srcloc 'string 1 0 1 1) "end of input" '("'b'"))))))

(describe "one-of/p"
  (it "succeeds if any of the provided elements are equal"
    (check-equal? (parse-string (one-of/p '(#\a #\b)) "a")
                  (success #\a))
    (check-equal? (parse-string (one-of/p '(#\a #\b)) "b")
                  (success #\b)))

  (it "fails if none of the provided elements are equal"
    (check-equal? (parse-string (one-of/p '(#\a #\b)) "c")
                  (failure (message (srcloc 'string 1 0 1 1) #\c '("a" "b"))))))

(describe "guard/p"
  (let ([byte/p (label/p "byte" (try/p (guard/p integer/p #{% . < . 256})))])
    (it "succeeds when the guard predicate is non-#f"
      (check-equal? (parse-string byte/p "128")
                    (success 128)))
    (it "fails when the guard predicate is #f"
      (check-equal? (parse-string byte/p "1024")
                    (failure (message (srcloc 'string 1 0 1 4) 1024
                                      '("byte")))))))

(describe "list/p"
  (context "when not given a separator"
    (define letter-digit-letter/p (list/p letter/p digit/p letter/p))
    (it "succeeds when given components in sequence"
      (check-equal? (parse-string letter-digit-letter/p "a1b")
                    (success (list #\a #\1 #\b))))
    (it "fails when given too few components"
      (check-equal? (parse-string letter-digit-letter/p "a1")
                    (failure (message (srcloc 'string 1 1 2 1)
                                      "end of input"
                                      '("letter"))))))
  (context "when given a separator"
    (define dotted-letter-digit-letter/p
      (list/p letter/p digit/p letter/p #:sep (char/p #\.)))
    (it "succeeds when given separated components"
      (check-equal? (parse-string dotted-letter-digit-letter/p "a.1.b")
                    (success (list #\a #\1 #\b))))
    (it "fails when given unseparated components"
      (check-equal? (parse-string dotted-letter-digit-letter/p "a1b")
                    (failure (message (srcloc 'string 1 1 2 1)
                                      #\1 '("'.'")))))))

(describe "delay/p"
  (define eval-count 0)
  (define rec/p (or/p (char/p #\.)
                      (list/p (char/p #\a)
                              (delay/p (begin (set! eval-count (add1 eval-count))
                                              rec/p)))))

  (it "delays evaluation of its argument"
    (check-equal? eval-count 0))

  (it "allows a parser to be self-recursive"
    (check-equal? (parse-string rec/p "aaa.")
                  (success '(#\a (#\a (#\a #\.))))))

  (it "only evaluates its argument once"
    (check-equal? (parse-string rec/p "aaa.")
                  (success '(#\a (#\a (#\a #\.)))))
    (check-equal? eval-count 1)))

(describe "many/p"
  (context "when given a letter parser"
    (define many-letters/p (many/p letter/p))
    (it "succeeds when parsing multiple letters"
      (check-equal? (parse-string many-letters/p "abc")
                    (success (list #\a #\b #\c))))
    (it "succeeds when parsing one letter"
      (check-equal? (parse-string many-letters/p "a")
                    (success (list #\a))))
    (it "succeeds with an empty list when unable to parse"
      (check-equal? (parse-string many-letters/p "123")
                    (success (list)))))
  (context "when given a letter parser and dot separator"
    (define many-dotted-letters/p (many/p letter/p #:sep (char/p #\.)))
    (it "succeeds with only letters when parsing dotted letters"
      (check-equal? (parse-string many-dotted-letters/p "a.b.c")
                    (success (list #\a #\b #\c)))))
  (context "when given a letter parser and a maximum count of three"
    (define at-most-three-letters/p (many/p letter/p #:max 3))
    (it "succeeds when parsing three letters"
      (check-equal? (parse-string at-most-three-letters/p "abc")
                    (success (list #\a #\b #\c))))
    (it "succeeds when parsing fewer than three letters"
      (check-equal? (parse-string at-most-three-letters/p "a")
                    (success (list #\a))))
    (it "only consumes three letters when parsing four letters"
      (check-equal? (parse-string at-most-three-letters/p "abcd")
                    (success (list #\a #\b #\c)))))
  (context "when given a maximum count of zero"
    (it "consumes no input"
      (check-equal? (parse-string (many/p letter/p #:max 0) "abc")
                    (success (list)))))
  (context "when given a letter parser and a minimum count of three"
    (define at-least-three-letters/p (many/p letter/p #:min 3))
    (it "succeeds when parsing three letters"
      (check-equal? (parse-string at-least-three-letters/p "abc")
                    (success (list #\a #\b #\c))))
    (it "succeeds when parsing four letters"
      (check-equal? (parse-string at-least-three-letters/p "abcd")
                    (success (list #\a #\b #\c #\d))))
    (it "fails when parsing two letters"
      (check-equal? (parse-string at-least-three-letters/p "ab")
                    (failure (message (srcloc 'string 1 1 2 1)
                                      "end of input"
                                      '("letter"))))))
  (context "when given a minimum count of 2 and a maximum count of 4"
    (define two-to-four-letters/p (many/p letter/p #:min 2 #:max 4))
    (it "succeeds when parsing two letters"
      (check-equal? (parse-string two-to-four-letters/p "ab")
                    (success (list #\a #\b))))
    (it "succeeds when parsing four letters"
      (check-equal? (parse-string two-to-four-letters/p "abcd")
                    (success (list #\a #\b #\c #\d))))
    (it "fails when parsing one letter"
      (check-equal? (parse-string two-to-four-letters/p "a")
                    (failure (message (srcloc 'string 1 0 1 1)
                                      "end of input"
                                      '("letter")))))
    (it "only consumes four letters when given five"
      (check-equal? (parse-string two-to-four-letters/p "abcde")
                    (success (list #\a #\b #\c #\d))))))

(describe "parser parameters"
  (define param (make-parser-parameter #f))

  (it "returns the initial value before being set"
    (check-equal? (parse-string (param) "")
                  (success #f)))

  (it "returns the set value after being set"
    (check-equal? (parse-string (do (param #t) (param)) "")
                  (success #t)))

  (it "unsets the value when backtracking"
    (check-equal? (parse-string (or/p (do (param #t) any-char/p)
                                      (param))
                                "")
                  (success #f)))

  (describe "parameterize/p"
    (it "sets the value with a local extent"
      (check-equal? (parse-string (do [(list a b) <- (parameterize/p ([param 1])
                                                       (do [a <- (param)]
                                                           (param 2)
                                                           [b <- (param)]
                                                           (pure (list a b))))]
                                      [c <- (param)]
                                      (pure (list a b c)))
                                  "")
                    (success (list 1 2 #f))))))
