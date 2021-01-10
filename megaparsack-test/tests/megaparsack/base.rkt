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
    (check-equal? (parse-string (or/p (pure 'eof)
                                      (do letter/p letter/p))
                                "a")
                  (failure (message (srcloc 'string 1 0 1 1)
                                    "end of input"
                                    '("letter"))))))

(describe "try/p"
  (define any-any-digit/p (try/p (list/p any-char/p any-char/p digit/p)))
  (define any-letter-letter/p (try/p (list/p any-char/p letter/p letter/p)))
  (define complex/p (or/p any-any-digit/p any-letter-letter/p))
  (it "succeeds with successful parse when the parser backtracks"
    (check-equal? (parse-string complex/p "abc")
                  (success '(#\a #\b #\c))))
  (it "fails with last-matched source location when the parser backtracks"
    (check-equal? (parse-string complex/p "a1!")
                  (failure (message (srcloc 'string 1 1 2 2)
                                    #\!
                                    '("number" "letter"))))))

(describe "eof/p"
  (it "succeeds with empty input"
    (check-equal? (parse-string eof/p "")
                  (success (void))))

  (it "fails non-empty input"
    (check-equal? (parse-string eof/p "!")
                  (failure (message (srcloc 'string 1 0 1 1)
                                    #\!
                                    '("end of input"))))))

(describe "fail/p"
  (it "always fails"
    (let* ([loc (srcloc #f #f #f #f #f)]
           [msg (message loc
                         "something"
                         (list "something else"))]
           [p (fail/p msg)])
      (check-equal? (parse-string p "")
                    (failure msg)))))
