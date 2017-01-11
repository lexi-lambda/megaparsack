#lang curly-fn racket/base

(require data/either
         megaparsack
         megaparsack/text
         rackunit
         rackunit/spec)

(describe "fail/p"
  (it "always fails"
    (let* ([loc (srcloc #f #f #f #f #f)]
           [msg (message loc
                         "something"
                         (list "something else"))]
           [p (fail/p msg)])
      (check-equal? (parse-string p "")
                    (failure msg)))))

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
                    ;; I have no idea why the source locations are lost here
                    (failure (message (srcloc #f #f #f #f #f)
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
                    (failure (message (srcloc 'string 1 0 1 2)
                                      #\1 '("'.'")))))))

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
