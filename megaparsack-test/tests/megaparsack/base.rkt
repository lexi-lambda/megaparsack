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
      (list/p letter/p digit/p letter/p #:separator (char/p #\.)))
    (it "succeeds when given separated components"
      (check-equal? (parse-string dotted-letter-digit-letter/p "a.1.b")
                    (success (list #\a #\1 #\b))))
    (it "fails when given unseparated components"
      (check-equal? (parse-string dotted-letter-digit-letter/p "a1b")
                    (failure (message (srcloc 'string 1 0 1 2)
                                      #\1 '("'.'")))))))
