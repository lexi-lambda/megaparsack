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
