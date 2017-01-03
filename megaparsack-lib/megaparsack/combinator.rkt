#lang curly-fn racket/base

(require data/applicative
         data/monad
         megaparsack/base
         racket/contract)

(provide (contract-out
          [many*/p (parser? . -> . parser?)]
          [many+/p (parser? . -> . parser?)]
          [repeat/p (exact-nonnegative-integer? parser? . -> . (parser/c any/c list?))]
          [==/p (->* [any/c] [(any/c any/c . -> . any/c)] parser?)]
          [many/sep*/p (parser? parser? . -> . parser?)]
          [many/sep+/p (parser? parser? . -> . parser?)]
          [guard/p (->* [parser? (any/c . -> . any/c)]
                        [(or/c string? #f) (any/c . -> . any/c)]
                        parser?)]))

(define (many*/p p)
  (or/p (lazy/p (many+/p p)) (pure '())))

(define (many+/p p)
  ((pure cons) p (lazy/p (many*/p p))))

(define (repeat/p n p)
  (if (zero? n)
      (pure '())
      ((pure cons) p (repeat/p (sub1 n) p))))

(define (==/p v [=? equal?])
  (satisfy/p #{=? v}))

(define (many/sep*/p p sep)
  (or/p (many/sep+/p p sep) (pure '())))

(define (many/sep+/p p sep)
  (do [x <- p]
      [xs <- (many*/p (do sep p))]
      (pure (cons x xs))))

(define (guard/p p pred? [expected #f] [mk-unexpected values])
  (do [s <- (syntax-box/p p)]
      (define v (syntax-box-datum s))
      (if (pred? v)
          (pure v)
          (fail/p (message (syntax-box-srcloc s)
                           (mk-unexpected v)
                           (if expected (list expected) '()))))))
