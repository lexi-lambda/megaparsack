#lang curly-fn racket/base

(require data/applicative
         data/monad
         megaparsack/base
         racket/contract)

(provide (contract-out
          [many/p (parser? . -> . parser?)]
          [some/p (parser? . -> . parser?)]
          [repeat/p (exact-nonnegative-integer? parser? . -> . (parser/c any/c list?))]
          [==/p (->* [any/c] [(any/c any/c . -> . any/c)] parser?)]
          [many/sep/p (parser? parser? . -> . parser?)]
          [some/sep/p (parser? parser? . -> . parser?)]))

(define (many/p p)
  (or/p (lazy/p (some/p p)) (pure '())))

(define (some/p p)
  ((pure cons) p (lazy/p (many/p p))))

(define (repeat/p n p)
  (if (zero? n)
      (pure '())
      ((pure cons) p (repeat/p (sub1 n) p))))

(define (==/p v [=? equal?])
  (satisfy/p #{=? v}))

(define (many/sep/p p sep)
  (or/p (some/sep/p p sep) (pure '())))

(define (some/sep/p p sep)
  (do [x <- p]
      [xs <- (many/p (do sep p))]
      (pure (cons x xs))))
