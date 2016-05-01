#lang curly-fn racket/base

(require data/applicative
         data/monad
         megaparsack/base
         racket/contract)

(provide (contract-out
          [many/p (parser? . -> . parser?)]
          [some/p (parser? . -> . parser?)]
          [==/p (->* [any/c] [(any/c any/c . -> . any/c)] parser?)]
          [many/sep/p (parser? parser? . -> . parser?)]
          [some/sep/p (parser? parser? . -> . parser?)]))

(define (many/p p)
  (or/p (lazy/p (some/p p)) (pure '())))

(define (some/p p)
  ((pure cons) p (lazy/p (many/p p))))

(define (==/p v [=? equal?])
  (satisfy/p #{=? v}))

(define (many/sep/p p sep)
  (or/p (some/sep/p p sep) (pure '())))

(define (some/sep/p p sep)
  (do [x <- p]
      [xs <- (or/p (do sep (some/sep/p p sep))
                   (pure '()))]
      (pure (cons x xs))))
