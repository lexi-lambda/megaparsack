#lang racket/base

(require data/applicative
         data/monad
         megaparsack/base
         racket/contract
         racket/list)

(provide (contract-out
          [many*/p (parser? . -> . parser?)]
          [many+/p (parser? . -> . parser?)]
          [repeat/p (exact-nonnegative-integer? parser? . -> . (parser/c any/c list?))]
          [many/sep*/p (parser? parser? . -> . parser?)]
          [many/sep+/p (parser? parser? . -> . parser?)]))

;; These combinators are special cases of many/p that predated many/p

;; equivalent to (many/p p)
(define (many*/p p)
  (or/p (lazy/p (many+/p p)) (pure '())))

;; equivalent to (many/p p #:min-count 1)
(define (many+/p p)
  ((pure cons) p (lazy/p (many*/p p))))

;; equivalent to (many/p p #:min-count n #:max-count n)
(define (repeat/p n p)
  (if (zero? n)
      (pure '())
      ((pure cons) p (repeat/p (sub1 n) p))))

;; equivalent to (many/p p #:separator sep)
(define (many/sep*/p p sep)
  (or/p (many/sep+/p p sep) (pure '())))

;; equivalent to (many/p p #:separator sep #:min-count 1)
(define (many/sep+/p p sep)
  (do [x <- p]
      [xs <- (many*/p (do sep p))]
      (pure (cons x xs))))
