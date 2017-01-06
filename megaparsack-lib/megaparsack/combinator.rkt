#lang curly-fn racket/base

(require data/applicative
         data/monad
         megaparsack/base
         racket/contract
         racket/list
         "combinator-deprecated.rkt")

(provide (all-from-out "combinator-deprecated.rkt")
         (contract-out
          [many/p (->* [parser?]
                       [#:separator parser?
                        #:min-count exact-nonnegative-integer?
                        #:max-count (or/c exact-nonnegative-integer? +inf.0)]
                       (parser/c any/c list?))]
          [==/p (->* [any/c] [(any/c any/c . -> . any/c)] parser?)]
          [guard/p (->* [parser? (any/c . -> . any/c)]
                        [(or/c string? #f) (any/c . -> . any/c)]
                        parser?)]
          [list/p (->* []
                       [#:separator parser?]
                       #:rest (listof parser?)
                       (parser/c any/c list?))]))

(define (==/p v [=? equal?])
  (satisfy/p #{=? v}))

(define (guard/p p pred? [expected #f] [mk-unexpected values])
  (do [s <- (syntax-box/p p)]
      (define v (syntax-box-datum s))
      (if (pred? v)
          (pure v)
          (fail/p (message (syntax-box-srcloc s)
                           (mk-unexpected v)
                           (if expected (list expected) '()))))))

(define (list/p #:separator [sep void/p] . ps)
  (cond [(empty? ps) (pure '())]
        [(empty? (rest ps)) ((pure list) (first ps))]
        [else
         ((pure list*) (do [v <- (first ps)] sep (pure v))
                       (apply list/p #:separator sep (rest ps)))]))

(define (many/p p
                #:separator [sep void/p]
                #:min-count [min-count 0]
                #:max-count [max-count +inf.0])
  (define (loop-mandatory p
                          #:min-count [min-count min-count]
                          #:max-count [max-count max-count]
                          #:recur-parser [recur p])
    (cond [(zero? min-count) (loop-optional p max-count #:recur-parser recur)]
          [else
           (define rest/p
             (loop-mandatory recur
                             #:min-count (sub1 min-count)
                             #:max-count (sub1 max-count)))
           ((pure cons) p rest/p)]))
  (define (loop-optional p max-count #:recur-parser [recur p])
    (if (zero? max-count)
        (pure '())
        (or/p (lazy/p ((pure cons) p (loop-optional recur (sub1 max-count))))
              (pure '()))))
  (loop-mandatory p #:recur-parser (do sep p)))
