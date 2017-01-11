#lang curly-fn racket/base

(require data/applicative
         data/monad
         megaparsack/base
         racket/contract
         racket/list)

(provide (contract-out
          [many/p (->* [parser?]
                       [#:sep parser?
                        #:min exact-nonnegative-integer?
                        #:max (or/c exact-nonnegative-integer? +inf.0)]
                       (parser/c any/c list?))]
          [many+/p (->* [parser?]
                        [#:sep parser?
                         #:max (or/c exact-nonnegative-integer? +inf.0)]
                        (parser/c any/c list?))]
          [many*/p (parser? . -> . parser?)]
          [repeat/p (exact-nonnegative-integer? parser? . -> . (parser/c any/c list?))]
          [many/sep*/p (parser? parser? . -> . parser?)]
          [many/sep+/p (parser? parser? . -> . parser?)]
          [==/p (->* [any/c] [(any/c any/c . -> . any/c)] parser?)]
          [guard/p (->* [parser? (any/c . -> . any/c)]
                        [(or/c string? #f) (any/c . -> . any/c)]
                        parser?)]
          [list/p (->* []
                       [#:sep parser?]
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

(define (list/p #:sep [sep void/p] . ps)
  (cond [(empty? ps) (pure '())]
        [(empty? (rest ps)) ((pure list) (first ps))]
        [else
         ((pure list*) (do [v <- (first ps)] sep (pure v))
                       (apply list/p #:sep sep (rest ps)))]))

(define (many/p p
                #:sep [sep void/p]
                #:min [min-count 0]
                #:max [max-count +inf.0])
  (define (loop-mandatory p
                          #:min [min-count min-count]
                          #:max [max-count max-count]
                          #:recur-parser [recur p])
    (cond [(zero? min-count) (loop-optional p max-count #:recur-parser recur)]
          [else
           (define rest/p
             (loop-mandatory recur
                             #:min (sub1 min-count)
                             #:max (sub1 max-count)))
           ((pure cons) p rest/p)]))
  (define (loop-optional p max-count #:recur-parser [recur p])
    (if (zero? max-count)
        (pure '())
        (or/p (lazy/p ((pure cons) p (loop-optional recur (sub1 max-count))))
              (pure '()))))
  (loop-mandatory p #:recur-parser (do sep p)))

(define (many+/p p #:sep [sep void/p] #:max [max-count +inf.0])
  (many/p p #:sep sep #:min 1 #:max max-count))

;; These combinators are special cases of many/p that predated many/p

(define (many*/p p) (many/p p))
(define (repeat/p n p) (many/p p #:min n #:max n))
(define (many/sep*/p p sep) (many/p p #:sep sep))
(define (many/sep+/p p sep) (many+/p p #:sep sep))
