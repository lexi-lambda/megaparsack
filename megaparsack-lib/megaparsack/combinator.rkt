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
          [repeat/p (exact-nonnegative-integer? parser? . -> . (parser/c any/c list?))]

          [many-until/p (->* [parser?
                              #:end parser?]
                             [#:sep parser?
                              #:min exact-nonnegative-integer?]
                             (parser/c any/c (list/c list? any/c)))]
          [many+-until/p (->* [parser?
                               #:end parser?]
                              [#:sep parser?]
                              (parser/c any/c (list/c list? any/c)))]

          [many*/p (parser? . -> . parser?)]
          [many/sep*/p (parser? parser? . -> . parser?)]
          [many/sep+/p (parser? parser? . -> . parser?)]

          [==/p (->* [any/c] [(any/c any/c . -> . any/c)] parser?)]
          [one-of/p (->* [list?] [(any/c any/c . -> . any/c)] parser?)]
          [list/p (->* []
                       [#:sep parser?]
                       #:rest (listof parser?)
                       (parser/c any/c list?))]))

(define (==/p v [=? equal?])
  (label/p (format "~a" v)
           (satisfy/p #{=? v %})))

(define (one-of/p vs [=? equal?])
  (apply or/p (map #{==/p % =?} vs)))

(define (list/p #:sep [sep void/p] . ps)
  (cond [(empty? ps) (pure '())]
        [(empty? (rest ps)) ((pure list) (first ps))]
        [else
         ((pure list*) (do [v <- (first ps)] sep (pure v))
                       (apply list/p #:sep sep (rest ps)))]))

(define (many/p p
                #:sep [sep-p void/p]
                #:min [min-count 0]
                #:max [max-count +inf.0])
  (define (loop-mandatory p [min-left min-count]
                          #:recur-parser [recur p])
    (if (zero? min-left)
        (loop-optional p #:recur-parser recur)
        ((pure cons) p (lazy/p (loop-mandatory recur (sub1 min-left))))))

  (define (loop-optional p [max-left (- max-count min-count)]
                         #:recur-parser [recur p])
    (if (<= max-left 0)
        (pure '())
        (or/p ((pure cons) p (lazy/p (loop-optional recur (sub1 max-left))))
              (pure '()))))

  (loop-mandatory p #:recur-parser (do sep-p p)))

(define (many+/p p #:sep [sep void/p] #:max [max-count +inf.0])
  (many/p p #:sep sep #:min 1 #:max max-count))

(define (repeat/p n p) (many/p p #:min n #:max n))

(define (many-until/p p #:end end-p
                      #:sep [sep-p void/p]
                      #:min [min-count 0])
  (define (loop-mandatory p
                          [results '()]
                          [min-left min-count]
                          #:recur-parser [recur p])
    (if (zero? min-left)
        (loop-optional p results #:recur-parser recur)
        (do [result <- p]
            (loop-mandatory recur (cons result results) (sub1 min-left)))))

  (define (loop-optional p results #:recur-parser [recur p])
    (or/p (do [end-result <- end-p]
              (pure (list (reverse results) end-result)))
          (do [result <- p]
              (loop-optional recur (cons result results)))))

  (loop-mandatory p #:recur-parser (do sep-p p)))

(define (many+-until/p p #:end end-p #:sep [sep-p void/p])
  (many-until/p p #:end end-p #:sep sep-p #:min 1))

;; These combinators are special cases of many/p that predated many/p

(define (many*/p p) (many/p p))
(define (many/sep*/p p sep) (many/p p #:sep sep))
(define (many/sep+/p p sep) (many+/p p #:sep sep))
