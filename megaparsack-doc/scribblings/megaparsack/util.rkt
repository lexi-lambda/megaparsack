#lang racket/base

(require (for-label data/functor
                    data/applicative
                    data/monad
                    data/either
                    megaparsack
                    megaparsack/text
                    megaparsack/parser-tools/lex
                    parser-tools/lex
                    (prefix-in : parser-tools/lex-sre)
                    (except-in racket/base do map)
                    racket/contract
                    racket/function)
         scribble/manual
         scribble/example)

(provide (for-label (all-from-out data/functor
                                  data/applicative
                                  data/monad
                                  data/either
                                  megaparsack
                                  megaparsack/text
                                  megaparsack/parser-tools/lex
                                  parser-tools/lex
                                  parser-tools/lex-sre
                                  racket/base
                                  racket/contract
                                  racket/function))
         reftech functech
         make-parser-eval parser-examples parser-interaction define-parser-interaction)

(define (reftech . content)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") content))

(define (functech . content)
  (apply tech #:doc '(lib "scribblings/data/functional.scrbl") content))

(define (make-parser-eval [require-paths '()])
  (let ([eval ((make-eval-factory '()))])
    (eval '(require data/functor
                    data/applicative
                    data/monad
                    data/either
                    megaparsack
                    megaparsack/text
                    megaparsack/parser-tools/lex
                    parser-tools/lex
                    (prefix-in : parser-tools/lex-sre)
                    (except-in racket/base do map)
                    racket/function))
    eval))

(define-syntax-rule (parser-examples body ...)
  (examples #:eval (make-parser-eval) body ...))

(define-syntax-rule (parser-interaction body ...)
  (parser-examples #:label #f body ...))

(define-syntax-rule (define-parser-interaction interaction close-interaction!)
  (begin
    (define eval (make-parser-eval))
    (define-syntax-rule (interaction body (... ...))
      (examples #:eval eval #:label #f body (... ...)))
    (define (close-interaction!)
      (close-eval eval))))
