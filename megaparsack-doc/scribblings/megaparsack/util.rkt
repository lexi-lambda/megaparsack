#lang racket/base

(require scribble/manual)

(provide reftech functech)

(define (reftech . content)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") content))

(define (functech . content)
  (apply tech #:doc '(lib "scribblings/data/functional.scrbl") content))
