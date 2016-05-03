#lang info

(define collection 'multi)

(define deps
  '("base"
    "megaparsack-lib"
    "megaparsack-doc"
    "megaparsack-parser"))
(define build-deps
  '())

(define implies
  '("megaparsack-lib"
    "megaparsack-doc"
    "megaparsack-parser"))
