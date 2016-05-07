#lang info

(define collection 'multi)

(define deps
  '("base"
    "megaparsack-lib"
    "megaparsack-doc"
    "megaparsack-parser"
    "megaparsack-parser-tools"))
(define build-deps
  '())

(define implies
  '("megaparsack-lib"
    "megaparsack-doc"
    "megaparsack-parser"
    "megaparsack-parser-tools"))
