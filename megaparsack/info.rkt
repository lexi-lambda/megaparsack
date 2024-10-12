#lang info

(define version "1.8")

(define collection 'multi)

(define deps
  '("base"
    ["megaparsack-lib" #:version "1.8"]
    ["megaparsack-doc" #:version "1.8"]
    ["megaparsack-parser" #:version "1.8"]
    ["megaparsack-parser-tools" #:version "1.8"]))
(define build-deps
  '())

(define implies
  '("megaparsack-lib"
    "megaparsack-doc"
    "megaparsack-parser"
    "megaparsack-parser-tools"))
