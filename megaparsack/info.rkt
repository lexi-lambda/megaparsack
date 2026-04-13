#lang info

(define version "1.8.1")
(define license 'ISC)

(define collection 'multi)

(define deps
  '("base"
    ["megaparsack-lib" #:version "1.8.1"]
    ["megaparsack-doc" #:version "1.8.1"]
    ["megaparsack-parser" #:version "1.8.1"]
    ["megaparsack-parser-tools" #:version "1.8.1"]))
(define build-deps
  '())

(define implies
  '("megaparsack-lib"
    "megaparsack-doc"
    "megaparsack-parser"
    "megaparsack-parser-tools"))
