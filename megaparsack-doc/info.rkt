#lang info

(define version "1.8")

(define collection 'multi)

(define deps
  '())
(define build-deps
  '("base"
    "functional-doc"
    "functional-lib"
    ["megaparsack-lib" #:version "1.8"]
    "megaparsack-parser-tools"
    "parser-tools-doc"
    "parser-tools-lib"
    "racket-doc"
    "scribble-lib"))
