#lang racket/base

(require megaparsack
         megaparsack/text
         rackunit
         rackunit/spec)

(describe "parse-syntax-string"
  (it "uses source location information from the provided syntax object"
    (let* ([stx #'"hello, world!"]
           [result (parse-result! (parse-syntax-string (syntax/p (string/p "hello")) stx))])
      (check-equal? (syntax-source stx) (syntax-source result))
      (check-equal? (syntax-line stx) (syntax-line result))
      (check-equal? (syntax-column stx) (syntax-column result))
      (check-equal? (syntax-position stx) (syntax-position result))
      (check-equal? (syntax-span result) 5))))
