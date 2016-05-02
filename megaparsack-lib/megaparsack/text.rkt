#lang curly-fn racket/base

(require data/applicative
         data/monad
         megaparsack/base
         megaparsack/combinator
         racket/contract
         racket/list
         racket/function)

(provide (contract-out
          [parse-string (->* [parser? string?] [any/c] any/c)]

          [char/p (char? . -> . (parser/c char? char?))]
          [letter/p (parser/c char? char?)]
          [digit/p (parser/c char? char?)]
          [space/p (parser/c char? char?)]
          [integer/p (parser/c char? integer?)]
          [string/p (string? . -> . (parser/c char? string?))]))

(define (parse-string p input [name 'string])
  (parse p (let loop ([pos 1]
                      [line 1]
                      [col 0]
                      [input (string->list input)])
             (if (empty? input)
                 '()
                 (let ([c (first input)])
                   (cons (token c (srcloc name line col pos 1))
                         (if (char=? c #\newline)
                             (loop (add1 pos) (add1 line) 0 (rest input))
                             (loop (add1 pos) line (add1 col) (rest input)))))))))

(define (char/p c) (label/p (format "'~a'" c) (satisfy/p #{char=? c})))
(define letter/p   (label/p "letter" (satisfy/p char-alphabetic?)))
(define digit/p    (label/p "number" (satisfy/p char-numeric?)))
(define space/p    (label/p "whitespace" (satisfy/p (disjoin char-whitespace? char-blank?))))

(define integer/p
  (label/p "integer"
           (do [digits <- (some/p digit/p)]
               (pure (string->number (apply string digits))))))

(define (string/p str)
  (if (zero? (string-length str))
      (pure "")
      (label/p str (do (char/p (string-ref str 0))
                       (string/p (substring str 1))
                       (pure str)))))
