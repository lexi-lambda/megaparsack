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
          [parse-stx-string (-> parser? syntax? any/c)]

          [char/p (char? . -> . (parser/c char? char?))]
          [char-ci/p (char? . -> . (parser/c char? char?))]
          [letter/p (parser/c char? char?)]
          [digit/p (parser/c char? char?)]
          [symbolic/p (parser/c char? char?)]
          [space/p (parser/c char? char?)]
          [integer/p (parser/c char? integer?)]
          [string/p (string? . -> . (parser/c char? string?))]))

(define (make-syntax-boxes name pos line col input)
  (if (empty? input)
      '()
      (let ([c (first input)])
        (cons (syntax-box c (srcloc name line col pos 1))
              (if (char=? c #\newline)
                  (make-syntax-boxes name (add1 pos) (add1 line) 0 (rest input))
                  (make-syntax-boxes name (add1 pos) line (add1 col) (rest input)))))))

(define (parse-string p input [name 'string])
  (parse p (make-syntax-boxes name 1 1 0 (string->list input))))

(define (parse-stx-string p stx-string)
  (define name (syntax-source stx-string))
  (parse p (make-syntax-boxes (syntax-source stx-string)
                              (add1 (syntax-position stx-string))
                              (syntax-line stx-string)
                              (add1 (syntax-column stx-string))
                              (string->list (syntax->datum stx-string)))))

(define (char/p c)    (label/p (format "'~a'" c) (satisfy/p #{char=? c})))
(define (char-ci/p c) (label/p (format "'~a'" c) (satisfy/p #{char-ci=? c})))
(define letter/p      (label/p "letter" (satisfy/p char-alphabetic?)))
(define digit/p       (label/p "number" (satisfy/p char-numeric?)))
(define symbolic/p    (label/p "symbolic" (satisfy/p char-symbolic?)))
(define space/p       (label/p "whitespace" (satisfy/p (disjoin char-whitespace? char-blank?))))

(define integer/p
  (label/p "integer"
           (do [digits <- (many+/p digit/p)]
               (pure (string->number (apply string digits))))))

(define (string/p str)
  (if (zero? (string-length str))
      (pure "")
      (label/p str (do (char/p (string-ref str 0))
                       (string/p (substring str 1))
                       (pure str)))))
