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
          [parse-syntax-string (-> parser? (syntax/c string?) any/c)]
          [char/p (char? . -> . (parser/c char? char?))]
          [char-not/p (char? . -> . (parser/c char? char?))]
          [char-ci/p (char? . -> . (parser/c char? char?))]
          [char-between/p (char? char? . -> . (parser/c char? char?))]
          [char-in/p (string? . -> . (parser/c char? char?))]
          [char-not-in/p (string? . -> . (parser/c char? char?))]
          [any-char/p (parser/c char? char?)]
          [letter/p (parser/c char? char?)]
          [digit/p (parser/c char? char?)]
          [symbolic/p (parser/c char? char?)]
          [space/p (parser/c char? char?)]
          [integer/p (parser/c char? integer?)]
          [string/p (string? . -> . (parser/c char? string?))]
          [string-ci/p (string? . -> . (parser/c char? string?))]))

(define (chars->syntax-boxes chars name pos line col)
  (if (empty? chars)
      '()
      (let ([c (first chars)])
        (cons (syntax-box c (srcloc name line col pos 1))
              (if (char=? c #\newline)
                  (chars->syntax-boxes (rest chars) name (add1 pos) (add1 line) 0)
                  (chars->syntax-boxes (rest chars) name (add1 pos) line (add1 col)))))))

(define (parse-string p input [name 'string])
  (parse p (chars->syntax-boxes (string->list input) name 1 1 0)))

(define (parse-syntax-string p stx-string)
  (parse p (chars->syntax-boxes (string->list (syntax->datum stx-string))
                                (syntax-source stx-string)
                                (syntax-position stx-string)
                                (syntax-line stx-string)
                                (syntax-column stx-string))))

(define (char/p c)    (label/p (format "'~a'" c) (satisfy/p #{char=? c})))
(define (char-not/p c) (label/p (format "not '~a'" c) (satisfy/p #{char=/? c})))
(define (char-ci/p c) (label/p (format "'~a'" c) (satisfy/p #{char-ci=? c})))
(define letter/p      (label/p "letter" (satisfy/p char-alphabetic?)))
(define digit/p       (label/p "number" (satisfy/p char-numeric?)))
(define symbolic/p    (label/p "symbolic" (satisfy/p char-symbolic?)))
(define space/p       (label/p "whitespace" (satisfy/p (disjoin char-whitespace? char-blank?))))
(define any-char/p    (label/p "any character" (satisfy/p (lambda (_) #t))))

(define integer/p
  (label/p "integer"
           (do [digits <- (many+/p digit/p)]
               (pure (string->number (apply string digits))))))

(define (string/p str)
  (define len (string-length str))
  (if (zero? len)
      (pure "")
      (label/p str
        (let loop ([i 0])
          (if (< i len)
              (do (char/p (string-ref str i))
                  (loop (add1 i)))
              (pure str))))))

(define (string-ci/p str)
  (define len (string-length str))
  (if (zero? len)
      (pure "")
      (label/p str
        (let loop ([i 0] [cs '()])
          (if (< i len)
              (do [c <- (char-ci/p (string-ref str i))]
                  (loop (add1 i) (cons c cs)))
              (pure (list->string (reverse cs))))))))

(define (char-between/p low high)
  (label/p (format "a character between '~a' and '~a'" low high)
           (satisfy/p #{char<=? low % high})))

(define (char-in/p str)
  (apply or/p (map char/p (string->list str))))

(define (char-not-in/p str)
  (satisfy/p #{string-not-member? str}))

;;;
;; char and string utility functions
(define (char=/? c k)
  (not (char=? c k)))

(define (char-ci=/? c k)
  (not (char-ci=? c k)))

(define (string-member? str c)
  (for/or ([k str])
    (char=? k c)))

(define (string-not-member? str c)
  (not (string-member? str c)))
