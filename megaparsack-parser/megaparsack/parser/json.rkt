#lang curly-fn racket/base

(require racket/require
         (multi-in data [collection applicative monad])
         (multi-in megaparsack [base combinator text]))

(provide parse-json-string)

(define spaces/p (many/p (hidden/p space/p)))

(define positive-number/p
  (or/p (try/p (do [i <- (or/p integer/p (pure 0))]
                   (char/p #\.)
                   [f <- integer/p]
                   (pure (string->number (format "~a.~a" i f)))))
        integer/p))

(define number/p
  (label/p
   "number"
   (or/p (do (char/p #\-)
             [n <- positive-number/p]
             (pure (- n)))
         positive-number/p)))

(define hex-digit/p
  (or/p (do (char/p    #\0) (pure  0))
        (do (char/p    #\1) (pure  1))
        (do (char/p    #\2) (pure  2))
        (do (char/p    #\3) (pure  3))
        (do (char/p    #\4) (pure  4))
        (do (char/p    #\5) (pure  5))
        (do (char/p    #\6) (pure  6))
        (do (char/p    #\7) (pure  7))
        (do (char/p    #\8) (pure  8))
        (do (char/p    #\9) (pure  9))
        (do (char-ci/p #\a) (pure 10))
        (do (char-ci/p #\b) (pure 11))
        (do (char-ci/p #\c) (pure 12))
        (do (char-ci/p #\d) (pure 13))
        (do (char-ci/p #\e) (pure 14))
        (do (char-ci/p #\f) (pure 15))))

(define string-char-or-escape/p
  (or/p (do (char/p #\\)
            (or/p (char/p #\")
                  (char/p #\\)
                  (char/p #\/)
                  (do (char/p #\b) (pure #\backspace))
                  (do (char/p #\f) (pure #\page))
                  (do (char/p #\n) (pure #\newline))
                  (do (char/p #\r) (pure #\return))
                  (do (char/p #\t) (pure #\tab))
                  (do (char/p #\u)
                      [ns <- (repeat/p 4 hex-digit/p)]
                      (define code (foldl #{+ %2 (* 16 %1)} 0 ns))
                      (pure (integer->char code)))))
        (satisfy/p #{not (char=? #\" %)})))

(define quoted-string/p
  (label/p
   "string"
   (do (char/p #\")
       [chars <- (many/p string-char-or-escape/p)]
       (char/p #\")
       (pure (apply string chars)))))

(define boolean/p
  (label/p
   "boolean"
   (or/p (do (string/p "true")  (pure #t))
         (do (string/p "false") (pure #f)))))

(define null/p
  (do (string/p "null") (pure 'null)))

(define object-pair/p
  (do spaces/p
      [k <- quoted-string/p]
      spaces/p
      (char/p #\:)
      spaces/p
      [v <- value/p]
      spaces/p
      (pure (cons (string->symbol k) v))))

(define object/p
  (label/p
   "object"
   (do (char/p #\{)
       [pairs <- (many/sep/p object-pair/p (char/p #\,))]
       spaces/p
       (char/p #\})
       (pure (make-immutable-hasheq (sequence->list pairs))))))

(define array/p
  (label/p
   "array"
   (do (char/p #\[)
       [elems <- (many/sep/p (do spaces/p value/p) (char/p #\,))]
       spaces/p
       (char/p #\])
       (pure elems))))

(define value/p
  (do spaces/p
      (or/p number/p
            quoted-string/p
            boolean/p
            null/p
            object/p
            array/p)))

(define entire-value/p
  (do [v <- value/p]
      eof/p
      (pure v)))

(define (parse-json-string str [name 'json])
  (parse-string entire-value/p str name))
