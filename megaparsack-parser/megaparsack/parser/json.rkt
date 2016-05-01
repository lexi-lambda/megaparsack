#lang curly-fn racket/base

(require racket/require
         (multi-in data [collection applicative monad])
         (multi-in megaparsack [base combinator text]))

(provide parse-json-string)

(define spaces/p (many/p (hidden/p space/p)))

(define positive-number/p
  (or/p (try/p (do [i <- integer/p]
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

(define quoted-string/p
  (label/p
   "string"
   (do (char/p #\")
       [chars <- (many/p (satisfy/p #{not (char=? #\" %)}))]
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
      (pure (cons k v))))

(define object/p
  (label/p
   "object"
   (do (char/p #\{)
       [pairs <- (many/sep/p object-pair/p (char/p #\,))]
       spaces/p
       (char/p #\})
       (pure (make-immutable-hash (sequence->list pairs))))))

(define array/p
  (label/p
   "array"
   (do (char/p #\[)
       [elems <- (many/sep/p ((pure #{values %2}) spaces/p value/p) (char/p #\,))]
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

(define (parse-json-string str [name 'json])
  (parse-string value/p str name))
