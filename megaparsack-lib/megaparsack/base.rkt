#lang racket/base

(require racket/require
         (multi-in data [functor applicative monad either])
         (prefix-in d: data/applicative)
         match-plus
         (multi-in racket [contract function list match port string])
         (prefix-in r: racket/base)
         (submod data/applicative coerce-delayed)
         (for-syntax racket/base
                     syntax/parse))

(provide lazy/p
         (contract-out
          [parser? (any/c . -> . boolean?)]
          [parser/c (contract? contract? . -> . contract?)]

          [rename do-parse parse (parser?* (listof syntax?) . -> . (either/c message? any/c))]
          [parse-error->string (message? . -> . string?)]
          [parse-result! ((either/c message? any/c) . -> . any/c)]
          [struct (exn:fail:megaparsack exn:fail)
            ([message any/c] [continuation-marks any/c]
             [srcloc srcloc?] [unexpected any/c] [expected (listof string?)])]

          [void/p (parser/c any/c void?)]
          [or/p (parser?* parser?* ... . -> . parser?*)]
          [try/p (parser?* . -> . parser?*)]
          [satisfy/p ((any/c . -> . any/c) . -> . parser?*)]
          [label/p (string? parser?* . -> . parser?*)]
          [hidden/p (parser?* . -> . parser?*)]))

;; supporting types
;; ---------------------------------------------------------------------------------------------------

(struct consumed (reply) #:transparent)
(struct empty (reply) #:transparent)

(struct error (message) #:transparent)
(struct ok (result rest message) #:transparent)
(struct message (srcloc unexpected expected)
  #:transparent
  #:methods gen:custom-write
  [(define/match* (write-proc (message srcloc unexpected expected) out mode)
     (fprintf out (if mode "(message ~v ~v ~v)" "(message ~a ~a ~a)")
              srcloc unexpected expected))])

(define empty-srcloc (srcloc #f #f #f #f #f))
(define empty-message (message empty-srcloc #f '()))

;; syntax object pattern-matching
;; ---------------------------------------------------------------------------------------------------

(define (syntax->values stx)
  (values (syntax-e stx)
          (srcloc (syntax-source stx)
                  (syntax-line stx)
                  (syntax-column stx)
                  (syntax-position stx)
                  (syntax-span stx))))

(define-match-expander syntax
  (syntax-parser
    [(_ datum srcloc)
     #'(? syntax? (app syntax->values datum srcloc))]))

;; core primitives
;; ---------------------------------------------------------------------------------------------------

(struct parser (proc)
  #:methods gen:functor
  [(define (map f p)
     (parser (compose (match-lambda [(consumed (ok v rest message)) (consumed (ok (f v) rest message))]
                                    [(empty (ok v rest message))    (empty (ok (f v) rest message))]
                                    [error                          error])
                      (parser-proc p))))]

  #:methods gen:applicative
  [(define (pure _ x)
     (parser (λ (input) (empty (ok x input empty-message)))))

   (define (apply p ps)
     (do [f  <- p]
         [xs <- (map/m values ps)]
         (d:pure (r:apply f xs))))]

  #:methods gen:monad
  [(define (chain f p)
     (parser
      (λ (input)
        (match (parse p input)
          [(empty    (ok x rest message)) (match (parse (f x) rest)
                                            [(empty reply) (empty (merge-message/reply message reply))]
                                            [consumed      consumed])]
          [(consumed (ok x rest message)) (consumed (match (parse (f x) rest)
                                                      [(consumed reply) reply]
                                                      [(empty    reply) (merge-message/reply message reply)]))]
          [error                          error]))))])

(define (parser?* v)
  (or (parser? v) (pure? v)))

(define void/p
  (parser (λ (input) (empty (ok (void) input)))))
(define coerce-parser
  (coerce-pure void/p))

(define (parse p input)
  ((parser-proc (coerce-parser p)) input))

(define (parser/c input/c output/c)
  (or/c (pure/c output/c)
        (let* (; Ideally, seq/c would be a more expressive contract, something like this:
               ;   (listof (struct/c token input/c srcloc?))
               ; but this turns out to be horribly slow in practice. Hopefully there is a better way
               ; to protect the input/c invariant without killing performance (should try using a
               ; custom contract).
               [seq/c any/c]
               [reply/c (or/c error? (struct/c ok output/c seq/c message?))])
          (struct/c parser (-> any/c (or/c (struct/c consumed reply/c)
                                           (struct/c empty reply/c)))))))

(define (do-parse p input)
  (match (parse p input)
    [(or (consumed (ok x _ _))
         (empty    (ok x _ _)))
     (right x)]
    [(or (consumed (error message))
         (empty    (error message)))
     (left message)]))

(define/match* (parse-error->string (message srcloc unexpected expected))
  (with-output-to-string
   (thunk (display (or (srcloc->string srcloc) "?"))
          (display ": parse error")

          (when (and (not unexpected) (null? expected))
            (display ";")
            (newline)
            (display " unknown parse error"))

          (when unexpected
            (newline)
            (display "  unexpected: ")
            (display unexpected))

          (unless (null? expected)
            (newline)
            (display "  expected: ")
            (display (if (< (length expected) 3)
                         (string-join expected " or ")
                         (string-join expected ", " #:before-last ", or ")))))))

(struct exn:fail:megaparsack exn:fail (srcloc unexpected expected) #:transparent)

(define/match (parse-result! result)
  [((right result)) result]
  [((left (and message (message srcloc unexpected expected))))
   (raise (exn:fail:megaparsack (parse-error->string message)
                                (current-continuation-marks)
                                srcloc unexpected expected))])

;; error message reconciliation
;; ---------------------------------------------------------------------------------------------------

(define/match* (merge-messages (message loc unexpected-a expected-a) (message _ unexpected-b expected-b))
  (message loc (or unexpected-a unexpected-b) (append expected-a expected-b)))

(define (merge-ok x rest message-a message-b)
  (empty (ok x rest (merge-messages message-a message-b))))

(define (merge-error message-a message-b)
  (empty (error (merge-messages message-a message-b))))

(define (merge-message/reply message-a reply)
  (match reply
    [(ok x rest message-b) (ok x rest (merge-messages message-a message-b))]
    [(error message-b)     (error (merge-messages message-a message-b))]))

;; laziness (lazy/p)
;; ---------------------------------------------------------------------------------------------------

(define-syntax-rule (lazy/p p)
  (parser (λ (input) (parse p input))))

;; choice (or/p)
;; ---------------------------------------------------------------------------------------------------

; binary version of or/p
(define (p . <or> . q)
  (parser
   (λ (input)
     (match (parse p input)
       [(empty (error message-a))     (match (parse q input)
                                        [(empty (error message-b))     (merge-error message-a message-b)]
                                        [(empty (ok x rest message-b)) (merge-ok x rest message-a message-b)]
                                        [consumed                      consumed])]
       [(empty (ok x rest message-a)) (match (parse q input)
                                        [(empty (error message-b))     (merge-error message-a message-b)]
                                        [(empty (ok _ _ message-b))    (merge-ok x rest message-a message-b)]
                                        [consumed                      consumed])]
       [consumed                      consumed]))))

(define (or/p . ps)
  (let-values ([(ps p) (split-at ps (sub1 (length ps)))])
    (foldr <or> (first p) ps)))

;; lookahead (try/p)
;; ---------------------------------------------------------------------------------------------------

(define (try/p p)
  (parser
   (λ (input)
     (match (parse p input)
       [(consumed (error message)) (empty (error message))]
       [other                      other]))))

;; conditional (satisfy/p)
;; ---------------------------------------------------------------------------------------------------

(define (satisfy/p proc)
  (parser
   (match-lambda
     [(list (syntax (? proc c) loc) cs ...) (consumed (ok c cs (message loc #f '())))]
     [(list (syntax c loc) _ ...)           (empty (error (message loc c '())))]
     [_                                     (empty (error (message empty-srcloc "end of input" '())))])))

;; parser annotation (label/p & hidden/p)
;; ---------------------------------------------------------------------------------------------------

(define/match* (expect (message srcloc unexpected _) expected)
  (message srcloc unexpected (list expected)))

(define/match* (hide (message srcloc unexpected _))
  (message srcloc unexpected '()))

(define (label/p str p)
  (parser
   (λ (input)
     (match (parse p input)
       [(empty (error message))     (empty (error (expect message str)))]
       [(empty (ok x rest message)) (empty (ok x rest (expect message str)))]
       [other                       other]))))

(define (hidden/p p)
  (parser
   (λ (input)
     (match (parse p input)
       [(empty (error message))     (empty (error (hide message)))]
       [(empty (ok x rest message)) (empty (ok x rest (hide message)))]
       [other                       other]))))
