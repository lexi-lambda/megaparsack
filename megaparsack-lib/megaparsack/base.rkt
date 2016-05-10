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
          [struct syntax-box ([datum any/c] [srcloc srcloc?])]

          [parser? (any/c . -> . boolean?)]
          [parser/c (contract? contract? . -> . contract?)]

          [rename parse-datum parse (parser?* (listof syntax-box?) . -> . (either/c message? any/c))]
          [parse-error->string (message? . -> . string?)]
          [parse-result! ((either/c message? any/c) . -> . any/c)]
          [struct (exn:fail:read:megaparsack exn:fail)
            ([message any/c] [continuation-marks any/c] [srclocs any/c]
             [unexpected any/c] [expected (listof string?)])]

          [void/p (parser/c any/c void?)]
          [or/p (parser?* parser?* ... . -> . parser?*)]
          [try/p (parser?* . -> . parser?*)]
          [satisfy/p ((any/c . -> . any/c) . -> . parser?*)]
          [eof/p (parser/c any/c void?)]
          [syntax/p (parser?* . -> . (parser/c any/c syntax?))]
          [label/p (string? parser?* . -> . parser?*)]
          [hidden/p (parser?* . -> . parser?*)]))

;; supporting types
;; ---------------------------------------------------------------------------------------------------

(struct syntax-box (datum srcloc)
  #:transparent
  #:methods gen:functor
  [(define/match* (map f (syntax-box datum srcloc))
     (syntax-box (f datum) srcloc))])

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

;; core primitives
;; ---------------------------------------------------------------------------------------------------

(struct parser (proc)
  #:methods gen:functor
  [(define (map f p)
     (parser (compose (match-lambda [(consumed (ok v rest message)) (consumed (ok (map f v) rest message))]
                                    [(empty (ok v rest message))    (empty (ok (map f v) rest message))]
                                    [error                          error])
                      (parser-proc p))))]

  #:methods gen:applicative
  [(define (pure _ x)
     (pure/p x))

   (define (apply p ps)
     (do [f  <- p]
         [xs <- (map/m values ps)]
         (d:pure (r:apply f xs))))]

  #:methods gen:monad
  [(define (chain f p)
     (parser
      (λ (input)
        (match (parse p input)
          [(empty (ok (and foo (syntax-box x _)) rest message))
           (match (parse (f x) rest)
             [(empty reply) (empty (merge-message/reply message reply))]
             [consumed      consumed])]
          [(consumed (ok (and foo (syntax-box x srcloc)) rest message))
           (consumed (match (parse (f x) rest)
                       [(consumed (ok stx rest message))
                        (ok (merge-syntax-box/srcloc stx srcloc) rest message)]
                       [(empty (ok (syntax-box datum _) rest message))
                        (merge-message/reply message (ok (syntax-box datum srcloc) rest message))]
                       [(consumed error) error]
                       [(empty error) (merge-message/reply message error)]))]
          [error error]))))])

(define (parser?* v)
  (or (parser? v) (pure? v)))

(define (pure/p x)
  (parser (λ (input) (empty (ok (syntax-box x empty-srcloc) input empty-message)))))
(define void/p (pure/p (void)))
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
               [boxed-output/c (struct/c syntax-box output/c srcloc?)]
               [reply/c (or/c error? (struct/c ok boxed-output/c seq/c message?))])
          (struct/c parser (-> any/c (or/c (struct/c consumed reply/c)
                                           (struct/c empty reply/c)))))))

(define (parse-syntax-box p input)
  (match (parse p input)
    [(or (consumed (ok x _ _))
         (empty    (ok x _ _)))
     (success x)]
    [(or (consumed (error message))
         (empty    (error message)))
     (failure message)]))

(define (parse-datum p input)
  (map syntax-box-datum (parse-syntax-box p input)))

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

          (define organized-expected (sort (remove-duplicates expected) string<=?))
          (unless (null? organized-expected)
            (newline)
            (display "  expected: ")
            (display (if (< (length organized-expected) 3)
                         (string-join organized-expected " or ")
                         (string-join organized-expected ", " #:before-last ", or ")))))))

(struct exn:fail:read:megaparsack exn:fail:read (unexpected expected) #:transparent)

(define/match (parse-result! result)
  [((success result)) result]
  [((failure (and message (message srcloc unexpected expected))))
   (raise (exn:fail:read:megaparsack (parse-error->string message)
                                     (current-continuation-marks)
                                     (list srcloc) unexpected expected))])

;; syntax object utilities
;; ---------------------------------------------------------------------------------------------------

(define some-original-syntax (read-syntax #f (open-input-string "()")))

(define/match* (syntax-box->syntax (syntax-box datum (srcloc name line col pos span)))
  (datum->syntax #f datum (list name line col pos span) some-original-syntax))

(define (make-syntax datum srcloc)
  (syntax-box->syntax (syntax-box datum srcloc)))

(define (merge-srclocs srcloc-a srcloc-b)
  (match (sort (list srcloc-a srcloc-b) < #:key (λ (x) (or (srcloc-position x) -1)))
    [(list (srcloc name-a line-a col-a pos-a span-a) (srcloc name-b line-b col-b pos-b span-b))
     (srcloc (or name-a name-b)
             (or line-a line-b)
             (or col-a col-b)
             (or pos-a pos-b)
             (cond [(and pos-a pos-b span-b) (+ (- pos-b pos-a) span-b)]
                   [(and pos-a span-a)       span-a]
                   [(and pos-b span-b)       span-b]
                   [else                     (or span-a span-b)]))]))

(define/match* (merge-syntax-box/srcloc (syntax-box datum srcloc-b) srcloc-a)
  (syntax-box datum (merge-srclocs srcloc-a srcloc-b)))

;; error message reconciliation
;; ---------------------------------------------------------------------------------------------------

(define/match* (merge-messages (message loc-a unexpected-a expected-a) (message loc-b unexpected-b expected-b))
  (message (merge-srclocs loc-a loc-b) (or unexpected-a unexpected-b) (append expected-a expected-b)))

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
     [(list (and stx (syntax-box (? proc) loc)) cs ...) (consumed (ok stx cs (message loc #f '())))]
     [(list (syntax-box c loc) _ ...)                   (empty (error (message loc c '())))]
     [_                                                 (empty (error (message empty-srcloc "end of input" '())))])))

;; termination (eof/p)
;; ---------------------------------------------------------------------------------------------------

(define eof/p
  (parser
   (match-lambda
     ['()                               (parse void/p '())]
     [(list (syntax-box c loc) _ ...)   (empty (error (message loc c '("end of input"))))])))

;; source location reification (syntax/p)
;; ---------------------------------------------------------------------------------------------------

(define (syntax/p p)
  (parser
   (λ (input)
     (match (parse p input)
       [(consumed (ok (syntax-box datum srcloc) rest message))
        (consumed (ok (syntax-box (make-syntax datum srcloc) srcloc) rest message))]
       [(empty (ok (syntax-box datum srcloc) rest message))
        (empty (ok (syntax-box (make-syntax datum srcloc) srcloc) rest message))]
       [error error]))))

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
