#lang racket/base

(require racket/require
         (multi-in data [functor applicative monad either])
         (prefix-in d: data/applicative)
         match-plus
         (multi-in racket [contract function generic list match port promise string])
         (prefix-in r: racket/base)
         (submod data/applicative coerce-delayed)
         (for-syntax racket/base)
         syntax/parse/define)

(provide lazy/p delay/p
         parser/c
         parser-parameter?
         make-parser-parameter
         parameterize/p
         (contract-out
          [struct syntax-box ([datum any/c] [srcloc srcloc?])]
          [struct message ([srcloc srcloc?] [unexpected any/c] [expected (listof string?)])]
          [rename parser?* parser? (any/c . -> . boolean?)]

          [rename parse-datum parse (parser?* (listof syntax-box?) . -> . (either/c message? any/c))]
          [parse-error->string (message? . -> . string?)]
          [parse-result! ((either/c message? any/c) . -> . any/c)]
          [struct (exn:fail:read:megaparsack exn:fail)
            ([message any/c] [continuation-marks any/c] [srclocs any/c]
             [unexpected any/c] [expected (listof string?)])]

          [void/p (parser/c any/c void?)]
          [or/p (parser?* parser?* ... . -> . parser?*)]
          [try/p (parser?* . -> . parser?*)]
          [noncommittal/p (parser?* . -> . parser?*)]
          [lookahead/p (parser?* . -> . parser?*)]
          [satisfy/p ((any/c . -> . any/c) . -> . parser?*)]
          [eof/p (parser/c any/c void?)]
          [syntax-box/p (parser?* . -> . (parser/c any/c syntax-box?))]
          [syntax/p (parser?* . -> . (parser/c any/c syntax?))]
          [label/p (string? parser?* . -> . parser?*)]
          [hidden/p (parser?* . -> . parser?*)]
          [fail/p (message? . -> . parser?*)]
          [guard/p (->* [parser?* (any/c . -> . any/c)]
                        [(or/c string? #f) (any/c . -> . any/c)]
                        parser?*)]))

;; supporting types
;; ---------------------------------------------------------------------------------------------------

(struct syntax-box (datum srcloc)
  #:transparent
  #:methods gen:functor
  [(define/match* (map f (syntax-box datum srcloc))
     (syntax-box (f datum) srcloc))])

(struct parser-input (position ; the number of tokens advanced past to get to this point
                      last-loc ; the srcloc of the previous token
                      tokens)  ; the remaining sequence of tokens, in syntax boxes
  #:transparent)

(struct consumed (reply) #:transparent)
(struct empty (reply) #:transparent)

(struct error (message) #:transparent)
(struct ok (result rest message) #:transparent)

; For backwards-compatibility reasons, the `message` struct cannot gain new fields. Therefore, we use
; `message*` internally and `message` externally.
(struct message (srcloc unexpected expected) #:transparent)
(struct message* (position   ; the number of tokens successfully parsed before this error was generated
                  user?      ; #t if generated by `fail/p`, otherwise #f
                  srcloc
                  unexpected
                  expected)
  #:transparent)

; Note that `ok` replies sometimes include an error message. Why? Because if a later parser fails
; without consuming input, previous branches might have failed in the same location, which could
; contribute to the error. For example, consider:
;
;   (do (or/p digit/p (pure #\0))
;       letter/p)
;
; Given the input "*", the `digit/p` parser will fail, and the second branch will be taken, which
; results in an `ok` response containing `#\0`. Next, the `letter/p` parser will fail, resulting in a
; parse error. If the `ok` response contains no error information, the best error we can produce is
;
;   string:1:1: parse error
;     unexpected: *
;     expected: letter
;
; but this is not good, because a digit is also legal at that position. So we need to take care to
; preserve error information from failed branches, even if we produce a successful result.

(define empty-srcloc (srcloc #f #f #f #f #f))

;; core primitives
;; ---------------------------------------------------------------------------------------------------

; A parser is a procedure that obeys the following contract:
;
;   (-> parser-input?
;       parser-parameterization?
;       (values (or/c consumed? empty?)
;               parser-parameterization?))
;
; The parser parameterization tracks user-defined parser state, and it is monadically threaded
; through each parser.

(struct parser (proc)
  #:methods gen:functor
  [(define/generic -map map)
   (define (map f p)
     (parser
      (λ (input paramz)
        (define-values [result paramz*] ((parser-proc p) input paramz))
        (values (match result
                  [(consumed (ok v rest message)) (consumed (ok (-map f v) rest message))]
                  [(empty (ok v rest message))    (empty (ok (-map f v) rest message))]
                  [error                          error])
                paramz*))))]

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
      (λ (input paramz)
        (define-values [result paramz*] ((parser-proc p) input paramz))
        (match result
          [(empty (ok (syntax-box x _) rest message))
           (define-values [result paramz**] (parse (f x) rest paramz*))
           (values (merge-message/result message result) paramz**)]
          [(consumed (ok (syntax-box x srcloc) rest message-a))
           (define-values [result* paramz**] (parse (f x) rest paramz*))
           (values (consumed (match result*
                               [(consumed (ok stx rest message-b))
                                (ok (merge-syntax-box/srcloc stx srcloc) rest (merge-messages message-a message-b))]
                               [(empty (ok (syntax-box datum _) rest message-b))
                                (ok (syntax-box datum srcloc) rest (merge-messages message-a message-b))]
                               [(consumed error) (merge-message/reply message-a error)]
                               [(empty error) (merge-message/reply message-a error)]))
                   paramz**)]
          [error (values error paramz*)]))))])

(define (parser?* v)
  (or (parser? v) (pure? v)))

(define (make-pure-result x input)
  (empty (ok (syntax-box x empty-srcloc) input #f)))
(define (make-stateless-parser proc)
  (parser (λ (input paramz) (values (proc input) paramz))))
(define (map-parser-result p proc)
  (parser
   (λ (input paramz)
     (define-values [result paramz*] (parse p input paramz))
     (values (proc result) paramz*))))

(define (pure/p x)
  (make-stateless-parser (λ (input) (values (make-pure-result x input)))))
(define void/p (pure/p (void)))
(define coerce-parser
  (coerce-pure void/p))

(define (parse p input paramz)
  ((parser-proc (coerce-parser p)) input paramz))

; these are used by `parser/c` to implement lazy input token contracts
(define parse-prompt-tag (make-continuation-prompt-tag 'parse))
(define mark:parser-token-ctc-proj (make-continuation-mark-key 'parser-token-ctc-proj))
(define (current-parser-token-ctc-proj)
  (continuation-mark-set-first #f
                               mark:parser-token-ctc-proj
                               (λ (val) val)
                               parse-prompt-tag))

(define (parse-syntax-box p input)
  (match-define-values [result _] (call-with-continuation-prompt
                                   (λ () (parse p input (hasheq)))
                                   parse-prompt-tag))
  (match result
    [(or (consumed (ok x _ _))
         (empty    (ok x _ _)))
     (success x)]
    [(or (consumed (error (message* _ _ srcloc unexpected expected)))
         (empty    (error (message* _ _ srcloc unexpected expected))))
     (failure (message srcloc unexpected expected))]))

(define (parse-datum p tokens)
  (map syntax-box-datum (parse-syntax-box p (parser-input 0 empty-srcloc tokens))))

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

(define (merge-messages message-a message-b)
  (match* {message-a message-b}
    [{_ #f} message-a]
    [{#f _} message-b]
    [{(message* pos-a user?-a loc-a unexpected-a expected-a)
      (message* pos-b user?-b loc-b unexpected-b expected-b)}
     (cond
       ; if the errors don’t occur at the same location, pick the one that made more progress
       [(> pos-a pos-b) message-a]
       [(< pos-a pos-b) message-b]
       ; if one was user-generated and the other wasn’t, pick the user-generated one
       [(and user?-a (not user?-b)) message-a]
       [(and user?-b (not user?-a)) message-b]
       ; otherwise, combine information from both errors
       [else
        (message* pos-a user?-a (merge-srclocs loc-a loc-b) (or unexpected-a unexpected-b) (append expected-a expected-b))])]))

(define (merge-message/reply message-a reply)
  (match reply
    [(ok x rest message-b) (ok x rest (merge-messages message-a message-b))]
    [(error message-b)     (error (merge-messages message-a message-b))]))

(define (merge-message/result message result)
  (match result
    [(empty reply) (empty (merge-message/reply message reply))]
    [(consumed reply) (consumed (merge-message/reply message reply))]))

;; laziness (lazy/p, delay/p)
;; ---------------------------------------------------------------------------------------------------

;; Not publicly documented, but provided for backwards compatibility.
;; Will likely be removed eventually in favor of `delay/p`.
(define-simple-macro (lazy/p p:expr)
  (parser (λ (input paramz) (parse p input paramz))))

(define-simple-macro (delay/p p)
  #:declare p (expr/c #'parser?*)
  (let ([pp (delay p.c)])
    (parser (λ (input paramz) (parse (force pp) input paramz)))))

;; choice (or/p)
;; ---------------------------------------------------------------------------------------------------

; binary version of or/p
(define (p . <or> . q)
  (parser
   (λ (input paramz)
     (define-values [result paramz*] (parse p input paramz))
     (match result
       [(empty (error message))
        (define-values [result paramz*] (parse q input paramz))
        (values (merge-message/result message result) paramz*)]
       [other (values other paramz*)]))))

(define (or/p . ps)
  (let-values ([(ps p) (split-at ps (sub1 (length ps)))])
    (foldr <or> (first p) ps)))

;; lookahead (try/p, noncommittal/p, lookahead/p)
;; ---------------------------------------------------------------------------------------------------

(define (try/p p)
  (map-parser-result
   p (match-lambda
       [(consumed (error message)) (empty (error message))]
       [other                      other])))

(define (noncommittal/p p)
  (map-parser-result
   p (match-lambda
       [(consumed (? ok? reply)) (empty reply)]
       [other                    other])))

(define (lookahead/p p)
  (parser
   (λ (input paramz)
     (define-values [result paramz*] (parse p input paramz))
     (values (match result
               [(consumed (ok x _ _)) (empty (ok x input #f))]
               [(empty    (ok x _ _)) (empty (ok x input #f))]
               [error                 error])
             paramz*))))

;; conditional (satisfy/p)
;; ---------------------------------------------------------------------------------------------------

(define (satisfy/p proc)
  (make-stateless-parser
   (match-lambda
     [(parser-input pos last-loc tokens)
      (match tokens
        [(cons (syntax-box (app (current-parser-token-ctc-proj) c) loc) cs)
         (if (proc c)
             (consumed (ok (syntax-box c loc) (parser-input (add1 pos) loc cs) #f))
             (empty (error (message* pos #f loc c '()))))]
        ['()
         (empty (error (message* pos #f last-loc "end of input" '())))])])))

;; termination (eof/p)
;; ---------------------------------------------------------------------------------------------------

(define eof/p
  (make-stateless-parser
   (match-lambda
     [(parser-input pos _ tokens)
      (match tokens
        ['()
         (make-pure-result (void) '())]
        [(cons (syntax-box (app (current-parser-token-ctc-proj) c) loc) _)
         (empty (error (message* pos #f loc c '("end of input"))))])])))

;; source location reification (syntax-box/p & syntax/p)
;; ---------------------------------------------------------------------------------------------------

(define (syntax-box/p p)
  (map-parser-result
   p (match-lambda
       [(consumed (ok {and box (syntax-box _ srcloc)} rest message))
        (consumed (ok (syntax-box box srcloc) rest message))]
       [(empty (ok {and box (syntax-box _ srcloc)} rest message))
        (empty (ok (syntax-box box srcloc) rest message))]
       [error error])))

(define (syntax/p p)
  (map syntax-box->syntax (syntax-box/p p)))

;; parser annotation (label/p & hidden/p)
;; ---------------------------------------------------------------------------------------------------

(define/match* (expect pos-a expected (and message (message* pos-b user? srcloc unexpected _)))
  (if (= pos-a pos-b)
      (message* pos-a user? srcloc unexpected (list expected))
      message))

(define/match* (hide pos-a (and message (message* pos-b user? srcloc unexpected _)))
  (if (= pos-a pos-b)
      (message* pos-a user? srcloc unexpected '())
      message))

(define (label/p str p)
  (parser
   (λ (input paramz)
     (define pos (parser-input-position input))
     (define-values [result paramz*] (parse p input paramz))
     (values (match result
               [(empty (error message))     (empty (error (expect pos str message)))]
               [(empty (ok x rest message)) (empty (ok x rest (expect pos str message)))]
               [consumed                    consumed])
             paramz*))))

(define (hidden/p p)
  (parser
   (λ (input paramz)
     (define pos (parser-input-position input))
     (define-values [result paramz*] (parse p input paramz))
     (values (match result
               [(empty (error message))     (empty (error (hide pos message)))]
               [(empty (ok x rest message)) (empty (ok x rest (hide pos message)))]
               [consumed                    consumed])
             paramz*))))

;; custom failure messages (fail/p, guard/p)
;; ---------------------------------------------------------------------------------------------------

(define/match* (fail/p (message srcloc unexpected expected))
  (make-stateless-parser
   (λ (input) (empty (error (message* (parser-input-position input) #t srcloc unexpected expected))))))

; Providing guard/p as a primitive rather than as a derived concept is somewhat unsatisfying. In
; previous versions of this library, it /was/ implemented as a derived combinator, with the following
; implementation:
;
;   (define (guard/p p pred? [expected #f] [mk-unexpected values])
;     (do [s <- (syntax-box/p p)]
;         (define v (syntax-box-datum s))
;         (if (pred? v)
;             (pure v)
;             (fail/p (message (syntax-box-srcloc s)
;                              (mk-unexpected v)
;                              (if expected (list expected) '()))))))
;
; Sadly, changes to the way `label/p` chooses to adjust the list of expected values makes this
; implementation no longer satisfactory. The issue is that the token position of the error produced by
; `fail/p` is after the tokens consumed by the enclosed parser. This causes `label/p` to consider the
; parse error as more specific than the one it would generate, so it declines to modify it.
;
; This really reveals a deeper issue with the way megaparsack answers the question of which errors
; made the most progress. If an error from a different branch were to occur in the middle of the token
; stream consumed by the enclosed parser, megaparsack would select it over the `guard/p`-generated
; error, since `guard/p` reports its error at the /start/ of that sequence of tokens. This is not
; really right---a more correct approach would be to record the whole span of tokens that `guard/p`
; wraps. But it’s unclear how much this really matters, and that still wouldn’t make it any easier to
; define `guard/p` as a derived concept, so for now, the imperfect implementation remains.

(define (guard/p p pred? [expected #f] [mk-unexpected values])
  (define (guard-reply pos reply)
    (match reply
      [(ok (syntax-box x loc) rest _)
       #:when (not (pred? x))
       (error (message* pos #t loc (mk-unexpected x) (if expected (list expected) '())))]
      [other other]))
  (parser
   (λ (input paramz)
     (define pos (parser-input-position input))
     (define-values [result paramz*] (parse p input paramz))
     (values (match result
               [(empty reply) (empty (guard-reply pos reply))]
               [(consumed reply) (consumed (guard-reply pos reply))])
             paramz*))))

;; contracts (parser/c)
;; ---------------------------------------------------------------------------------------------------

(define-syntax-parser parser/c
  [(head in-ctc out-ctc)
   (define ctc-tag (gensym 'ctc))
   (syntax-property
    (quasisyntax/loc this-syntax
      (parser/c-proc
       #,(syntax-property #'in-ctc 'racket/contract:negative-position ctc-tag)
       #,(syntax-property #'out-ctc 'racket/contract:positive-position ctc-tag)))
    'racket/contract:contract
    (vector ctc-tag (list #'head) '()))]
  [(head form ...)
   (syntax-property (quasisyntax/loc this-syntax
                      (parser/c-proc form ...))
                    'racket/contract:contract
                    (vector (gensym 'ctc) (list #'head) '()))]
  [head:id
   (syntax-property (quasisyntax/loc this-syntax
                      parser/c-proc)
                    'racket/contract:contract
                    (vector (gensym 'ctc) (list #'head) '()))])

; We define `parser/c` as a custom contract, which allows us to ensure that contracts on input tokens
; are applied /lazily/, when the tokens are actually consumed. If we didn’t do this, each individual
; parser would apply its input contract to every remaining token in the input stream, which is both
; unhelpful and extraordinarily slow.
;
; To keep contract checking minimal, `parser/c` does not directly enforce input contracts. Instead, it
; stores a contract projection in the `mark:parser-token-ctc-proj` continuation mark. The contract
; projection is only actually applied to input tokens as needed by primitive parser constructors like
; `satisfy/p`.
;
; This is all rather subtle, since it means all primitive parsers must individually take care to apply
; the contract projection before inspecting the token stream. Fortunately, the only parsers that
; actually inspect tokens directly are `satisfy/p` and `eof/p`, so the contract checking machinery
; can be localized there.

(define (parser/c-proc in-ctc out-ctc)
  (let ([in-ctc (coerce-contract 'parser/c in-ctc)]
        [out-ctc (coerce-contract 'parser/c out-ctc)])
    (define in-proj (contract-late-neg-projection in-ctc))
    (define out-proj (contract-late-neg-projection out-ctc))
    (define pure-out-proj (contract-late-neg-projection (pure/c out-ctc)))

    (define chaperone? (and (chaperone-contract? in-ctc)
                            (chaperone-contract? out-ctc)))
    ((if chaperone? make-chaperone-contract make-contract)
     #:name (build-compound-type-name 'parser/c in-ctc out-ctc)
     #:first-order parser?*
     #:late-neg-projection
     (λ (blame)
       (define in-elem-proj (in-proj (blame-add-context blame "the input to" #:swap? #t)))
       (define out-blame (blame-add-context blame "the result of"))
       (define out-elem-proj (out-proj out-blame))
       (define pure-out-elem-proj (pure-out-proj out-blame))
       (λ (val missing-party)
         (cond
           [(pure? val)
            (pure-out-elem-proj val missing-party)]
           [(parser? val)
            ((if chaperone? chaperone-struct impersonate-struct)
             val parser-proc
             (λ (self parse-proc)
               (define (wrap-ok x loc rest message)
                 (ok (syntax-box (out-elem-proj x missing-party) loc) rest message))
               ((if chaperone? chaperone-procedure impersonate-procedure)
                parse-proc
                (λ (input paramz)
                  (define old-ctc-proj (current-parser-token-ctc-proj))
                  (values (λ (result paramz)
                            (values (match result
                                      [(consumed (ok (syntax-box x loc) rest message))
                                       (consumed (wrap-ok x loc rest message))]
                                      [(empty (ok (syntax-box x loc) rest message))
                                       (empty (wrap-ok x loc rest message))]
                                      [error error])
                                    paramz))
                          'mark mark:parser-token-ctc-proj
                          (λ (val) (in-elem-proj (old-ctc-proj val) missing-party))
                          input paramz)))))]
           [else
            (raise-blame-error blame val #:missing-party missing-party
                               '(expected: "parser?" given: "~e") val)]))))))

;; state (make-parser-parameter, parameterize/p)
;; ---------------------------------------------------------------------------------------------------

(struct parser-parameter (initial-value)
  #:constructor-name make-parser-parameter
  #:authentic ; we look up parameters in the parameterization by `eq?`, so no contracts allowed
  #:property prop:procedure
  (case-lambda
    [(self)
     (parser
      (λ (input paramz)
        (define value (hash-ref paramz self (λ () (parser-parameter-initial-value self))))
        (values (make-pure-result value input) paramz)))]
    [(self value)
     (parser
      (λ (input paramz)
        (values (make-pure-result (void) input)
                (hash-set paramz self value))))]))

(define-syntax-parser parameterize/p
  [(_ ([param val:expr] ...) p)
   #:declare param (expr/c #'parser-parameter? #:name "parameter")
   #:declare p (expr/c #'parser? #:name "body parser")
   (for/fold ([body #'p.c])
             ([param (in-list (reverse (attribute param.c)))]
              [val (in-list (reverse (attribute val)))])
     (quasisyntax/loc this-syntax
       (parameterize-one/p #,body #,param #,val)))])

(define (parameterize-one/p p param val)
  (do [old-val <- (param)]
      (param val)
      [result <- p]
      (param old-val)
      (pure result)))
