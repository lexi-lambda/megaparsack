#lang racket/base

(require data/monad
         data/applicative
         megaparsack/base
         (prefix-in lex: parser-tools/lex)
         racket/contract
         racket/match)

(provide (contract-out [parse-tokens ([parser? (listof lex:position-token?)] [any/c] . ->* . any/c)]
                       [token/p (symbol? . -> . parser?)]))

(define (parse-tokens parser toks [srcname 'tokens])
  (parse parser (map (position-token->parser-token srcname) toks)))

(define ((position-token->parser-token source-name) pos-token)
  (match pos-token
    [(lex:position-token tok (lex:position offset-a line col) (lex:position offset-b _ _))
     (syntax-box tok (srcloc source-name line col offset-a (- offset-b offset-a)))]))

(define (token/p name)
  (label/p
   (symbol->string name)
   (do [tok <- (satisfy/p (λ (x) (and (or (lex:token? x) (symbol? x))
                                      (equal? (lex:token-name x) name))))]
       (pure (lex:token-value tok)))))
