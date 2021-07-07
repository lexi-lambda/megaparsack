#lang racket/base

(require data/applicative
         data/monad
         megaparsack
         megaparsack/text
         racket/list
         racket/contract
         rackunit
         rackunit/spec)

(describe "parser/c"
  (it "signals a contract violation on invalid input tokens"
    (define exn (with-handlers ([exn:fail? values])
                  (parse (char/p #\a) (list (syntax-box #f (srcloc #f #f #f #f #f))))))
    (check-pred exn:fail:contract:blame? exn)
    (define blame (exn:fail:contract:blame-object exn))
    (check-equal? (blame-contract blame)
                  '(-> char? (parser/c char? char?)))
    (check-pred blame-swapped? blame)
    (check-equal? (first (blame-context blame))
                  "the input to"))

  (it "signals a contract violation on invalid results"
    (define/contract foo/p
      (parser/c any/c string?)
      (do void/p (pure #f)))
    (define exn (with-handlers ([exn:fail? values])
                  (parse foo/p '())))
    (check-pred exn:fail:contract:blame? exn)
    (define blame (exn:fail:contract:blame-object exn))
    (check-equal? (blame-contract blame)
                  '(parser/c any/c string?))
    (check-pred blame-original? blame)
    (check-equal? (first (blame-context blame))
                  "the result of")))
