#lang racket/base

(require data/either
         megaparsack
         megaparsack/text
         rackunit
         rackunit/spec)

(describe "parse-syntax-string"
  (it "uses source location information from the provided syntax object"
    (let* ([stx #'"hello, world!"]
           [result (parse-result! (parse-syntax-string (syntax/p (string/p "hello")) stx))])
      (check-equal? (syntax-source stx) (syntax-source result))
      (check-equal? (syntax-line stx) (syntax-line result))
      (check-equal? (syntax-column stx) (syntax-column result))
      (check-equal? (syntax-position stx) (syntax-position result))
      (check-equal? (syntax-span result) 5))))

(describe "integer/p"
  (it "returns matched integer"
    (check-equal? (parse-string integer/p "123abc") (success 123))))

(describe "string/p"
  (it "returns matched string"
    (check-equal? (parse-string (string/p "abc") "abcde") (success "abc"))))

(describe "string-ci/p"
  (it "returns case-insensitive matched string"
    (check-equal? (parse-string (string-ci/p "aBc") "abCde") (success "abC"))))

(describe "char-between/p"
  (it "parses a single character"
    (check-equal? (parse-string (char-between/p #\b #\f) "bcdef") (success #\b)))
  (it "parses only characters between the given bounds"
    (check-equal? (parse-string (char-between/p #\b #\f) "a")
                  (failure (message (srcloc 'string 1 0 1 1) #\a
                                    '("a character between 'b' and 'f'"))))
    (check-equal? (parse-string (char-between/p #\b #\f) "g")
                  (failure (message (srcloc 'string 1 0 1 1) #\g
                                    '("a character between 'b' and 'f'"))))))

(describe "char-in/p"
  (it "parses a single character"
    (check-equal? (parse-string (char-in/p "aeiou") "a") (success #\a)))
  (it "parses only characters in the given string"
    (check-equal? (parse-string (char-in/p "aeiou") "b")
                  (failure (message (srcloc 'string 1 0 1 1) #\b
                                    '("'a'" "'e'" "'i'" "'o'" "'u'"))))))

(describe "char-not-in/p"
  (it "parses a single character"
    (check-equal? (parse-string (char-not-in/p "aeiou") "b") (success #\b)))
  (it "parses only characters not in the given string"
    (check-equal? (parse-string (char-not-in/p "aeiou") "a")
                  (failure (message (srcloc 'string 1 0 1 1) #\a
                                    '())))))
