#lang scribble/manual

@(require "util.rkt")

@(define-parser-interaction syntax-interaction close-syntax!)

@title[#:tag "producing-syntax"]{Producing Syntax}

One of the properties of megaparsack is that it always tracks source locations, which is how it is
able to include source location information in error messages. This can be leveraged for an entirely
separate purpose, which is creating parsers that produce @reftech{syntax objects} as output. This
functionality is extremely useful when creating custom @hash-lang[] languages.

@section{Annotating parsers to produce syntax}

Megaparsack does not opt to produce syntax objects as the result of every parse because it would make
composing parsers extremely tedious. For example, if @racket[integer/p] produced syntax objects
containing integers instead of integers themselves, they would need to be unwrapped before they could
be added together or otherwise used as numbers. Instead, megaparsack requires that you opt-in to
syntax object production by using the @racket[syntax/p] combinator.

The @racket[syntax/p] combinator is “magic”—it takes @emph{any} parser and turns it into a parser that
produces a value containing accurate source location information. This is because @racket[syntax/p]
takes advantage of the internal parser state to track information that is otherwise not accessible
to parsers. Fortunately, this makes the interface extremely simple to use—just wrap an ordinary parser
with @racket[syntax/p] and use it as usual:

@(parser-interaction
  (parse-string (syntax/p integer/p) "42"))

The produced syntax objects automatically keep track of all the relevant syntax properties, including
line, column, position, and span:

@(parser-interaction
  (define stx (parse-result! (parse-string (syntax/p integer/p) "42")))
  (syntax-line stx)
  (syntax-column stx)
  (syntax-span stx))

This syntax tracking is not specific to the built-in parsers, and you do not need to do anything
special to use it with your custom parsers. For example, consider a relatively complex parser that
parses a list of comma-delimited numbers surrounded by brackets:

@(syntax-interaction
  (define integer-list/p
    (do (char/p #\[)
        [ints <- (many/sep*/p (syntax/p integer/p)
                              (char/p #\,))]
        (char/p #\])
        (pure ints))))

We’ve annotated the @racket[integer/p] parser with @racket[syntax/p] once again so we can get location
tracking for each individual list element, but we’ll also annotate the whole thing with
@racket[syntax/p] so we can track information about the entire list as well:

@(syntax-interaction
  (define integer-list-stx
    (parse-result! (parse-string (syntax/p integer-list/p) "[1,2,3,5,8,13]")))
  integer-list-stx
  (syntax-span integer-list-stx))

As expected, the top-level syntax object spans the entire input, including the brackets. We can also
get information about the individual elements, since they are syntax objects as well:

@(syntax-interaction
  (syntax->list integer-list-stx))

This makes writing a reader for a @hash-lang[] relatively straightforward because source location
information is already encoded into a set of syntax objects which can be used as the source of a
Racket module.

@section{Parsing tokens from @racketmodname[parser-tools/lex]}

While @racket[syntax/p] can be used with any megaparsack parser, it is sometimes useful to be able to
perform a lexing phase before parsing to handle things like ignoring whitespace and tokenization in a
separate pass. Currently, megaparsack does not include tools of its own specifically for lexing
(though it would be perfectly possible to use the output of a separate simple parser as the input to
another parser), but it does provide a function to interoperate with @racketmodname[parser-tools/lex],
another Racket library that provides utilities designed specifically for lexing.

When using @racketmodname[parser-tools/lex], make sure to use the @racket[lexer-src-pos] form, which
enables the lexer’s own source location tracking. This configures the lexer to produce
@racket[position-token] values as output, which can be fed to @racket[parse-tokens] from
@racketmodname[megaparsack/parser-tools/lex] to parse with any megaparsack parser.

Parsers that operate on strings, like @racket[char/p] and @racket[integer/p], will not work with
tokens from @racketmodname[parser-tools/lex] because tokens can contain arbitrary data. Instead,
use the @racket[token/p] function to create parsers that handle particular tokens.

Here is a very simple lexer that produces lexemes for identifiers, numbers, parentheses, and commas:

@(syntax-interaction
  (define-tokens simple [IDENTIFIER NUMBER])
  (define-empty-tokens simple* [OPEN-PAREN CLOSE-PAREN COMMA])
  (define simple-lexer
    (lexer-src-pos
     [#\( (token-OPEN-PAREN)]
     [#\) (token-CLOSE-PAREN)]
     [#\, (token-COMMA)]
     [(:+ (:or (:/ #\a #\z) (:/ #\A #\Z)))
      (token-IDENTIFIER (string->symbol lexeme))]
     [(:+ (:/ #\0 #\9))
      (token-NUMBER (string->number lexeme))]
     [(:or whitespace blank iso-control) (void)]
     [(eof) eof])))

We can write a simple helper function to lex a string into a list of tokens, making sure to call
@racket[port-count-lines!] to enable source location tracking:

@(syntax-interaction
  (define (lex-simple str)
    (define in (open-input-string str))
    (port-count-lines! in)
    (let loop ([v (simple-lexer in)])
      (cond [(void? (position-token-token v)) (loop (simple-lexer in))]
            [(eof-object? (position-token-token v)) '()]
            [else (cons v (loop (simple-lexer in)))])))
  (lex-simple "f(1, g(3, 4))"))

Next, we can write a trivial parser to actually parse these tokens. Since we’ve written a lexer, most
of the heavy lifting is already done, and we can just focus on assigning semantics:

@(syntax-interaction
  (code:comment @#,elem{some wrappers around tokens that use @racket[syntax/p]})
  (define number/p (syntax/p (token/p 'NUMBER)))
  (define identifier/p (syntax/p (token/p 'IDENTIFIER)))
  (code:comment @#,elem{a simple function invokation})
  (define funcall/p
    (syntax/p
     (do [func <- identifier/p]
         (token/p 'OPEN-PAREN)
         [args <- (many/sep*/p expression/p (token/p 'COMMA))]
         (token/p 'CLOSE-PAREN)
         (pure (list* func args)))))
  (code:comment @#,elem{an expression can be a number or a function invokation})
  (define expression/p
    (or/p number/p
          funcall/p)))

Now, with our simple parser in place, we can actually parse arbitrary C-style function calls into
S-expressions:

@(syntax-interaction
  (define expr-stx
    (parse-result! (parse-tokens expression/p (lex-simple "f(1, g(3, 4))"))))
  expr-stx)

As expected, the source locations for each datum will automatically be assigned to the resulting
syntax object due to the use of @racket[syntax/p] on the base datums and around @racket[funcall/p]:

@(syntax-interaction
  (syntax->list expr-stx))

In just a couple dozens lines of code, we’ve managed to implement a fairly robust parser that produces
syntax objects ready to be handed off to Racket as the result of parsing a module body, which can be
compiled into working Racket code.

@(close-syntax!)
