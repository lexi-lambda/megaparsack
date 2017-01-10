#lang scribble/manual

@(require "util.rkt")

@title[#:tag "reference"]{API Reference}

A @tech{parser} is a value that represents a method of turning a syntax object or sequence of
syntax objects an arbitrary Racket value. Parsers can be created using various primitives, then
sequenced together using parser combinators to create larger parsers.

Parsers are @functech{functors}, @functech{applicative functors}, and @functech{monads}, which allows
them to be mapped over and sequenced together using the corresponding generic interfaces.

@section[#:tag "primitives"]{Primitives}

@defproc[(parser? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a @tech{parser}, otherwise returns @racket[#f].}

@defproc[(parser/c [in-ctc contract?] [out-ctc contract?]) contract?]{
Produces a @reftech{contract} that describes a @tech{parser} that consumes values described by
@racket[in-ctc] and produces values described by @racket[out-ctc].}

@defproc[(parse [parser parser?] [boxes (listof syntax-box?)]) (either/c message? any/c)]{
Runs @racket[parser] on @racket[boxes] and returns either the result of a successful parse or a value
that includes information about the parse failure.}

@defproc[(parse-error->string [message message?]) string?]{
Converts a parse error to a human-readable string. This is used by @racket[parse-result!] to format
the message used in the exception, but it can also be used if you want to display the error in other
ways.}

@defproc[(parse-result! [result (either/c message? any/c)]) any/c]{
Extracts a successful parse value from @racket[result]. If @racket[result] is a failure, raises
@racket[exn:fail:read:megaparsack] with the failure message converted to a string using
@racket[parse-error->string].}

@defstruct*[(exn:fail:read:megaparsack exn:fail:read) ([unexpected any/c]
                                                       [expected (listof string?)])
            #:transparent]{
Raised by @racket[parse-result!] when given a parse failure.}

@defstruct*[syntax-box ([value any/c] [srcloc srcloc?]) #:transparent]{
Represents a single parsable entity. Just like @reftech{syntax objects}, a @deftech{syntax box}
associates some source location information with an arbitrary datum. However, unlike ordinary syntax
objects, values like lists and vectors can be stored in a syntax box without being recursively wrapped
in additional layers of syntax objects.

The @racket[value] can be anything at all, but usually it is either a character or some token produced
as the result of lexing. It is unlikely that you will need to create @racket[syntax-box] values
yourself; rather, use higher-level functions like @racket[parse-string] that create these values for
you.}

@defstruct*[message ([srcloc srcloc?] [unexpected any/c] [expected (listof string?)]) #:transparent]{
Represents a parse error. Generally you will not need to construct or use these yourself, since they
will be automatically constructed when parsers fail, and you can convert them to a human-readable
error message using @racket[parse-error->string]. For more complicated use cases, though, you may want
to raise custom parse errors using @racket[fail/p] or format your own error messages, so you can use
this structure directly.}

@defthing[void/p (parser/c any/c void?)]{
A parser that always succeeds and always returns @|void-const|.}

@defproc[(or/p [parser parser?] ...+) parser?]{
Tries each @racket[parser] in succession until one consumes input, at which point its result will be
returned as the result of the overall parse. Parsers that are successful but do @emph{not} consume
input will not prevent successive parsers from being tried, and parsers that consume input but fail
will halt further parsers from being tried and will simply return an error.

If no parsers consume input, then the first successful empty parser is used instead. If all parsers
fail, the result will be a failure that combines failure information from each parser attempted.}

@defproc[(try/p [parser parser?]) parser?]{
Creates a new parser like @racket[parser], except that it does not consume input if @racket[parser]
fails. This allows the parser to backtrack and try other alternatives when used inside a
@racket[or/p] combinator.}

@defproc[(satisfy/p [proc (any/c . -> . any/c)]) parser?]{
Creates a parser that checks if @racket[proc] produces a non-@racket[#f] value when applied to a
single datum. If so, it consumes the datum and returns successfully; otherwise, it fails without
consuming input.}

@defthing[eof/p (parser/c any/c void?)]{
A parser that only succeeds when there is no more input left to consume. It always returns
@|void-const|.}

@defproc[(label/p [label string?] [parser parser?]) parser?]{
Creates a parser like @racket[parser], except that failures are reported in terms of @racket[label]
instead of whatever names would have been used instead.}

@defproc[(hidden/p [parser parser?]) parser?]{
Like @racket[label/p], adjusts how failures are reported for @racket[parser], but @racket[hidden/p]
completely hides any failure information produced by @racket[parser] when reporting errors. (This is
useful when parsing things like whitespace which are usually not useful to include in error
messages.)}

@defproc[(syntax/p [parser parser?]) (parser/c any/c syntax?)]{
Produces a parser like @racket[parser], except that its result is wrapped in a @reftech{syntax object}
that automatically incorporates source location information from the input stream. This allows parsers
to add a sort of automated source location tracking to their output.

The @racket[syntax/p] combinator makes source location wrapping opt-in, which is desirable since it is
often useful to return values from combinators that are intermediate values not intended to be wrapped
in syntax (for example, @racket[many*/p] returns a list of results, not a syntax list).}

@defproc[(syntax-box/p [parser parser?]) (parser/c any/c syntax-box?)]{
Like @racket[syntax/p], but wraps the result in a @racket[syntax-box] instead of a @reftech{syntax
object}. This is useful if you want to get the source location information from a parse result, but
you want to ensure the underlying datum remains untouched.}

@defproc[(fail/p [msg message?]) (parser/c any/c none/c)]{
Produces a parser that always fails and produces @racket[msg] as the error message. This is the
lowest-level way to report errors, but many cases in which you would want to raise a custom failure
message can be replaced with @racket[guard/p] instead, which is slightly higher level.}

@defproc[(many*/p [parser parser?]) (parser/c list?)]{
Produces a parser that attempts @racket[parser] zero or more times and returns a list of the results.}

@defproc[(many+/p [parser parser?]) parser?]{
Produces a parser that attempts @racket[parser] one or more times and returns a list of the results.}

@defproc[(repeat/p [n exact-nonnegative-integer?] [parser parser?]) (parser/c any/c list?)]{
Produces a parser that attempts @racket[parser] @emph{exactly} @racket[n] times and returns a list of
the results.}

@defproc[(==/p [v any/c] [=? (any/c any/c . -> . any/c) equal?]) parser?]{
Produces a parser that succeeds when a single datum is equal to @racket[v], as determined by
@racket[=?]. Like @racket[satisfy/p], it consumes a single datum upon success but does not consume
anything upon failure.}

@defproc[(many/sep*/p [parser parser?] [sep parser?]) parser?]{
Produces a parser that attempts @racket[parser] zero or more times, each parse separated by
@racket[sep]. It returns a list of successful @racket[parser] parses but discards the results of each
successful @racket[sep] parse.}

@defproc[(many/sep+/p [parser parser?] [sep parser?]) parser?]{
Produces a parser that attempts @racket[parser] one or more times, each parse separated by
@racket[sep]. It returns a list of successful @racket[parser] parses but discards the results of each
successful @racket[sep] parse.}

@defproc[(guard/p [parser parser?] [pred? (any/c . -> . any/c)]
                  [expected (or/c string? #f) #f] [make-unexpected (any/c . -> . any/c) identity])
         parser?]{
Produces a parser that runs @racket[parser], then applies a guard predicate @racket[pred?] to the
result. If the result of @racket[pred?] is @racket[#f], then the parser fails, otherwise the parser
succeeds and produces the same result as @racket[parser].

If the parser fails and @racket[expected] is a string, then @racket[expected] is used to add
expected information to the parser error. Additionally, the @racket[make-unexpected] function is
applied to the result of @racket[parser] to produce the @racket[unexpected] field of the parse error.

@(parser-examples
  (define small-integer/p
    (guard/p integer/p (λ (x) (<= x 100))
             "integer in range [0,100]"))
  (eval:check (parse-result! (parse-string small-integer/p "42")) 42)
  (eval:error (parse-result! (parse-string small-integer/p "300"))))}

@defproc[(list/p [parser parser?] ... [#:separator sep parser? void/p]) (parser/c any? list?)]{
 Returns a @tech{parser} that runs each @racket[parser] in sequence separated by @racket[sep] and
 produces a list containing the results of each @racket[parser]. The results of @racket[sep] are
 ignored.

 @(parser-examples
   (define dotted-let-digit-let/p
     (list/p letter/p digit/p letter/p #:separator (char/p #\.)))
   (eval:check (parse-result! (parse-string dotted-let-digit-let/p "a.1.b")) (list #\a #\1 #\b))
   (eval:error (parse-result! (parse-string dotted-let-digit-let/p "a1c")))
   (eval:error (parse-result! (parse-string dotted-let-digit-let/p "a.1"))))

 Using a separator parser that consumes no input (such as the default separator, @racket[void/p]) is
 equivalent to not using a separator at all.

 @(parser-examples
   (define let-digit-let/p (list/p letter/p digit/p letter/p))
   (eval:check (parse-result! (parse-string let-digit-let/p "a1b")) (list #\a #\1 #\b)))}

@section[#:tag "parsing-text"]{Parsing Text}

@defmodule[megaparsack/text]

@defproc[(parse-string [parser (parser/c char? any/c)] [str string?] [src-name any/c 'string])
         (either/c message? any/c)]{
Parses @racket[str] using @racket[parser], which must consume character datums. The value provided for
@racket[src-name] is used in error reporting when displaying source location information.}

@defproc[(parse-syntax-string [parser (parser/c char? any/c)] [stx-str (syntax/c string?)])
         (either/c message? any/c)]{
Like @racket[parse-string], but uses the source location information from @racket[stx-str] to
initialize the source location tracking. The result of @racket[(syntax-source stx-str)] is used in
place of the @racket[_src-name] argument.}

@defproc[(char/p [c char?]) (parser/c char? char?)]{
Parses a single datum that is equal to @racket[c].}

@defproc[(char-ci/p [c char?]) (parser/c char? char?)]{
Parses a single datum that is case-insensitively equal to @racket[c], as determined by
@racket[char-ci=?].}

@defthing[letter/p (parser/c char? char?)]{
Parses an alphabetic letter, as determined by @racket[char-alphabetic?].}

@defthing[digit/p (parser/c char? char?)]{
Parses a single digit, as determined by @racket[char-numeric?].}

@defthing[symbolic/p (parser/c char? char?)]{
Parses a symbolic character, as determined by @racket[char-symbolic?].}

@defthing[space/p (parser/c char? char?)]{
Parses a single whitespace character, as determined by @racket[char-whitespace?] or
@racket[char-blank?].}

@defthing[integer/p (parser/c char? integer?)]{
Parses a sequence of digits as an integer. Does not handle negative numbers or numeric separators.}

@defproc[(string/p [str string?]) (parser/c char? string?)]{
Parses a sequence of characters equal to @racket[str] and returns @racket[str] as its result.}

@section[#:tag "parsing-with-parser-tools"]{Parsing with @racketmodname[parser-tools/lex]}

@defmodule[megaparsack/parser-tools/lex]

Sometimes it is useful to run a lexing pass over an input stream before parsing, in which case
@racketmodname[megaparsack/text] is not appropriate. The @tt{parser-tools} package provides the
@racketmodname[parser-tools/lex] library, which implements a lexer that produces tokens.

When using @racketmodname[parser-tools/lex], use @racket[lexer-src-pos] instead of @racket[lexer] to
enable the built-in source location tracking. This will produce a sequence of @racket[position-token]
elements, which can then be passed to @racket[parse-tokens] and detected with @racket[token/p].

@defproc[(parse-tokens [parser parser?] [tokens (listof position-token?)] [source-name any/c 'tokens])
         syntax?]{
Parses a stream of tokens, @racket[tokens], produced from @racket[lexer-src-pos] from
@racketmodname[parser-tools/lex].}

@defproc[(token/p [name symbol?]) (parser/c (or/c symbol? token?) any/c)]{
Produces a parser that expects a single token with @racket[name], as produced by @racket[token-name].}
