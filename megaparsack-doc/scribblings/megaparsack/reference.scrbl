#lang scribble/manual

@(require (for-label data/either
                     megaparsack
                     megaparsack/text
                     racket/base
                     racket/contract)
          "util.rkt")

@title[#:tag "reference"]{API Reference}

A @deftech{parser} is a value that represents a method of turning a syntax object or sequence of
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

@defproc[(parse [parser parser?] [stxs (listof syntax?)]) (either/c message? any/c)]{
Runs @racket[parser] on @racket[stxs] and returns either the result of a successful parse or a value
that includes information about the parse failure.}

@defproc[(parse-error->string [message message?]) string?]{
Converts a parse error to a human-readable string. This is used by @racket[parse-result!] to format
the message used in the exception, but it can also be used if you want to display the error in other
ways.}

@defproc[(parse-result! [result (either/c message? any/c)]) any/c]{
Extracts a successful parse value from @racket[result]. If @racket[result] is a failure, raises
@racket[exn:fail:megaparsack] with the failure message converted to a string using
@racket[parse-error->string].}

@defstruct*[(exn:fail:megaparsack exn:fail) ([srcloc srcloc?]
                                             [unexpected any/c]
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

@defproc[(label/p [label string?] [parser parser?]) parser?]{
Creates a parser like @racket[parser], except that failures are reported in terms of @racket[label]
instead of whatever names would have been used instead.}

@defproc[(hidden/p [parser parser?]) parser?]{
Like @racket[label/p], adjusts how failures are reported for @racket[parser], but @racket[hidden/p]
completely hides any failure information produced by @racket[parser] when reporting errors. (This is
useful when parsing things like whitespace which are usually not useful to include in error
messages.)}

@defproc[(many/p [parser parser?]) (parser/c list?)]{
Produces a parser that attempts @racket[parser] zero or more times and returns a list of the results.}

@defproc[(some/p [parser parser?]) parser?]{
Produces a parser that attempts @racket[parser] one or more times and returns a list of the results.}

@defproc[(repeat/p [n exact-nonnegative-integer?] [parser parser?]) (parser/c any/c list?)]{
Produces a parser that attempts @racket[parser] @emph{exactly} @racket[n] times and returns a list of
the results.}

@defproc[(==/p [v any/c] [=? (any/c any/c . -> . any/c) equal?]) parser?]{
Produces a parser that succeeds when a single datum is equal to @racket[v], as determined by
@racket[=?]. Like @racket[satisfy/p], it consumes a single datum upon success but does not consume
anything upon failure.}

@defproc[(many/sep/p [parser parser?] [sep parser?]) parser?]{
Produces a parser that attempts @racket[parser] zero or more times, each parse separated by
@racket[sep]. It returns a list of successful @racket[parser] parses but discards the results of each
successful @racket[sep] parse.}

@defproc[(some/sep/p [parser parser?] [sep parser?]) parser?]{
Produces a parser that attempts @racket[parser] one or more times, each parse separated by
@racket[sep]. It returns a list of successful @racket[parser] parses but discards the results of each
successful @racket[sep] parse.}

@section[#:tag "parsing-text"]{Parsing Text}

@defmodule[megaparsack/text]

@defproc[(parse-string [parser (parser/c char? any/c)] [str string?] [src-name any/c 'string])
         (either/c message? any/c)]{
Parses @racket[str] using @racket[parser], which must consume character datums. The value provided for
@racket[src-name] is used in error reporting when displaying source location information.}

@defproc[(char/p [c char?]) (parser/c char? char?)]{
Parses a single datum that is equal to @racket[c].}

@defproc[(char-ci/p [c char?]) (parser/c char? char?)]{
Parses a single datum that is case-insensitively equal to @racket[c], as determined by
@racket[char-ci=?].}

@defthing[letter/p (parser/c char? char?)]{
Parses an alphabetic letter, as determined by @racket[char-alphabetic?].}

@defthing[digit/p (parser/c char? char?)]{
Parses a single digit, as determined by @racket[char-numeric?].}

@defthing[space/p (parser/c char? char?)]{
Parses a single whitespace character, as determined by @racket[char-whitespace?] or
@racket[char-blank?].}

@defthing[integer/p (parser/c char? integer?)]{
Parses a sequence of digits as an integer. Does not handle negative numbers or numeric separators.}

@defproc[(string/p [str string?]) (parser/c char? string?)]{
Parses a sequence of characters equal to @racket[str] and returns @racket[str] as its result.}
