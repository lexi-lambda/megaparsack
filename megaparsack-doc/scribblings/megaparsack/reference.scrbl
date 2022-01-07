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
Produces a @reftech{contract} that recognizes @tech{parsers}. Tokens consumed by the parser must match
@racket[in-ctc], and values returned by the parser must match @racket[out-ctc].

If both @racket[in-ctc] and @racket[out-ctc] are @reftech{chaperone contracts}, then the result will
also be a @reftech{chaperone contract}.}

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

@defstruct*[syntax-box ([datum any/c] [srcloc srcloc?]) #:transparent]{
Represents a single parsable entity. Just like @reftech{syntax objects}, a @deftech{syntax box}
associates some source location information with an arbitrary datum. However, unlike ordinary syntax
objects, values like lists and vectors can be stored in a syntax box without being recursively wrapped
in additional layers of syntax objects.

The @racket[datum] can be anything at all, but usually it is either a character or some token produced
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
Tries each @racket[parser] in succession until one either succeeds or consumes input, at which point
its result will be returned as the result of the overall parse. Parsers that consume input but fail
will halt further parsers from being tried and will simply return an error; if backtracking is
desired, the parser should be wrapped with @racket[try/p].

@history[#:changed "1.5" @elem{Changed to always return the first successful result, rather than
           continuing to try parsers until one consumes input. The new behavior is more predictable
           and more consistent with existing Parsec implementations, though the old behavior was
           more consistent with the presentation in the original paper.}]}

@defproc[(try/p [parser parser?]) parser?]{
Creates a new parser like @racket[parser], except that it does not consume input if @racket[parser]
fails. This allows the parser to backtrack and try other alternatives when used inside a
@racket[or/p] combinator.}

@(define-parser-interaction lookahead-interaction close-lookahead!)

@defproc[(lookahead/p [parser parser?]) parser?]{
Creates a new parser like @racket[parser], except that it does not consume input if @racket[parser]
succeeds, so subsequent parsers will continue from the same location in the input stream. This
allows a parser to ensure something will appear in future input without immediately consuming it.

For example, @racket[lookahead/p] can be used to implement a parser that only succeeds at the end of a
line, but does not consume the newline character itself:

@(lookahead-interaction
  (define end-of-line/p (lookahead/p (char/p #\newline))))

This can be used to parse, for example, line comments that span to the end of the current line, while
still allowing a later parser to consume the newline character:

@(lookahead-interaction
  (define rest-of-line/p
    (or/p (do end-of-line/p (pure ""))
          (do [c <- any-char/p]
              [cs <- rest-of-line/p]
              (pure (string-append (string c) cs)))))
  (define line-comment/p
    (do (try/p (string/p "# "))
        rest-of-line/p))
  (eval:check (parse-string (many/p (do [line <- line-comment/p]
                                        (char/p #\newline)
                                        (pure line)))
                            (string-append "# hello\n"
                                           "# world\n"))
              (success (list "hello" "world"))))

Note that if @racket[parser] @emph{fails}, @racket[lookahead/p] has no effect; if it consumed input
before failing, it will not try other alternatives in an enclosing @racket[or/p]. Wrap @racket[parser]
with @racket[try/p] if this behavior is undesirable.

@history[#:added "1.5"]}

@(close-lookahead!)

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
in syntax (for example, @racket[many/p] returns a list of results, not a syntax list).}

@defproc[(syntax-box/p [parser parser?]) (parser/c any/c syntax-box?)]{
Like @racket[syntax/p], but wraps the result in a @racket[syntax-box] instead of a @reftech{syntax
object}. This is useful if you want to get the source location information from a parse result, but
you want to ensure the underlying datum remains untouched.}

@defproc[(fail/p [msg message?]) (parser/c any/c none/c)]{
Produces a parser that always fails and produces @racket[msg] as the error message. This is the
lowest-level way to report errors, but many cases in which you would want to raise a custom failure
message can be replaced with @racket[guard/p] instead, which is slightly higher level.}

@defform[(delay/p parser-expr)
         #:contracts ([parser-expr parser?])]{
Creates a parser that delays evaluation of @racket[parser-expr] until the first time it is applied to
input. Otherwise, the parser’s behavior is identical to that of @racket[parser-expr]. The parser
returned by @racket[delay/p] never evaluates @racket[parser-expr] more than once, even if it’s applied
to input multiple times.

@(parser-examples
  (define delayed/p (delay/p (begin (println 'evaluated)
                                    (char/p #\a))))
  (eval:check (parse-result! (parse-string delayed/p "a")) #\a)
  (eval:check (parse-result! (parse-string delayed/p "a")) #\a))

@racket[delay/p] can be used to delay evaluation in situations where a (possibly mutually) recursive
parser would otherwise require evaluating a parser before its definition. For example:

@(parser-interaction
  (define one/p (or/p (char/p #\.)
                      (delay/p two/p)))
  (define two/p (list/p (char/p #\a) one/p))
  (eval:check (parse-result! (parse-string one/p "aa.")) '(#\a (#\a #\.))))

Without the use of @racket[delay/p], the reference to @racket[two/p] would be evaluated too soon
(since @racket[or/p] is an ordinary function, unlike @racket[or]).

Note that the @racket[delay/p] expression itself may be evaluated multiple times, in which case the
@racket[parser-expr] may be as well (since each evaluation of @racket[delay/p] creates a separate
parser). This can easily arise from uses of @racket[do], since @racket[do] is syntactic sugar for
nested uses of @racket[lambda], though it might not be syntactically obvious that the @racket[delay/p]
expression appears under one such @racket[lambda]. For example:

@(parser-interaction
  (define sneaky-evaluation/p
    (do (char/p #\a)
        (delay/p (begin (println 'evaluated)
                        (char/p #\b)))))
  (eval:check (parse-result! (parse-string sneaky-evaluation/p "ab")) #\b)
  (eval:check (parse-result! (parse-string sneaky-evaluation/p "ab")) #\b))

In other words, @racket[delay/p] doesn’t perform any magical memoization or caching, so it can’t be
used to prevent a parser from being evaluated multiple times, only to delay its evaluation to a later
point in time.

@history[#:added "1.6"]}

@defproc[(many/p [parser parser?]
                 [#:sep sep parser? void/p]
                 [#:min min-count exact-nonnegative-integer? 0]
                 [#:max max-count (or/c exact-nonnegative-integer? +inf.0) +inf.0])
         (parser/c any/c list?)]{
Produces a parser that attempts @racket[parser] at least @racket[min-count] times and at most
@racket[max-count] times, with attempts separated by @racket[sep]. The returned parser produces a
list of results of successful attempts of @racket[parser]. Results of @racket[sep] are ignored.

@(parser-examples
  (define letters/p (many/p letter/p))
  (eval:check (parse-result! (parse-string letters/p "abc")) (list #\a #\b #\c))
  (define dotted-letters/p
    (many/p letter/p #:sep (char/p #\.) #:min 2 #:max 4))
  (eval:check (parse-result! (parse-string dotted-letters/p "a.b.c")) (list #\a #\b #\c))
  (eval:error (parse-result! (parse-string dotted-letters/p "abc")))
  (eval:error (parse-result! (parse-string dotted-letters/p "a")))
  (eval:check (parse-result! (parse-string dotted-letters/p "a.b.c.d.e")) (list #\a #\b #\c #\d)))

@history[#:added "1.1"]}

@defproc[(many+/p [parser parser?]
                  [#:sep sep parser? void/p]
                  [#:max max-count (or/c exact-nonnegative-integer? +inf.0) +inf.0])
         (parser/c any/c list?)]{
Like @racket[many/p], but attempts @racket[parser] at least once. Equivalent to
@racket[(many/p parser #:sep sep #:min 1 #:max max-count)].

@history[#:changed "1.1" @elem{Added support for @racket[#:sep] and @racket[#:max] keyword arguments
                               for consistency with @racket[many/p].}]}

@defproc[(repeat/p [n exact-nonnegative-integer?] [parser parser?]) (parser/c any/c list?)]{
Produces a parser that attempts @racket[parser] @emph{exactly} @racket[n] times and returns a list
of the results. Equivalent to @racket[(many/p parser #:min n #:max n)].}

@defproc[(==/p [v any/c] [=? (any/c any/c . -> . any/c) equal?]) parser?]{
Produces a parser that succeeds when a single datum is equal to @racket[v], as determined by
@racket[=?]. Like @racket[satisfy/p], it consumes a single datum upon success but does not consume
anything upon failure.}

@defproc[(one-of/p [vs list?] [=? (any/c any/c . -> . any/c) equal?]) parser?]{
Like @racket[(or/p (one-of/p _v =?) ...)]. Produces a parser that succeeds when a single datum is
equal to any of the elements of @racket[vs], as determined by @racket[=?]. Like @racket[satisfy/p],
it consumes a single datum upon success but does not consume anything upon failure.

@(parser-examples
  (eval:check (parse-result! (parse-string (one-of/p '(#\a #\b)) "a")) #\a)
  (eval:check (parse-result! (parse-string (one-of/p '(#\a #\b)) "b")) #\b)
  (eval:error (parse-result! (parse-string (one-of/p '(#\a #\b)) "c"))))

@history[#:added "1.2"]}

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

@defproc[(list/p [parser parser?] ... [#:sep sep parser? void/p]) (parser/c any? list?)]{
 Returns a @tech{parser} that runs each @racket[parser] in sequence separated by @racket[sep] and
 produces a list containing the results of each @racket[parser]. The results of @racket[sep] are
 ignored.

 @(parser-examples
   (define dotted-let-digit-let/p
     (list/p letter/p digit/p letter/p #:sep (char/p #\.)))
   (eval:check (parse-result! (parse-string dotted-let-digit-let/p "a.1.b")) (list #\a #\1 #\b))
   (eval:error (parse-result! (parse-string dotted-let-digit-let/p "a1c")))
   (eval:error (parse-result! (parse-string dotted-let-digit-let/p "a.1"))))

 Using a separator parser that consumes no input (such as the default separator, @racket[void/p]) is
 equivalent to not using a separator at all.

 @(parser-examples
   (define let-digit-let/p (list/p letter/p digit/p letter/p))
   (eval:check (parse-result! (parse-string let-digit-let/p "a1b")) (list #\a #\1 #\b)))}

@subsection[#:tag "parser-parameters"]{Parser Parameters}

@defproc[(make-parser-parameter [v any/c]) parser-parameter?]{
Returns a new @deftech{parser parameter}. A parser parameter is like an ordinary @reftech{parameter},
but instead of its state being scoped to a particular thread, a parser parameter’s state is scoped to
a particular call to @racket[parse]. Furthermore, modifications to parser parameters are discarded if
the parser backtracks past the point of modification, which ensures that only modifications from
@emph{successful} parse branches are retained.

Like ordinary parameters, parser parameters are procedures that accept zero or one argument. Unlike
ordinary parameters, the result of applying a parser parameter procedure is a @tech{parser}, which
must be monadically sequenced with other parsers to have any effect.

@(parser-examples
  (define param (make-parser-parameter #f))
  (eval:check (parse-result! (parse-string (param) "")) #f)
  (eval:check (parse-result! (parse-string (do (param #t) (param)) "")) #t))

Each call to @racket[parse] is executed with a distinct @deftech{parser parameterization}, which means
modifications to parser parameters are only visible during that particular parser execution. The
@racket[v] argument passed to @racket[make-parser-parameter] is used as the created parser parameter’s
initial value in each distinct parser parameterization.

Parser parameters are useful for tracking state needed by context-sensitive parsers, but they can also
be used to provide values with dynamic extent using @racket[parameterize/p], just as ordinary
parameters can be locally modified via @racket[parameterize].

@history[#:added "1.4"]}

@defproc[(parser-parameter? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a @tech{parser parameter}, otherwise returns @racket[#f].

@history[#:added "1.4"]}

@defform[(parameterize/p ([param-expr val-expr] ...) parser-expr)
         #:contracts ([param-expr parser-parameter?]
                      [parser-expr parser?])]{
Returns a new @tech{parser} that behaves just like @racket[parser-expr], except that the value of
each @tech{parser parameter} @racket[param-expr] is given by the corresponding @racket[val-expr]
during the dynamic extent of the parser’s execution.

@(parser-examples
  (define param (make-parser-parameter #f))
  (eval:check (parse-result! (parse-string (do [a <- (param)]
                                               [b <- (parameterize/p ([param #t])
                                                       (param))]
                                               [c <- (param)]
                                               (pure (list a b c)))
                                           ""))
              (list #f #t #f)))

If any of the @racket[param-expr]’s values are modified by @racket[parser-expr] via a direct call to
the parser parameter procedure, the value remains modified until control leaves the enclosing
@racket[parameterize/p] parser, after which the value is restored. (This behavior is precisely
analogous to modifying an ordinary @reftech{parameter} within the body of a @racket[parameterize]
expression.)

@(parser-examples
  (define param (make-parser-parameter #f))
  (eval:check (parse-result! (parse-string (do (param 1)
                                               [a <- (parameterize/p ([param 2])
                                                       (do (param 3)
                                                           (param)))]
                                               [b <- (param)]
                                               (pure (list a b)))
                                           ""))
              (list 3 1)))

@history[#:added "1.4"]}

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

@defproc[(char-not/p [c char?]) (parser/c char? char?)]{
Parses a single datum that is different from @racket[c].
@history[#:added "1.3"]}

@defproc[(char-ci/p [c char?]) (parser/c char? char?)]{
Parses a single datum that is case-insensitively equal to @racket[c], as determined by
@racket[char-ci=?].}

@defproc[(char-between/p [low char?] [high char?]) (parser/c char? char?)]{
Parses a single character that is between @racket[low] and @racket[high] according to
@racket[char<=?].

@(parser-examples
  (eval:check (parse-result! (parse-string (char-between/p #\a #\z) "d")) #\d)
  (eval:error (parse-result! (parse-string (char-between/p #\a #\z) "D"))))

@history[#:added "1.2"]}

@defproc[(char-in/p [alphabet string?]) (parser/c char? char?)]{
Returns a parser that parses a single character that is in @racket[alphabet].
                                                            
@(parser-examples
  (eval:check (parse-result! (parse-string (char-in/p "aeiou") "i")) #\i)
  (eval:error (parse-result! (parse-string (char-in/p "aeiou") "z"))))

@history[#:added "1.2"]}

@defproc[(char-not-in/p [alphabet string?]) (parser/c char? char?)]{
Returns a parser that parses a single character that is not in @racket[alphabet].
@history[#:added "1.3"]}

@defthing[any-char/p (parser/c char? char?)]{
Returns a parser that parses a single character.
@history[#:added "1.3"]}

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
Parses a sequence of characters that is case-insensitively equal to @racket[str] and returns @racket[str] as its result.}

@defproc[(string-ci/p [str string?]) (parser/c char? string?)]{
Parses a sequence of characters equal to @racket[str] (as determined by
@racket[char-ci=?]) and returns @racket[str] as its result.
@history[#:added "1.3"]}

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

@section[#:tag "deprecated-forms-and-functions"]{Deprecated Forms and Functions}

@defproc[(many*/p [parser parser?]) (parser/c list?)]{
@deprecated[#:what "function" @racket[many/p]]}

@defproc[(many/sep*/p [parser parser?] [sep parser?]) parser?]{
@deprecated[#:what "function" @racket[(many/p parser #:sep sep)]]}

@defproc[(many/sep+/p [parser parser?] [sep parser?]) parser?]{
@deprecated[#:what "function" @racket[(many+/p parser #:sep sep)]]}
