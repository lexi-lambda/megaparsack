#lang scribble/manual

@(require "util.rkt")

@(define-parser-interaction choice-interaction close-choice!)

@title[#:tag "choice" #:style 'toc]{Parsers with Choice}

Most grammars are not completely static—usually, there is an element of @emph{choice}. For example,
when parsing a boolean, a valid value is @tt{true} or @tt{false}. Even more complicated, when parsing
a list of elements, a valid input might be @emph{any number of booleans}, all next to one another. To
handle these kinds of grammars, a parser can be provided with multiple paths, each of which are valid.

@local-table-of-contents[]

@section[#:tag "parsing-branching"]{Providing multiple paths}

To create a parser with multiple possibilities, use the @racket[or/p] combinator. It accepts any
number of parsers, and it tries them one at a time until one of them matches. For example, let’s
consider a parser that parses either the string @racket["true"] or @racket["false"], then returns the
value as a Racket boolean:

@(choice-interaction
  (define true/p
    (do (string/p "true")
        (pure #t)))
  (define false/p
    (do (string/p "false")
        (pure #f)))
  (define boolean/p
    (or/p true/p
          false/p)))

By using @racket[or/p], we’ve created a choice point, where the parser will try each path before
giving up. If none of the paths match, the parser will still fail, but if any of the paths match, it
will consider the parse successful and return. To demonstrate that this works, we can use our new
parser on some strings:

@(choice-interaction
  (eval:check (parse-string boolean/p "true") (success #t))
  (eval:check (parse-string boolean/p "false") (success #f)))

The @racket[or/p] combinator also automatically cooperates with error handling to provide helpful
error messages when parsing fails:

@(choice-interaction
  (eval:error (parse-result! (parse-string boolean/p "not a boolean"))))

Note that the error includes all the possible values that would have been considered valid at the
point that the parser failed.

Remember that the @racket[or/p] combinator is not magic: it does not attempt to predict which parse
will be valid, and it does not even try to look ahead to see which parse will be the longest. This can
cause problems when two different parses could @emph{both} succeed—@racket[or/p] will just pick the
first one:

@(parser-interaction
  (define overlapping/p
    (or/p (string/p "hello")
          (string/p "hello, world!")))
  (eval:check (parse-string overlapping/p "hello, world!") (success "hello")))

Just like ordinary boolean @racket[or], keep in mind that order does matter with @racket[or/p].

@subsection{Parsing ambiguous grammars}

So, if @racket[or/p] does not perform any lookahead, how exactly does it choose between parsers? It
might @emph{seem} like it tries each parser completely, then backtracks when any of them fail, but
this is not entirely true—consider the parser above, fixed so the longest match is first:

@(choice-interaction
  (define overlapping/p
    (or/p (string/p "hello, world!")
          (string/p "hello"))))

You might expect that, if the first match fails, it will try the second one, but in practice, this
doesn’t actually work:

@(choice-interaction
  (eval:check (parse-string overlapping/p "hello, world!") (success "hello, world!"))
  (eval:error (parse-result! (parse-string overlapping/p "hello"))))

What gives? Take a close look at the error message: it is expecting the rest of @tt{hello, world!},
but obviously we only gave it @tt{hello}. Why isn’t the parser backtracking? Well, megaparsack
actually does not backtrack by default. Instead, it implements a single-character lookahead: it tries
to parse the first token from each branch, and if it succeeds, it @emph{commits} to that path.

This means that, since part of the @tt{hello, world} parse was successful, the parser has already
committed to that branch and will not try any of the other options. This turns out to provide far
superior error reporting because it reports to the user precisely where the error occurred, not
somewhere much earlier in the parse. However, this obviously causes problems in this case where the
parse is @emph{ambiguous}, or more generally, the choice cannot be determined by a single character
of lookahead.

To solve this by allowing the parser to backtrack, use the @racket[try/p] combinator, which converts
a parser into one the backtracks upon failure. We can use this to solve our issue with our parser:

@(choice-interaction
  (define backtracking-overlapping/p
    (or/p (try/p (string/p "hello, world!"))
          (string/p "hello")))
  (eval:check (parse-string backtracking-overlapping/p "hello, world!") (success "hello, world!"))
  (eval:check (parse-string backtracking-overlapping/p "hello") (success "hello")))

All that @racket[try/p] does is disable the “committing” behavior mentioned earlier: instead of
committing to a particular path once any of the parse succeeds, any error that occurs within the
parser provided to @racket[try/p] is non-fatal, and the parser will backtrack and try the next
alternative.

@subsection{Backtracking with caution}

In this case, since the parse is truly ambiguous based on the first character, @racket[try/p] is the
correct approach. Note that the error messages are still helpful upon failure:

@(choice-interaction
  (eval:error (parse-result! (parse-string backtracking-overlapping/p "not hello"))))

However, be deliberate about where you put @racket[try/p] because it is very easy to end up with a
parser that provides completely useless error messages because all errors simply backtrack instead of
failing fast and reporting the real problem. For an example of this, consider a parser that parses an
integer or a boolean, depending on a label provided first:

@(choice-interaction
  (define the-labeled-integer/p
    (do (string/p "the integer: ")
        integer/p))
  (define the-labeled-boolean/p
    (do (string/p "the boolean: ")
        boolean/p)))

It might be tempting to use @racket[try/p] here because we know that the integer case might fail.
Therefore, you might write the parser like this:

@(choice-interaction
  (define try-labeled/p
    (or/p (try/p the-labeled-integer/p)
          the-labeled-boolean/p)))

This parser seems innocuous enough, right? It even works successfully:

@(choice-interaction
  (eval:check (parse-string try-labeled/p "the integer: 42") (success 42))
  (eval:check (parse-string try-labeled/p "the boolean: false") (success #f)))

But there is a lurking problem with this parser, and that’s its error messages. Consider a mismatch,
when we provide the @tt{the integer:} label but do not actually provide an integer:

@(choice-interaction
  (eval:error (parse-result! (parse-string try-labeled/p "the integer: false"))))

Oops. What happened? Well, the parser tried to parse an integer, but it failed, so it backtracked. It
then tried to parse a boolean, and it parsed the @tt{the}, but then it failed, too, so it reported an
error message. To a user, though, that error message is totally useless. The @emph{actual} issue is
that they should have provided an integer, but instead provided a boolean. Unfortunately, the
overzealous backtracking has eliminated that information.

This is tricky, because we can’t just drop the @racket[try/p]—since both cases share @tt{the}, the
parse is ambiguous without a little bit of lookahead. In order to fix this, what we really want to
do is factor out the common @tt{the}, which will allow us to eliminate the @racket[try/p] altogether:

@(choice-interaction
  (define labeled-integer/p
    (do (string/p "integer: ")
        integer/p))
  (define labeled-boolean/p
    (do (string/p "boolean: ")
        boolean/p))
  (define labeled/p
    (do (string/p "the ")
        (or/p labeled-integer/p
              labeled-boolean/p))))

Since we’ve removed all of the uses of @racket[try/p], now the parser can provide much more precise
error messages when we provide invalid input.

@(choice-interaction
  (eval:error (parse-result! (parse-string labeled/p "the integer: false"))))

@section{Parsing sequences}

Using @racket[or/p], it is possible to choose between alternatives when parsing, but what if a
particular grammar permits @emph{any number of} elements in sequence? For that, you can use the
@racket[many/p] combinator. It accepts a parser and attempts to parse it over and over again until
it fails. For example, here is a parser that parses any number of occurrences of the letter @tt{a}:

@(parser-interaction
  (eval:check (parse-string (many/p (char/p #\a)) "") (success '()))
  (eval:check (parse-string (many/p (char/p #\a)) "a") (success '(#\a)))
  (eval:check (parse-string (many/p (char/p #\a)) "aaaa") (success '(#\a #\a #\a #\a))))

This allows creating grammars that parse arbitrary numbers of values. The @racket[many/p] combinator
accepts an optional keyword argument @racket[#:min-count] to specify a minimum number of values to
parse. This can be used to parse two integers separated by some amount of whitespace, for example.

@(parser-interaction
  (define two-integers/p
    (do [x <- integer/p]
        (many/p space/p #:min-count 1)
        [y <- integer/p]
        (pure (list x y))))
  (eval:check (parse-string two-integers/p "13     102") (success '(13 102))))

Perhaps even more frequently, though, you may want to parse some number of values separated by some
delimiter. For example, perhaps you want to parse a whole list of integers separated by commas. That
can be accomplished by passing a parser for the @racket[#:separator] argument to @racket[many/p].

@(parser-interaction
  (define many-integers/p
    (many/p integer/p #:separator (char/p #\,)))
  (eval:check (parse-string many-integers/p "1,2,3,5,8,13") (success '(1 2 3 5 8 13))))

Often an unbounded number of values is undesirable: some limit is desired. The @racket[#:max-count]
argument to @racket[many/p] allows specifying a max number of values to parse. For example, we may
not wish to allow more than five comma-separated integers.

@(parser-interaction
  (define at-most-five-integers/p
    (many/p integer/p #:separator (char/p #\,) #:max-count 5))
  (eval:check (parse-string at-most-five-integers/p "1,2,3") (success '(1 2 3)))
  (eval:check (parse-string at-most-five-integers/p "1,2,3,5,8,13") (success '(1 2 3 5 8))))

@(close-choice!)
