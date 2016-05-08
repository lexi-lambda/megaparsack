#lang scribble/manual

@(require "util.rkt")

@title[#:tag "parsing-basics"]{Parsing Basics}

Megaparsack is a library for manipulating @deftech{parsers}, which are, very simply, functions that
operate on streams of tokens. This is very broad: the tokens in question can simply be characters in a
string, they can be tokens produced as the result of a lexer, they can be syntax objects, or they can
even be completely arbitrary data.

What’s special about parsers is that they can be @emph{sequenced}—that is, multiple parsers can be
chained together to make a larger parser. For example, to make a parser that parses the string
@racket["ab"], you might compose two parsers that parse the characters @racket[#\a] and @racket[#\b]
individually.

@section[#:tag "getting-started"]{Getting started with parsers}

To get started, require the @racketmodname[megaparsack] and @racketmodname[megaparsack/text]
libraries.

@(racketinput
  (require #,(racketmodname megaparsack) #,(racketmodname megaparsack/text)))

This will import the basic parser functions, as well as some built-in parsers for parsing textual
data. Now, you can use the @racket[parse-string] function along with basic parsers to parse values
from strings. Let’s start by parsing an integer:

@(parser-interaction
  (eval:check (parse-string integer/p "42") (right 42)))

Since the parser was successful, it returns a @racket[right] value. The @racket[parse-string] function
returns an @functech{either} value that represents success and failure. For example, take a look at
what would happen when a parse fails:

@(parser-interaction
  (parse-string integer/p "not an integer"))

When a parse fails, it returns a @racket[left] value that encodes some information about what caused
the parser to error. You can convert that information to a human-readable error message using the
@racket[parse-error->string] function:

@(parser-interaction
  (map-left parse-error->string (parse-string integer/p "not an integer")))

You can also assert that a parse will succeed and just get the result out by using the
@racket[parse-result!] function, which will throw an @racket[exn:fail:read:megaparsack] exception
when the parser fails.

@(parser-interaction
  (eval:check (parse-result! (parse-string integer/p "42")) 42)
  (eval:error (parse-result! (parse-string integer/p "not an integer"))))

You may notice that the error message includes some useful information. Specifically, megaparsack will
attempt to provide the following information to the user whenever a parse fails:

  @itemlist[
    @item{the source location of the error that caused the parse to fail}
    @item{the token that was “unexpected”, which caused the parse to fail}
    @item{a set of values that were “expected”, which would have been accepted as valid values for the
          parse}]

In the above case, the parser reports that it expected an integer, but it encountered the character
@tt{n}, which obviously isn’t a valid piece of an integer.

@section[#:tag "parsing-textual-data"]{Parsing textual data}

The @racket[integer/p] parser, as would be expected, parses a single integer. However, this isn’t very
useful on its own—most of the time, you will want to parse something much more complicated than that.
However, it is a useful building block for creating larger parsers. Let’s look at some other “building
block” parsers that work with strings.

The @racket[letter/p], @racket[digit/p], and @racket[space/p] parsers parse a single letter, digit, or
whitespace character, respectively:

@margin-note{
  Note that these parsers succeed even when only part of the input is consumed. This is important when
  combining parsers together, but if you want to ensure a parser parses the entire input, you can use
  @racket[eof/p].}

@(parser-interaction
  (eval:check (parse-string letter/p "hello") (right #\h))
  (eval:check (parse-string digit/p "123") (right #\1))
  (eval:check (parse-string space/p " ") (right #\space)))

The @racket[char/p] function creates a parser that parses a single character:

@(parser-interaction
  (eval:check (parse-string (char/p #\a) "abc") (right #\a))
  (eval:error (parse-result! (parse-string (char/p #\a) "xyz"))))

It may not make very much sense why the @racket[char/p] parser is useful—after all, it just seems to
return itself. Indeed, in these contrived examples, it’s not very useful at all! However, it becomes
@emph{extremely} important when combining multiple parsers together.

@section[#:tag "sequencing-parsers"]{Sequencing parsers}

@(define-parser-interaction sequencing-interaction close-sequencing!)

All @tech{parsers} are @functech{monads}, so it’s possible to use @racket[chain] and @racket[do] to
combine multiple parsers together to create a bigger parser. For example, let’s create a parser that
parses the letters @tt{a} and @tt{b} in sequence:

@(sequencing-interaction
  (define ab/p
    (do (char/p #\a)
        (char/p #\b))))

Now we can use our new parser just like any other:

@(sequencing-interaction
  (eval:check (parse-string ab/p "ab") (right #\b))
  (eval:error (parse-result! (parse-string ab/p "ac"))))

The parser succeeds when we supply it with the string @racket["ab"], but it fails when it doesn’t
match, and we automatically get a pretty good error message.

One thing to note is that the result of the parser is not particularly meaningful—it’s just
@racket[#\b]. That’s because the last parser in the @racket[do] block was @racket[(char/p #\b)], so
the result of @racket[ab/p] is just the result of its final parser. If we wanted to, we could change
the result to be whatever we wanted (but only on a successful parse) by returning our own value at the
end of the @racket[do] block:

@(sequencing-interaction
  (define ab*/p
    (do (char/p #\a)
        (char/p #\b)
        (pure "success!"))))

We need the @racket[pure] wrapper in order to properly “lift” our arbitrary value into the context of
a parser. Now we can run our new parser and get back our custom value when it succeeds:

@(sequencing-interaction
  (eval:check (parse-string ab*/p "ab") (right "success!")))

This parser is a little silly, but we can use these concepts to implement parsers that might actually
be useful. For example, you might need to parser two integers, separated by a comma, then add them
together. Using the monadic parser interface, this is extremely simple:

@(sequencing-interaction
  (define add-two-ints/p
    (do [x <- integer/p]
        (char/p #\,)
        [y <- integer/p]
        (pure (+ x y)))))

This definition is a little bit more complicated because we are using the results of the two integer
parsers in our sequence, so we use the @racket[[_a <- _b]] syntax to “pull out” the result of each
parser and bind it to a variable. Then we can add the two results together at the end. Actually using
this parser works just as intended:

@(sequencing-interaction
  (eval:check (parse-string add-two-ints/p "7,12") (right 19)))

Using this technique, it’s possible to build up fairly complex parsers from small, self-contained
units.

@(close-sequencing!)
