#lang scribble/manual

@(require "util.rkt")

@title[#:tag "differences-from-parsack"]{Appendix: Parsack vs Megaparsack}

Megaparsack is not the only port of Parsec to Racket, and in fact, it isn’t even the first. The
original Parsec port is @hyperlink["http://docs.racket-lang.org/parsack/index.html"]{Parsack}. When
looking for a parser combinator library, you might be unsure which one to pick, so this attempts to
provide an unbiased comparison between the two libraries.

Without getting into the nitty gritty details, here’s a quick overview of the differences:

@itemlist[
 @item{Megaparsack is about two and a half years newer, initially released in the spring of 2016,
       while Parsack was released in the fall of 2013.}

 @item{Probably the most significant difference in the two libraries’ APIs is that Megaparsack can
       parse arbitrary tokens as input, while Parsack is specialized to exclusively operate on text or
       bytes. This allows Megaparsack to operate after an initial lexing phase (such as using
       @racketmodname[parser-tools/lex] with @racketmodname[megaparsack/parser-tools/lex]), while
       Parsack is designed to exclusively parse input directly.}

 @item{Megaparsack supports the production of syntax objects from parsers automatically, whereas
       Parsack does not.}

 @item{A less impactful difference but still significant design difference is that Megaparsack
       implements the @racket[gen:functor], @racket[gen:applicative], and @racket[gen:monad]
       interfaces from the @seclink["top" #:doc '(lib "scribblings/data/functional.scrbl")]{
       @tt{functional}} library, while Parsack is entirely monomorphic and provides its own
       sequencing and composition operators.}

 @item{Megaparsack provides contracts on parsers, while Parsack only includes a simple predicate. This
       is more important for Megaparsack because of the different token types that parsers can accept,
       but it’s also useful in general for denoting what parsers can produce.}

 @item{As a consequence of the above four differences, Parsack is currently @italic{considerably}
       faster than Megaparsack, @bold{by more than an order of magnitude}.}

 @item{Megaparsack’s documentation is better than Parsack’s, and it includes a tutorial-style guide.}

 @item{Megaparsack’s naming conventions are somewhat closer to idiomatic Racket, whereas Parsack’s
       names are more directly ported from Haskell.}

 @item{Megaparsack does not allow the user to customize the parser state, whereas Parsack does.}

 @item{Both Megaparsack and Parsack use the same general model for parsing, backtracking, and error
       reporting, which is adapted from the common parent, Parsec.}

 @item{Both Megaparsack and Parsack expose a monadic interface for composing and sequencing parsers
       together, and they both provide a minimal set of combinators for producing new parsers from
       primitives.}]

My general recommendation is to use Megaparsack unless performance is an issue, at which point it may
be worth it to use Parsack, instead. However, while some of Megaparsack’s design decisions do make it
inherently somewhat slower than Parsack, it’s likely that a lot of Megaparsack can be optimized much
more than it currently is. If you run into performance problems with Megaparsack, feel free to open
a bug report, and it might be possible to make Megaparsack palatably fast.
