#lang scribble/manual

@(require (for-label racket/file)
          scribble/bnf
          "util.rkt")

@(define-parser-interaction state-interaction close-state! #:eval state-eval)

@title[#:tag "state"]{Parsers with State}

So far, all of the languages we have attempted to parse have been
@hyperlink["https://en.wikipedia.org/wiki/Context-free_grammar"]{context free}, but in practice, many
languages have varying amounts of context sensitivity. Parsers for such languages are often made much
simpler by the addition of state that tracks the necessary context. However, megaparsack parsers can
backtrack, which makes maintaining mutable state somewhat subtle: if a parser abandons a parse branch,
all its state modifications must be rolled back.

To make this simpler, megaparsack provides built-in support for arbitrary, user-defined state in the
form of @tech{parser parameters}. Parser parameters are similar to ordinary @reftech{parameters}, but
their values are associated with a parse context rather than with a thread. This means their values
are automatically rolled back whenever the parser backtracks, and they behave predictably regardless
of parser evaluation order.

@section[#:tag "state-context"]{Parsing with context}

Suppose we have a simple language that consists of a sequence of variable declarations of the form

@BNF[(list @nonterm{decl} @BNF-seq[@litchar{let} @nonterm{var} @litchar{=} @nonterm{integer}])]

where each declaration appears on a separate line. We might write a parser for such a language like
this:

@(state-interaction
  (define name/p
    (map (compose1 string->symbol list->string) (many+/p letter/p)))
  (define declaration/p
    (do (string/p "let ")
        [name <- name/p]
        (string/p " = ")
        [value <- integer/p]
        (pure (cons name value))))
  (define declarations/p
    (many/p (do [decl <- declaration/p]
                (char/p #\newline)
                (pure decl)))))

This definition works alright:

@(state-interaction
  (eval:check (parse-string declarations/p
                            (string-append "let x = 1\n"
                                           "let y = 2\n"))
              (success '((x . 1) (y . 2)))))

However, note that it also accepts multiple declarations with the same name, which may not be desired:

@(state-interaction
  (eval:check (parse-string declarations/p
                            (string-append "let x = 1\n"
                                           "let x = 2\n"))
              (success '((x . 1) (x . 2)))))

One way to prevent this is to keep track of all the declarations that we’ve parsed so far using a
@tech{parser parameter}:

@(state-interaction
  (define declared-names (make-parser-parameter '())))

Just like an ordinary @reftech{parameter}, we can read the current value of a parser parameter simply
by calling it as a procedure, like @racket[(declared-names)], and we can update its value by applying
it to a single argument, like @racket[(declared-names _new-value)]. However, unlike an ordinary
parameter, the applying a parser parameter procedure does not directly return or update the parser
parameter’s value. Instead, it returns a @tech{parser} that, when executed, parses no input, but
returns or updates the parser parameter’s value.

This means we can sequence reads and writes to @racket[declared-names] the same way we sequence any
other parser, using @racket[do]:

@(state-interaction
  (define declaration/p
    (do (string/p "let ")
        [names <- (declared-names)]
        [name <- (guard/p name/p
                          (λ (name) (not (memq name names)))
                          "an unused variable name")]
        (declared-names (cons name names))
        (string/p " = ")
        [value <- integer/p]
        (pure (cons name value)))))

Now duplicate definitions are rejected with a helpful error:

@(state-interaction
  #:hidden
  (define declarations/p
    (many/p (do [decl <- declaration/p]
                (char/p #\newline)
                (pure decl)))))
@(state-interaction
  (eval:error (parse-result! (parse-string declarations/p
                                           (string-append "let x = 1\n"
                                                          "let x = 2\n")))))

@section[#:tag "state-indentation"]{Indentation sensitivity}

Another useful application of parser parameters is parsing languages that are sensitive to
indentation. For example, we might wish to parse a bulleted list of items, like this:

@(define groceries.txt @list{
* produce
  * apples
  * spinach
* dairy
  * milk
    * whole milk
    * buttermilk
  * cheese
    * cheddar
    * feta})

@filebox["groceries.txt" (apply verbatim groceries.txt)]

To track the current indentation level, we can use a parser parameter:

@(state-interaction
  (define current-indent (make-parser-parameter 0))
  (define indentation/p
    (do [indent <- (current-indent)]
        (repeat/p indent (char/p #\space)))))

This makes defining a parser for an indentation-sensitive bulleted list remarkably straightforward:

@(state-interaction
  (define tree-list/p
    (do (try/p indentation/p)
        (string/p "* ")
        [entry <- (many+/p (char-not/p #\newline))]
        (char/p #\newline)
        [indent <- (current-indent)]
        [children <- (parameterize/p ([current-indent (+ indent 2)])
                       (many/p tree-list/p))]
        (pure (list (list->string entry) children)))))

The @racket[parameterize/p] form works just like @racket[parameterize], but with parser parameters
instead of ordinary ones. This definition of @racket[tree-list/p] is enough to parse the
@filepath{groceries.txt} file above:

@(state-eval `(define groceries.txt ,(string-append (apply string-append groceries.txt) "\n")))
@(state-interaction
  (eval:alts (define grocery-list (file->string "groceries.txt"))
             (define grocery-list groceries.txt))
  (eval:check (parse-string (many/p tree-list/p) grocery-list)
              (success
               '(("produce" (("apples" ()) ("spinach" ())))
                 ("dairy"
                  (("milk" (("whole milk" ()) ("buttermilk" ())))
                   ("cheese" (("cheddar" ()) ("feta" ())))))))))

Admittedly, in such a simple example, using a parser parameter is not strictly necessary. An
alternative definition of @racket[tree-list/p] could simply accept the indentation level as an
argument:

@(state-interaction
  (define (tree-list/p indent)
    (do (try/p (repeat/p indent (char/p #\space)))
        (string/p "* ")
        [entry <- (many+/p (char-not/p #\newline))]
        (char/p #\newline)
        [children <- (many/p (tree-list/p (+ indent 2)))]
        (pure (list (list->string entry) children)))))

However, in more complex parsers, this approach can require threading additional arguments through
several layers of nested parsers, which is difficult to read and even more difficult to maintain.
Just as ordinary parameters can help avoid threading values through many layers of nested functions,
parser parameters can help avoid threading them through nested parsers.

@(close-state!)
