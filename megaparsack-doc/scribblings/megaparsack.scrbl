#lang scribble/manual

@(require (for-label data/monad))

@title{Megaparsack: Practical Parser Combinators}

@defmodule[megaparsack]

Megaparsack is a @emph{parser combinator} library: a composable set of simple parsers that can be used
to create larger parsing systems that can parse arbitrary grammars, including context-sensitive ones.
Megaparsack uses the @racket[gen:monad] generic interface to provide a uniform interface to sequence
and compose different parsers using a base set of primitives.

@table-of-contents[]

@include-section["megaparsack/reference.scrbl"]
