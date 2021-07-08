# megaparsack [![Build Status](https://github.com/lexi-lambda/megaparsack/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/lexi-lambda/megaparsack/actions/workflows/build.yml)

This library implements a set of **practical parser combinators** inspired by libraries like [parsec][parsec], [megaparsec][megaparsec], and [parsack][parsack]. It can be used to build parsers for arbitrary input data, and it includes built-in facilities for parsing textual data and tokens produced by [`parser-tools/lex`][parser-tools/lex].

To install it, simply install the `megaparsack` package:

```
$ raco pkg install megaparsack
```

[For more information, see the full documentation.][megaparsack-doc]

[megaparsack-doc]: http://docs.racket-lang.org/megaparsack/
[megaparsec]: https://hackage.haskell.org/package/megaparsec
[parsack]: http://docs.racket-lang.org/parsack/
[parsec]: https://hackage.haskell.org/package/parsec
[parser-tools/lex]: http://docs.racket-lang.org/parser-tools/Lexers.html
