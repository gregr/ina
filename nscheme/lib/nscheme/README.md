# nScheme language implementation library

## TODO

### bootstrap with only simple code

* parallel processing: spawn, (mvector-cas! mv i expected new) => boolean?

* define ports, read, write

* try self-hosting the interpreter
  * define any missing procedures
  * replace as much Racket as possible for module building, testing, repl
    * still need Racket to provide filesystem/IO interface
    * see ../../README.md on Racket platform
  * eliminate unused bootstrap primitives from nscheme.rkt
    * import/export, `and/let*`, case, quasiquote, match, read/write

* small-step evaluation with transparent values
  * for interleaving evaluation and compilation
    * guarantees serializability of residual programs
    * e.g., transformations such as inlining, staged compilation
    * flexible syntactic extension
      * parsers can safely produce code containing computed values
        * the computed values will be serializable, amenable to analysis
  * eventually also for resource control: budgeting time, memory usage

* Is it necessary to implement bootstrap interpeter for nScheme in Racket?
  * the current poor emulation of nScheme in Racket may be enough
  * avoid using apply to pass a non-list argument to a variadic procedure


### syntax extensions

* lang:extended
  * provide more of the env/binding interface from base.scm for convenience
    * move it to parse.scm?  maybe not, would like all the base parsers too
  * light extensions
    * quasiquote, case, extended cond
  * heavier extensions
    * define-syntax, let[rec]-syntax
    * maybe match, extend lambda/`let[*]`/letrec/etc. to support matching

* flexible module body interpretation: (language evaluation-adaptor ...)
  * maybe parameterize over parsers (marked with context) instead


### compiler backend targeting Racket

* procedure representation supporting non-list apply

* fully self-host
