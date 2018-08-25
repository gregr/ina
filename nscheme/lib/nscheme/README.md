# nScheme language implementation library

## TODO

### bootstrap with only simple code

* discard define-vector-type
  * instead, define vector type predicates as deconstructors
    * return #f or assoc of components
* no quasiquote, match, or even case?
  * build userspace versions of these via unify, assoc, etc.
    * case/let, match/let, type construction/deconstruction
* `and/let*`
* let/blacklist, let/whitelist[/syntax]
* (export name ...)
* (import (name ...) body ...)
  * produces (cons '(name ...) (lambda (name ...) body ...))
* userspace definitions: import/apply, import->lambda

* nscheme.scm: frontend
* test.scm

* env: [(srcname, (tgtname, proc|'lexical|#f))]
* representation independence: environments, closures, continuations?
* eliminate macros after all
  * no open/close needed
  * keep env-bound parsers
  * will need better `$lambda`, `$let`, `$let*`, and `$letrec`
    * $lambda takes source names for params, and `(src&renamed)*->body`
  * replace closed-name with overriding expander
    * expanders as procs, no need for wrapper?
      * similar for names?

* when should convenient syntactic abstraction be reintroduced?
  * staging operatives (i.e., staged vau) that build transparent code?
    * transparency allows some local optimization/rewriting
    * but, would like to keep optimization aspects separate via hyperprograms
  * operatives that specifically operate within definition lists
    * simpler than recognizing macros that generate uses of begin/define
