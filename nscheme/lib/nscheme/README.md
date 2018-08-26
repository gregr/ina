# nScheme language implementation library

## TODO

### bootstrap with only simple code

* discard define-vector-type
  * instead, define vector type predicates as deconstructors
    * return #f or assoc of components
* no quasiquote, match, or even case?
  * build userspace versions of these via unify, assoc, etc.
    * case/let, match/let, type construction/deconstruction

* op handling
  * allow primitives/built-ins to be used in any value position
    * during compilation, transform these uses based on position
      * reduces the amount of inlining work
  * define some derived ops, such as `append` and `equal?`, as built-ins that
    will be elaborated in terms of real primitives during compilation

* define these now:
  * `and/let*`
  * (export name ...)
  * (import (name ...) body ...)
    * produces (cons '(name ...) (lambda (name ...) body ...))
  * userspace definitions: import/apply, import->lambda
* define these after bootstrapping: let/blacklist, let/whitelist[/syntax]

* define box.scm
* define type.scm to share type predicate deconstruction idea
* update data.scm, ast.scm, eval-ast.scm to avoid match
  * use new type predicates for ast

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
