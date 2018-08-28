# nScheme language implementation library

## TODO

### bootstrap with only simple code

* no syntax for match
  * build userspace version of this via unify, assoc, etc.
  * `var`, `identifier`, `_`
  * `quote`, `quasiquote`, `cons`, `list*`, `list`, `vector`, `literal`
  * `unquote`, `unquote-splicing`, `#(qq ...)`, `(qq . qq)`, `literal`
  * maybe support a simpler match syntax instead

* op handling
  * allow primitives/built-ins to be used in any value position
    * during compilation, transform these uses based on position
      * reduces the amount of inlining work
  * define some derived ops, such as `append` and `equal?`, as built-ins that
    will be elaborated in terms of real primitives during compilation
  * these still need lexical entries in the expansion environment, though

* better firewalling of base library/env
  * mark lexical bindings as constant (set! disallowed)

* define these after bootstrapping: let/blacklist, let/whitelist[/syntax]

* nscheme.scm: frontend
* test.scm

* ast-code (transparent data) vs. ast-eval (opaque procedures)
  * more explicit linking and program building
    * show example of varied implementations of the same exported names
    * try building a program.db.scm (also maybe rename lib to library)

* `assoc-ref*`

* better naming convention: import-apply

* env: [(srcname, (tgtname, proc|'lexical|#f))]
  * incorporate lexical constant (set! disallowed) markings
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

* flexible module body interpretation: (language evaluation-adaptor ...)
