# nScheme language implementation library

## TODO

### bootstrap with only simple code

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

* small-step evaluation with transparent values
  * for interleaving evaluation and compilation
    * guarantees serializability of residual programs
    * e.g., transformations such as inlining, staged compilation
    * flexible syntactic extension
      * parsers can safely produce code containing computed values
        * the computed values will be serializable, amenable to analysis

* when should convenient syntactic abstraction be reintroduced?
  * staging operatives (i.e., staged vau) that build transparent code?
    * transparency allows some local optimization/rewriting
    * but, would like to keep optimization aspects separate via hyperprograms
  * operatives that specifically operate within definition lists
    * simpler than recognizing macros that generate uses of begin/define

* flexible module body interpretation: (language evaluation-adaptor ...)

* module-level unquote[-splicing]
  * for convenient local macro expansion during bootstrap
