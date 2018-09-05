# nScheme language implementation library

## TODO

### bootstrap with only simple code

* define these after bootstrapping: let/blacklist, let/whitelist[/syntax]

* nscheme.scm: frontend
* test.scm

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
