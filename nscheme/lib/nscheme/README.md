# nScheme language implementation library

## TODO

### bootstrap with only simple code

* nscheme.scm: frontend
  * syntax.rkt should be absorbed by nscheme.scm; only need env/address/name defs
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
