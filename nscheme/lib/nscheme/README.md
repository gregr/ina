# nScheme language implementation library

## TODO

### bootstrap with only simple code

* nscheme in "one page"
  * try again to eliminate match/case/quasiquote during bootstrapping
    * can we even get away without cond? maybe an incomplete cond?
    * ast-eval via cond instead of match
      * just check tag; no need to validate shape
    * concise error checking of shapes
      * various list shape predicates for when we do validate
      * box "logic var" pattern conds
      * e.g.,
        (define a (box #t)) (define b (box #t)) ...
        (define (? pattern)
          _unify with some datum, treating boxes like lvar via set-box!_)
        (cond ((? `(foo ,a ,b)) _use (unbox a) and (unbox b)_)
              ((? `(bar ,a)) _etc.) ...)
        * note, we might not have quasiquote when bootstrapping
  * don't need import/export primitives

* define these after bootstrapping: let/blacklist, let/whitelist[/syntax]

* small-step evaluation with transparent values
  * for interleaving evaluation and compilation
    * guarantees serializability of residual programs
    * e.g., transformations such as inlining, staged compilation
    * flexible syntactic extension
      * parsers can safely produce code containing computed values
        * the computed values will be serializable, amenable to analysis

* introduce convenient syntax extension immediately via fexprs/operatives
  * bootstrap via bare bones interpreter written directly in Racket
    * instead of poorly emulating nScheme in Racket as we're doing now
    * start with minimal operatives to conveniently define full self-evaluator
      * (e.g., define, let)
  * staging operatives (i.e., staged vau) that build transparent code?
    * transparency allows some local optimization/rewriting
    * but, would like to keep optimization aspects separate via hyperprograms
    * can simulate "immediate eval" operatives with wrappers that stage calls
      to the wrapped operative, so that it can eval directly without worry
  * operatives that specifically operate within definition lists
    * via multi-context environment bindings (definition context)
    * simpler than recognizing macros that generate uses of begin/define

* flexible module body interpretation: (language evaluation-adaptor ...)
