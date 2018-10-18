# nScheme language implementation library

## TODO

### bootstrap with only simple code

* create eval.scm: interpreter for evaluating; written in stageable subset
  * note: it may perform better on small tests than stage.scm
  * add syntax error checking in eval.rkt, then port it to nScheme as eval.scm
  * allow base-test.scm to work with both stage.scm and eval.scm

* compare performance on tests
  * stage.rkt running stage.scm vs. stage.rkt running eval.scm (latter may be faster?)

* try bootstrapping the interpreter for self-hosting:
  * improve module manipulation and testing to make this easier?
  * stage.rkt running stage.scm on both stage.scm and eval.scm; compile ast to Racket
  * compare compiled Racket performance on tests
  * if successful, start porting module construction and testing to nScheme

* possible pre-bootstrap ast improvement
  * generated Racket code is currently enormous, partly due to base library
  * try bootstrapping eval.scm/stage.scm first to see how bad it will really be
  * augmented ast
    * vector (for vectorizing apply), begin, let, maybe letrec (for lambdas only)
    * variable refcounts, mutability, escape status
    * crude termination/effect status
      * expr always evaluate to a value without observable effects? yes/no
      * tracking more precise status gets too complicated for now, e.g.:
        * some type info is needed to notice error effects
        * tracking effects under lambda is important for higher order procedures
      * inlining could improve precision without the analysis complexity
    * doubly-linked tree?
      * children point to parent and know position in parent
      * variables point to both their binding and references
  * simple, code shrinking optimizations
    * ast augmentation and normalization; worker/wrapper-ification
      * lambda and apply are vectorized; lambdas are factored into worker and wrapper
        * param tree manipulation is elaborated, leaving only single-arg lambdas
        * single-arg, always-inlineable "wrapper" lambdas call multi-arg workers
          * workers must be named and lifted out, to avoid recursive inlining forever
        * e.g., (lambda (x . y) _) => (let ((f (lambda #(x y) _)))
                                        (lambda a  ;; inlined as often as possible
                                          (assert (pair? a))
                                          (let ((a.1 (car a)) (a.2 (cdr a)))
                                            (apply f (vector a.1 a.2)))))
      * (apply (lambda x _) a) => (let ((x a)) _)
      * (apply (lambda #(x ...) _) (vector a ...)) => (let ((x a) ...) _)
      * (let ((#f X) _ ...) Y) => (begin X (let (_ ...) Y))
      * lift lets everywhere while preserving effect order; e.g., let in let binding:
        (let (X ... ((v (let (A ...) B))) Y ...) _) =>  ;; assumes unique var names
        (let (X ... A ...) (let ((v B) Y ...) _))
    * value decomposition/propagation/folding/dedup
      * complex constructions are decomposed with let bindings
    * context simplification
      * non-last begin context reduces to effects; non-effects eliminated
        * e.g., (begin (cons (mvector-set! A ...) B) C ...) =>
                (begin (mvector-set! A ...) B C ...)
            and (begin VALUE _ ...) => (begin _ ...)
      * unreferenced binders become #f; #f-bound values simplify to #t
        * same idea as begin's effect context; dead code elimination
        * try to infer #f-bound-ness across non-inlined procedure application
      * if-condition context reduces to truthiness; more dead code elimination
    * worker-wrapper inlining, single-site inlining, eta-expansion inlining
      * (lambda a* (apply f a*)) is an eta-expansion of variable f; safe to inline
      * general eta-reduction would also be safe with enough type info
    * if these techniques don't provide enough shrinking:
      * some other specualtive, shrinking inlining (fuel-based inline-and-measure)
  * supercompilation (if simple techniques don't provide enough performance)
    * speculative, effect-aware, memoized inlining
      * memoization produces letrecs of residual procedures
      * should first lower set!-able variables to mvectors
    * branching w/ logic variables tracking condition properties
    * rollback with generalization


* parallel processing: spawn, (mvector-cas! mv i expected new) => boolean?

* define ports, read, write

* throw away dead Racket support code
  * replace as much Racket as possible for module building, testing, repl
    * still need Racket to provide filesystem/IO interface
    * see ../../README.md on Racket platform
  * eliminate unused bootstrap primitives from nscheme.rkt
    * import/export, `and/let*`, case, quasiquote, match, read/write
    * throw away nscheme.rkt entirely if possible

* small-step evaluation with transparent values
  * for interleaving evaluation and compilation
    * guarantees serializability of residual programs
    * e.g., transformations such as inlining, staged compilation
    * flexible syntactic extension
      * parsers can safely produce code containing computed values
        * the computed values will be serializable, amenable to analysis
  * eventually also for resource control: budgeting time, memory usage


### syntax extensions

* light extensions
  * case-lambda, case-let, quasiquote
  * lambda-syntax, let[rec]-syntax, define-syntax, define-syntax/define
  * maybe quasiquote, case, extended cond

* definition context conveniences
  * embedded procedures in a definition context that take st as argument
    * #(define ,(lambda (st) ...))
  * ($define (lambda (st) ...)) to directly access state without embedded procs
  * property manipulation shorthand
    * (describe identifier ((property-name value) ...))
    * `(describe* (id*) kvs)`
    * e.g., (describe var ((set! #f))) to hide the capability to set! a variable
  * @definitions-run to force accmulated actions to run, and reset defined names
    * for staging support, defstate needs a continuation stack
  * other ideas for commands:
    (define-set! name form) : eval form and stick in set! prop
    (define-ref  name form) : eval form and stick in ref prop
    ;; some of these don't need to add commands; can perform effect immediately
    (define-syntax name form)
    (define-definition-syntax name form)
    (define-alias name name)
    (declare-syntax names ...)
    (declare-constant names ...)
    (declare-hidden names ...)
    (set-parameter! name form)  ;; dynamic unwinding assignment
    ;; these decompose into (multiple) define(s)
    (define* names ...) ;; define each to #t initially
    (define-vector-type name field ...)


### compiler backend targeting Racket

* procedure representation supporting non-list apply

* fully self-host
