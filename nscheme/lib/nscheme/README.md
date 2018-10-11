# nScheme language implementation library

## TODO

### bootstrap with only simple code

* nscheme.rkt using fexprs
  * update/improve how modules work
  * replace nscheme.rkt with fexpr.rkt

* Redesign for static fexpr implementation
  * throw away interpret.scm
  * eval.scm: interpreter for evaluating; written in stageable subset
  * stage.scm: interpreter for producing asts (replace base.scm with this)
    * support generalized formal parameter trees
    * prefix with @ instead of parse:
    * @letrec in terms of definitions
    * @let/name in terms of @letrec
    * clean up uses of `@apply*`
    * rely less on embedding procedures in syntax

* definition context conveniences
  * embedded procedures in a definition context that take st as argument
    * #(define ,(lambda (st) ...))
  * ($define (lambda (st) ...)) to directly access state without embedded procs
  * property manipulation shorthand
    * (describe identifier ((property-name value) ...))
    * `(describe* (id*) kvs)`
    * e.g., (describe var ((set! #f))) to hide the capability to set! a variable
  * @definitions-run to force accmulated actions to run, and reset defined names

* extended syntax:
  * case-lambda, case-let, quasiquote
  * lambda-syntax, let[rec]-syntax, define-syntax, define-syntax/define

* ditch explicit parsing, in favor of full (static) fexprs
  * make ample use of constants to give stable meanings to syntax/fexprs
  * define eval via tiny staged language (corresponding closely with ast:)
  * static, in contrast with Kernel's dynamic fexprs
    * I don't see a downside to the static approach; it seems just as expressive;
      yet there are modularity/encapsulation benefits, so it may be more expressive
    * in Kernel, applicative behavior is a dynamic property of values via wrap/unwrap
    * in this approach, applicative behavior is a static property of bound names
      * syntactically apparent, determined by how a name is bound
      * any procedure may behave as an applicative or operative without modification
    * static approach allows encapsulation where desired
      * e.g., the dynamic approach, an operative passed to map will observe
        map's implementation details, and there is no way to prevent this
      * in the static approach, map can bind f as applicative, guaranteeing
        the security of its source code

* redesign definition list parsing/evaluation
  * express letrec in terms of this instead of the other way around?
  * definition state: #(env defined-names definition-commands expr-last?)
    * track the expr-last? flag to ensure a body ends on one
    * track defined-names to catch duplicates
    * gradually update env while establishing new commands
    * definition-commands describe things like set! and normal expression
  * some ideas for commands:
    aside from exprs, these could all just be represented as (env -> effect)
    have expr effects return their value/code
    #(define name form)
    #(#f form)  ;; expr
    ;; in the future? maybe these:
    #(match-define pattern form)
    #(define-set! name form) : eval form and stick in set! prop
    #(define-ref  name form) : eval form and stick in ref prop
    ;; some of these don't need to add commands; can perform effect immediately
    #(define-syntax name form)
    #(define-definition-syntax name form)
    #(definitions-break/end/start/split/newgroup...?)
    #(define-alias name name)
    #(declare-syntax names ...)
    #(declare-constant names ...)
    #(declare-hidden names ...)
    #(set-parameter! name form)  ;; dynamic unwinding assignment
    ;; not these, they decompose into (multiple) define(s)
    #(define* names ...) ;; define each to #t initially
    #(define-vector-type name field ...)


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
