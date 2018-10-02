# nScheme language implementation library

## TODO

### bootstrap with only simple code

* redesign environments
  * consolidate all contexts for a bound name in a single property list
    * property list stores named capabilities
    * a name's participation in a context is determined by keys in this assoc
    * the values for various contexts correspond to parsers
    * e.g., var or ref, set!, apply, define
  * no need for general uids/addresses
    * ast:lambda will use raw params
    * keywords can be expressed by tagging a uid in the property list:
      * e.g., (... (else . (... (keyword . UID) ...)) ...)
  * should env be a procedure (object)? is this safe when running untrusted code?
    * untrusted code could inject a malicious env
    * possible methods: all-names, lookup, set/extend/bind, add-property

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

* replace $ names with more ast: names
* use parse:lambda more liberally
  * just embed a procedure in the body if context-specific parsing is desired
    * no need for special behavior of $lambda

* consider ditching explicit parsing, in favor of full (static) fexprs
  * can this simplify things even more? at what cost? still easy to reason about?
    * make ample use of constants to give stable meanings to syntax/fexprs
  * could reintroduce parsing if performance is an issue (after optimization)
  * define eval via tiny applicative language (corresponding closely with ast:)
    * can eval be written the same way in both Racket and applicative nScheme?
      * non-list apply is the only issue?
    * but parse to closures instead of vector-code, for flexibility
      * can run transparently to emit and optimize first order repr of closures
      * won't need things like env reification, since closure can just capture
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
