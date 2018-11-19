# nScheme language implementation library

## TODO

### bootstrap with only simple code

* reconsider stage.rkt performance using pre-built base library with more ast:prims

* reduce dependency on Racket in test.rkt
  * continue using Racket to load files and parse modules (i.e., module.rkt)
    * in order to bootstrap, Racket will need to be able to do these things anyway
  * port module evaluation and migrate actual test runs to nscm
    * Racket still needs to be able to evaluate modules for bootstrapping

* backend-racket code generation
  * use a decorated sexpr encoding for Racket-specific data
    strings vs. symbols, characters, keywords, etc.
  * delegate to Racket what to do with the generated code
    * code could be written to file or immediately evaluated by a racket-eval

* try bootstrapping the interpreter for self-hosting:
  * stage.rkt running stage.scm on both stage.scm and eval.scm; compile ast to Racket
  * compare compiled Racket performance on tests

* remove stage.rkt, maybe replacing it with ast.rkt? ideally won't even need ast.rkt
  * an alternative to Racket code generation is to only generate ASTs and run via
    ast.rkt (implementing ast-eval); no need for the full stage.rkt in this case
    * if delimited control is omitted, Chez Scheme could easily implement ast-eval
    * also a shortcut for inefficient, early versions of other backends, like JS

* possible pre-bootstrap ast improvement
  * generated Racket code is currently enormous, partly due to base library
  * try bootstrapping eval.scm/stage.scm first to see how bad it will really be
  * augmented ast
    * vectorized `lambda*` and `apply*`, begin, let, letrec for lambdas (aka labels)
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
        * e.g., (lambda (x . y) _) => (let ((f (lambda* (x y) _)))
                                        (lambda a  ;; inlined as often as possible
                                          (assert (pair? a))
                                          (let ((a.1 (car a)) (a.2 (cdr a)))
                                            (apply* f a.1 a.2))))
      * (apply (lambda x _) a) => (let ((x a)) _)
      * (apply* (lambda* (x ...) _) a ...) => (let ((x a) ...) _)
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


### Racket platform and integration

* define ports, read, write
  * include non-device (aka string/byte buffer) ports
    * for when you want a port interface to string-like data
  * prefer dealing with bytes/words, not strings

* provide host system capabilities to a program via lambda
  * (lambda (host) ... program ...)
  * host object can be queried for (virtual) hardware capabilities
    * spectrum of capability granting outcomes and other feedback:
      * no-grant to full-unconditional-grant
      * capability may or may not be recognized
        * host recognizes device, but refuses to grant access
        * host unable to grant because it doesn't understand what you want

* example (virtual) devices
  * timers
  * network
  * storage: typical filesystem vs. something else?
  * speakers, mic, camera
  * keyboard, mouse, other input devices
  * display: terminal, canvas, html, gui widgets, etc.
  * gui sub-windows/frames: multiplexed access to many of the above devices
    * another level of virtualization

* persistent images
  * is full persistence worth it just for bootstrapping?
  * serialize snapshots as racket programs that resume from the snapshot point
    * or should this be in some other executable form? executable+data would be nice
  * what processes/state does an image actually persist?
    * all state and processes, except for real devices (or external virtual hardware)
      * real devices can't persist; their drivers involve external processes
    * internal virtual device states and driver processes can be persisted
      * image restart begins with a (boot) process that hooks them up to real devices

* evaluating expressions in a REPL subsumes many activities
  * manipulate data, operate devices, launch and manage concurrent processes
    * read in data, write out data (including taking image snapshots)
  * start a sub-REPL in a new environment via (enter!)
  * reboot into something that isn't the REPL: (exit! START-NEW-PROCESS)
  * halt as a special case of rebooting: exit! with a terminating process

* exammple text interfaces to try
  * rudimentary REPL using stdin/stdout
  * REPL with persistent state/history (less-style scrolling?)
  * terminal hijacked by ncurses-style UI (with mouse support)

* backend-racket platform integration
  * runtime compilation with option for immediate execution
    * foreign procedures (capabilities) for using Racket eval and namespaces
      * minimize the surface area; ideally something like: (racket-eval racket-sexpr)
  * ultimately, a platform should abstract away its native language for normal use
    * publicly provide just eval, not racket-eval directly
      * internally, it would compose the frontend with racket-eval


### control extensions

* implement tag-aware delimited continuation operators in base language
  * e.g., reset/tag, shift/tag, abort/tag

* delimited control:
  * programs not making use of delimited control should incur no additional overhead
  * for more precise resource control, replace shift/reset with: prompt0, control0, abort
    * should dynamic-put, dynamic-get be primitives for tail call efficient parameterize?
    * maybe exclude delimited control from base language, and implement via CPS-ing
      embedded interpreter; aggressive inlining can recover native stack-like efficiency
  * higher level continuation interface
    * support restartable error handlers (particularly important for REPL support)
    * aborting w/ specific tags
    * parameterize
    * dynamic-wind (but be wary of spurious exit/re-enter cycling
      * should forward dynamic parameters, to avoid exit/re-enter for parent lookup
        * what other kinds of aborts should/can be pre-empted by dynamic-wind in this way?
    * what does Racket really do?
      * exceptions/errors, break (and other interrupts)
        * asynchronous exceptions, like break, don't really make sense
          * Interrupts make more sense in terms of parallel processing, and should be
            designed for explicitly.  An interrupted program that does not explicitly
            ask to handle interrupts should terminate, not raise an exception in an
            arbitrary thread.
          * concepts like "break" belong to the meta-level, not in object-level programs
            * e.g., a main meta-level program (such as an IDE or debugger) is evaluating
              an object-level program when a "break" is signaled.  The interrupt handling
              behaves like a separate thread which communicates a break message to the
              main program thread.  The main program has been designed as an event loop
              that listens for interrupt messages such as "break", and responds by
              suspending its evaluation of the object-level program.

* parallel processing: spawn, (mvector-cas! mv i expected new) => boolean?
  * Is spawn necessary for equational reasoning with multiple processes?  If not, we can
    just expose a threading interface via procedures.  Would such an interface have to be
    platform-specific?  Racket efficiently supports interruption features that JS can't.
  * high level synchronizable actions interface
    * as in Concurrent ML, or Racket synchronizable events


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


### compiling/evaluating guest programs (program execution states as data)

* stage.scm syntax error checking that covers all corner cases and contains failure
  * could implement with reset/tag and shift/tag so that errors are catchable
  * could also embed errors within a result, providing more context for better feedback

* small-step evaluation (of AST) with transparent values
  * guarantees serializability of residual programs
  * supports transformations such as inlining, staged compilation

* flexible syntactic extension
  * parsers can safely produce code containing computed values
    * the computed values will be serializable, amenable to analysis

* virtualization
  * resource control
    * budget time and memory, throttle/suspend/resume/rewind/terminate
    * replicate and distribute
  * failure isolation: optional recovery/repair and resumption
  * incremental/adaptive computation
  * observe a program's internal state while it's running
  * modify a program while it's running
  * symbolic profiling, time-travel debugging, provenance
    * and other forms of analysis and immediate feedback
