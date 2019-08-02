# nScheme

## Test the bootstrap interpreter

`raco test *.rkt`

## Bootstrap the compiler and evaluator

Run `./clean-bootstrap`, or manually run:

```
racket bootstrap.rkt
raco exe -o compile compile.scm.rkt
./compile eval.scm
raco exe -o eval eval.scm.rkt
./eval -e "(printf '\"hello ~a\n\" 'world)"
```

## TODO
* store near-source language ASTs as virtualized programs
  * usual ASTs, but everything allocated in a VM heap
    * heap values are values annotated with a virtual address
      * no lookup needed in most cases (value is local)
      * address is useful for comparison, sharing, gc, serialization, modelling mutable state
    * ASTs themselves, and any values they embed (e.g., in ast:quote), are heap values
  * still CPS-convert if supporting delimited continuations and dynamic parameters
  * optionally optimize ASTs without changing representation
    * e.g., some variant of partial evaluation
  * implement resource-budgeted evaluation
    * optionally w/ provenance tracking for debugging
    * interrupted virtualized programs should be storeable
  * ast:io as the only way to interact with host; no leaking of procedures in either direction

* virtualized evaluation
  * host provides the s-expr datum representing a program and has the vm parse it
    * datum is "allocated" in the vm heap, optionally with metadata/provenance
    * datum is then parsed by the vm in some syntactic environment
      * i.e., parse is a vm-level program taking newly-allocated datum as input
      * in this way, env may refer to heap-allocated macros
        * which may embed heap-allocated values as program constants during expansion
          * e.g., quasiquote may embed references to `append` and `list->vector`
  * host can then eval the parse result/AST (also a vm heap value) using a
    safe/resource-budgeted/debuggable/etc. evaluator, with the vm heap as background
    * host can optionally optimize the AST before evaluating it
    * host could also extract the AST heap value, compile the AST, etc.
  * how does the vm's version of parse get installed in the first place?
    * host can run non-vm parse on itself to get its AST, then allocate/install it

* add ast:io
* add AST type general enough to cover dynamic state and delimited control
  * call it something like handle/dynamic/context?
* provide unreliable eq[v] primitive(s)
* provide n-ary vector constructor primitive (but can't lift it as a procedure)

* macro system alternatives
  * unusual systems
    * current system is similar to syntactic closures and "micros"
      * syntax may embed lambdas, which are anonymous parsers
    * staged syntax w/ usual lexical scope, requiring explicit environment management
    * explicit syntax-open/syntax-close
    * hygienic scope/binder static inference
  * standard scheme systems
    * mark/rename
    * implicit/explicit renaming
    * syntactic closures
    * macros-that-work
  * expansion-passing style

* pattern matching for tree structures
  * multiple surface syntaxes
    * quasiquote vs. syntax-rules vs. kitchen sink; guards vs. qualifiers
  * a single low level pattern language
    * should support literals, compound data, and/or/?/app, quasiquote, ellipses, etc.
  * multiple matching algorithms
    * general vs. fast for restricted patterns

* pattern matching for sequences (maybe later when the use is clearer?)
  * maybe limit to context free, aka mu-regular languages/grammars
  * recognition-only vs. accumulation/transformation
  * detecting and exploiting unambiguous grammars

* optional ast:io requests for alloc/free and execute (with operands) arbitrary host-platform code
  * to support dynamic compilation without interpretive overhead
  * need to design compatible calling conventions
  * valid code depends on platform, but interface can be wrapped as a generic dynamic eval

* ensure readable formatting of all code
* internable strings? internable pairs/vectors?
* sorted map (btree?); once there's a clear need

### AST simplification (optional?)
* generated Racket code is currently enormous, partly due to base library
* simple, code shrinking optimizations
  * ast augmentation (introduce astx:let, astx:begin) and normalization
    * `(apply (lambda P _) A) => (let ((p a) ...) _)` via (pbind P A)
      * (pbind P A) => (list (p a) ...)
        * P = (Q . R) & A = (cons B C)|(quote (B . C))
          * (append (pbind Q B) (pbind R C))
        * P = #(Q ..k) & A = (vector R ..k)|(quote #(R ..k))
          * (append (pbind Q R) ..k)
        * P = #f | v
          * (list (P A))
    * `(let (A ... (#f VALUE) B ...) _) => (let (A ... B ...) _)`
    * `(let ((#f X) _ ...) Y) => (begin X (let (_ ...) Y))`
    * lift lets while preserving effect order; e.g., let in let binding:
      `(let (X ... ((v (let (A ...) B))) Y ...) _)` =>  ;; assumes unique var names
      `(let (X ... A ...) (let ((v B) Y ...) _))`
  * context simplification
    * non-last begin context reduces to effects; non-effects eliminated
      * e.g., `(begin (cons (mvector-set! A ...) B) C ...)` =>
              `(begin (mvector-set! A ...) B C ...)`
          and `(begin VALUE _ ...)` => `(begin _ ...)`
    * unreferenced binders become #f; #f-bound values simplify to #t
      * same idea as begin's effect context; dead code elimination
      * try to infer #f-bound-ness across non-inlined procedure application
    * if-condition context reduces to truthiness; more dead code elimination
  * inline tiny procedures (i.e., single constant, variable, or primitive)
  * inline single-apply procedures
  * constant propagation/folding
  * be careful with set! variables
* orthogonal to elaboration

### AST elaboration before converting to SSA
* convert ast:letrec into uses of set! (other translations are possible)
* elaborate set! param matching
* replace set! with mvector-set! so that all variables are immutable
* replace dynamic var and delimited control operators via CPS transform
* elaborate lambda param matching; avoid quadratic growth of error message constants
* lift lambda and pass closure argument explicitly
  * locally bind free variable names as closure-refs
  * replace lambda expressions with closure construction

### Intermediate representation, analysis, transformation, execution

#### CPS-style SSA
Implement SSA as a sea of continuations, inspired by MLton and Guile Scheme.

```
atoms:
  primitive names: p
  variable ids:    v
  block labels:    l
  value ids:       id

SSA:
  identifier: I ::= (v  . I-provenance) | #f
  variable:   X ::= (v  . X-provenance)
  constant:   C ::= (id . C-provenance)
  expression: E ::= (values X ...) | (constants C ...) | (prim p X ...) | (call l X ...) | (apply X X)
  transfer:   T ::= (return E) | (jump E l) | (if v l l)
  block:      B ::= (b-provenance (I ...) T)

atomic constants: c
values: V ::= c | (cons id id) | (vector id ...) | (mvector length) | (closure l fv-count)

heap:
  blocks                = {(l  => B)        ...}
  values                = {(id => V)        ...}
  mvector/closure store = {(id => (id ...)) ...}
  provenance accumulators

env:   ENV ::= {(v => id) ...}
process: P ::= (stuck reason details) | (running id* returns=((l . ENV) ...))

system state:
  IO log      = ((action id ...) ...)
  global heap
  threads, possibly with local heaps
```

* streamline and add new primitives
  * direct vector construction via (prim vector X ...)
  * (closure label fv-slot-count) (closure-set! c i v) (closure-ref c i)
  * mvector-set! returns no values

#### Flow analyses
* support general flow analysis on a graph representation of SSA
  * preorder/postorder numbering
  * predecessor and immediate dominator links
* abstract interpretation
  * effects
    * identity-=: mvector=?, procedure=?
    * type-check: null? boolean? number? string? pair? vector? mvector? procedure?
    * (and (<= 0 I) (< I (vector-length  V)))
    * (and (<= 0 I) (< I (mvector-length M)))
    * (not (= 0 X))
    * (mvector-set! M I X)
    * (mvector-ref M I)
    * diverge
  * labels/procedures
    * static-call-count, closure-count, cont-count, abstract store/IO-log preconditions
  * bindings and references
    * abstract-value, ref-counts: call, mref, mset, identity-=, boolean, escape, use
    * availability and required lifetime
    * return
    * if-condition or argument
    * can these be subsumed by type constraints?
      * as procedure
      * impure: mvector-set!/mvector-ref
    * pure references
      * irrelevant argument to known procedure: e.g. X in ((lambda (#f y) y) X Y)
      * argument to type-testing predicate; use as if-condition
      * argument to procedure or constructor
      * type-specific
        * use as procedure
        * component access: car, cdr, vector-ref
        * arithmetic or other type-specific operator argument
    * escapes
      * different levels of escape
        * full: unknown uses of this value are possible
        * deterministic limited: value-only escapes via known code
        * nondeterministic limited: captured by a known thread
      * argument to unknown procedure
      * procedure return to unknown caller
      * transitive: embedding in escaping value as constructor argument or closure free var

#### Transformations
* remove unreachable blocks
* remove irrelevant mset!s  ; escape/lifetime, no subsequent mrefs
* remove irrelevant labels  ; no jump/call/closure/return-stack
* remove #f-ified bindings  ; pure i.e., no produced-effects
* #f-ify irrelevant vars    ; no refs
* simplify #f-ified sources
* simplify boolean-only sources
* simplify closures
* constant propagate/fold/flatten
* contify single-point returns
* worker/wrapper refactoring
* inline call
* CSE/tieback memoized      ; original label
* reorder blocks            ; commutative produced-effects according to before/after abstract-env
  * not sound wrt removed type-checks without consulting abstract-env
  * need to track abstract env to commute follow-up type-checks
  * might not be necessary for effective tiebacks
  * when supercompiling you'll often want to bring relevant bindings as close to the
    return expr as possible, making it easier to form tieback loops

#### Deciding what to specialize
* parameters: SSA node costs; inlining threshold; conditional/join specialization threshold
* when is a procedure "interesting" to inline?
  * when doing so has no downsides (program does not increase in size)
  * or, if there are downsides, when doing so is reasonably more efficient than not doing so
    efficiency comes from reducing/removing:
      * i/o operations (often hard to prove these are unnecessary)
      * allocation (cons, vector, mvector, closure)
        * unknown call overhead (allocating a structured argument)
        * dead code (code takes up space)
      * indirect data access (car/cdr, vector-ref, mvector-ref, closure-ref, spilled arguments)
        * for one datum, larger difference between 0 and 1 access than 1 and 2, due to caching
      * branching
        * conditionals
        * all call overhead
      * arithmetic operations
  * when it is known to be called exactly once
  * when it has a tiny body; what is tiny?
    * any number of constants or variable references
    * one primitive
      * no matter how small the call signature is
    * no larger than the call signature size
      * sum up the sizes of all primitives, if-statements, and procedure calls (known or unknown)
  * when interesting arguments, or their elements, are known; which ones?
    * known applied closure; even better if the application is interesting enough to inline
    * known cons/vector/mvector being taken apart
    * known numeric operands
    * revealing aggregates/closures that will be taken-apart/applied
    * arguments to a child procedure call that considers them interesting
    * known truthiness of if-statement conditions, which allows removal of dead code
      * particularly when eliminating possibility of bad behavior (e.g., escaping values)
    * interest is lower under if-statements and lambdas (lambdas may be worse)
      * interest can be recovered
        * knowing the truthiness of an if-statement's condition
        * knowing the outgoing lambda will be applied
  * when it's not too boring (too large in an uninteresting way); what is boring?
    * performing IO
    * constructing aggregates/closures that aren't known to be taken-apart/applied
    * calling unknown or boring procedures
  * when doing so won't potentially infinite loop
* when is a join point "interesting" to specialize?

#### Dynamic linking and virtualized execution
* compile SSA to RTL with lower level value/control representations
  * [de]allocate based on linearity/escape analysis
* heap linking
* system state image serialization

### Program virtualization
* program representation
  * global and thread-local heaps
    * transparent values; procedures retain linkable code
    * procedure heap is effectively a giant letrec
  * additional gc roots; migration
  * live threads
    * thread-local heap; cached global heap
      * escaping values/mutations committed to global heap upon sync/preemption
      * thread allocates from a disjoint chunk of heap ids
        * exhaustion forces preemption to obtain a new chunk
    * code can directly reference heap values; allocates ids from same namespace
    * how do they coordinate heap changes? similar to linking?
    * resource-budgeted small-step evaluation of any subset of threads
      * resource control
        * budget time and memory, throttle/suspend/resume/rewind/terminate
        * replicate and distribute
      * failure isolation: optional recovery/repair and resumption
      * incremental/adaptive computation
      * observe a program's internal state while it's running
      * modify a program while it's running
      * symbolic profiling, time-travel debugging, provenance
        * and other forms of analysis and immediate feedback
* how does external state/communication work?
  * what interface with host/io?
* incremental, dynamic compilation and linking with adaptive evaluation
  * can link multiple program states
* persisting program states as images
  * serialize snapshots as (racket) programs that resume from the snapshot point
    * or should this be in some other executable form? executable+data would be nice
  * what processes/state does an image actually persist?
    * all state and processes, except for real devices (or external virtual hardware)
      * real devices can't persist; their drivers involve external processes
    * internal virtual device states and driver processes can be persisted
      * image restart begins with a (boot) process that hooks them up to real devices
* safe compiling of guest programs from source
  * parse.scm syntax error checking that covers all corner cases and contains failure
    * could implement with reset/tag and shift/tag so that errors are catchable
    * could also embed errors within a result, providing more context for better feedback

### Racket platform
* io.rkt capabilities:
  * stdin/stdout/stderr, stty, filesystem, network sockets, gui
  * threads/places/futures, timers, exception/break/interrupt handling
    * uncaught-exception-handler, call-with-exception-handler, exn:break?
  * cmdline/shell/env/subprocesses, racket-eval
* define ports, read, write
  * start with high level ops for now, then reimplement in terms of lower level ops
    * open-input-file, open-output-file, close-input-port, close-output-port, flush-output
    * high level: eof?, printf, write-string, write, read-string, read
    * low level: peek-bytes!, read-bytes!, write-bytes, file-position (via `*` version for safety)
      * store bytes in mvectors; support consolidation into vector of unicode chars
  * include non-device (aka string/byte buffer) ports
    * for when you want a port interface to string-like data
* backend-racket integration
  * runtime compilation with option for immediate execution
    * foreign procedures (capabilities) for using Racket eval and namespaces
      * minimize the surface area; ideally something like: (racket-eval racket-sexpr)
  * ultimately, a platform should abstract away its native language for normal use
    * publicly provide just eval, not racket-eval directly
      * internally, it would compose the frontend with racket-eval
* (virtual) hardware capabilities
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
* evaluating expressions in a REPL subsumes many activities
  * manipulate data, operate devices, launch and manage concurrent processes
    * read in data, write out data (including taking image snapshots)
  * start a sub-REPL in a new environment via (enter!)
  * reboot into something that isn't the REPL: (exit! START-NEW-PROCESS)
  * halt as a special case of rebooting: exit! with a terminating process
* example text interfaces to try
  * rudimentary REPL using stdin/stdout
  * REPL with persistent state/history (less-style scrolling?)
  * terminal hijacked by ncurses-style UI (with mouse support)

### Reorganize
* reorganize lib/
  * README.md
  * link.scm
  * test.scm
  * modules/
  * eventually cache results?: linked.sdata or linked.bdata

### backend for web/javascript
Define a target AST (micro-js) for javascript-specific simplification/optimization.
* target ASTs will be converted to strings for output or processing by JS `Function`
* data: undefined, null, boolean, number, string, array, object, function; var, ref, set!
* control: if, labelled while, apply, return; labelled continue/break
* consult this list: https://github.com/anko/eslisp/blob/master/doc/basics-reference.markdown#built-in-macros

### Web browser platform
Wrap web features as capabilities and build a runtime system in nScheme.
* e.g., console.log, event handling, DOM manipulation, webRTC, storage, etc.
  * maybe present an interface like SDL or pygame
* Do a shallow wrapping of capabilities in micro-js, and pass them to nscheme
  * Could simplify this with a higher level notation (mini-js) compiling to micro-js
    * if, switch, for, while, do/while, apply, return, continue/break, labels
    * few fancy transformations; maybe for lexical scope; no proper tail calls
    * but must be able to safely interoperate with nScheme
      * create and apply nScheme procedures; manipulate mvectors and other data

### Host system interaction patterns
Real-world (effect) interaction/reaction:
* monolithic, hierarchical, purely functional request handling
  * value: (list 'value val)
  * request: (list 'request payload continuation)
  * continuation: -> (lambda (response) ...)
  * eval: program -> value|request
  * handle: value|request -> ...
* independent processes communicating via shared mvectors
  * arbitrary topology; can support true paralllelism
  * can reason about host system as a concurrent, black box process

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
  * lambda/match, match
  * lambda-syntax, let[rec]-syntax, define-syntax, define-syntax/define
* definition context conveniences
  * embedded procedures in a definition context that take st as argument
    * #(define ,(lambda (st) ...))
  * ($define (lambda (st) ...)) to directly access state without embedded procs
  * property manipulation shorthand
    * (describe identifier ((property-name value) ...))
    * `(describe* (id*) kvs)`
    * e.g., (describe var ((set! #f))) to hide the capability to set! a variable
  * @definitions-run to force accumulated actions to run, and reset defined names
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

### representation type system
* an alternative design for structural types (inconsistent, needs some cleanup)
  * unit, atom, (dependent sum?) product (n-ary shorthand), sum, fix, fixvar, pointers?
  * atom      A ::= unit | (float ...) | (int ... numeric range/set ...) | (pointer T)
  * mutable?  M ::= A | (mutable A)
  * product   P ::= M | (struct M P) | (exists (n ...). P) | (array N P P)  ;; array of N repetitions of P followed by P
  * recursive R ::= S | (fix x. S) | (fixvar x)
  * sum       S ::= P | (case N (N P) ...)
  * sealed/nominal? witness tag, existential, ...
  * procedure F ::= (-> ...) | (forall n. F) | sealer/unsealer(must be given an underlying type)?
  * ptrtarget T ::= F | R
  * nat       N ::= ... arithmetic expressions ...
  * convenient let expressions and corresponding names/variables, struct-append, etc? or have this be metalevel?
  * example:
    (fix scheme-object. (union (exists n. (int60 n))
                               (*8 (struct scheme-object scheme-object))
                               (*8 (exists n. (struct n (array n scheme-object unit))))
                               (*8 (exists n. (struct n (array n (mutable scheme-object) unit))))
                               (exists c.
                                (fix closure. (*8 (struct (-> (closure scheme-object) (scheme-object))
                                                         (array c (mutable scheme-object) unit)))))
                               (* sealed)))
    * replace union with case mappings (nat-indexed dependent types)
  * name attributes, allow nested structs, allow mutable products as a shorthand for product of mutables?
  * linear types for initialization mutation of to-be-immutable values
  * allocation, initialization, and stack frames
    * shared vs. unique pointers
      * shared mutation must preserve type
      * unique mutation may change type, but may not be copied; similar to registers
  * How are garbage collectable pointers indicated?  Or is everything implicitly gc-able?

* an intermediate representation, but also a lower level user notation
* aliasing/uniqueness annotations
  * e.g., mvector->vector need not copy if the mvector was unique and is no longer used
  * single/multi-threaded aliasing constraints can inform mvector optimizations
    * along with aliasing, infer thread-local and uncontended mvector accesses
* escape/local annotations
  * closures need not be allocated when only used within parent's stack lifetime
  * stack allocations for data with appropriate lifetimes
* maybe some simple effect annotations on procedures
  * parameter aliasing (hoarding away shared references to parameters, increasing their refcount)
  * resource usage:
    * heap/stack allocation
      * heap regions created and/or allocated into, if applicable
      * whether the allocation is temporary (should be for stack) or lasting
        * is this subsumed by region info?
    * termination, time bounds
  * use of shift/abort, set!, mvector-set!
* mix high level and C-like types (could optionally allow unsafe coercion)
  * unknown (only valid when runtime type info is available, e.g. vector element type)
  * tagged (the default dynamic type of tagged pointers and small constants)
    * const: #t #f () char fixnum [u]int8 [u]int16 [u]int32
    * flonum
    * number: bigint rational complex [u]int64
    * string/symbol
    * pair {T T}
    * [m]vector {(struct T ...) | (array N T)}
    * procedure {(T ... [. T]) -> E T} where E is an effect annotation
  * ints, floats, structs, arrays, procedures
  * (struct) is Top, (struct T) :=: T, (struct A B) :<: (struct A)
    * intuitive conversions with array types
  * addresses/pointers
    * (pointer-to T)        ;; typical pointer
    * (pointer-between T T) ;; pointer that knows about data before it
      * (pointer-between A B) is a subtype of (pointer-to B)
* could go beyond C-like types: memory map types that describe hierarchical context
  * individual data structures within the context of larger memory (sub)pages
    * parent alignment allows calculation of parent addresses from child addresses
  * both static and dynamic offsets within a parent context
    * . (dot) or [N] (where N is a literal) is sugar for a static offset
  * AND(&&) and OR(|) types/constraints: OR for something like unions, AND for composing contexts
    * value-dependent constraints
      * for determining sizes, positions, and/or which OR branch is relevant
      * e.g., a tagged pointer where the tag describes what the pointer points to:
        * example-pointer-union = [bits[61]|0b001] && p1-ptr-type | [bits[61]|0b002] && p2-ptr-type
          * would also describe calculation of actual address, masking lower 3 bits
      * length-encoded arrays:
        * dynamic-array = &[N|any[N]]
      * endpoint-encoded arrays:
        * dynamic-array = (word P) && &[end|any[N]] && (N == (end - P) / (type-size any))
  * full control over all memory management, models, and algorithms
    * enough descriptive power to optionally verify implementations
    * e.g., able to describe data in the context of garbage collection metadata
      * page-type = [prev-page|next-page|type-info|gc-marks[N]|words[N]]
      * pointer to datum of type {struct T U}:
        * ptr-type = (word A)
                  && &(page-type.words[N])
                  && &{struct T U}
                  && (N == (A - (address->page-address A)) / (type-size type-info))
        * can define copying/mark-sweep/mark-copy/generational/etc. gc algorithms
      * pointer to pool-allocated datum:
        * pool-page-type = [type-info|allocation-info[N]|obj[N]]
        * pool-ptr-type = (word A)
                        && &obj
                        && &(pool-page-type.obj[N])
                        && (N == (A - (address->pool-page-address A)) / (type-size type-info))
        * can define non-fragmenting pool allocation for same-sized objs
      * size-dependent allocation decisions:
        * maybe-large-array = &[N|any[N]]
                            && ((N >= threshold && &([prev|next|N|any[N]][2]))
                              |(N < threshold  && normal-gc-page-allocated-array)
    * also e.g., allowing call stack layouts to be described explicitly
      * plain example (^ denotes frame pointer) for stack frames [0,1,2]:
        * frame-entry-2     = [^return1|a1|a2|...]
        * frame-returning-2 = [r1|r2|...|^return1|a1|a2|...]
        * frame-returned-1  = [^return0|b1|b2|...|r1|r2|...]
      * explicit frame-specific gc handlers example:
        * frame-entry-2     = [handle-gc1|^return1|a1|a2|...|handle-gc2]
        * frame-entry-1     = [handle-gc0|^return0|b1|b2|...|handle-gc1]
        * could instead map return addresses directly to gc handlers, saving a little space
        * could instead decorate frames with pointer maps and define a single gc handler
      * can define segmented, spaghetti, cactus, etc. stacks
        * can define continuation capturing and resumption
* motivations and example:
  * immediate unboxing, e.g., operating directly on unboxed flonums
  * vector element unboxing
    * vector{flonum} can be represented more efficiently than vector{tagged}
  * statically-known vector-ref access logic
    * vectors store element type tag along with arity at runtime, to inform vector-ref
    * so e.g., a vector{flonum} can still be accessed safely by type-agnostic vector-ref
    * vector-ref can be more efficient when the element type is known
      * even when the type is 'tagged', at least the stored type tag check can be elided
* eventually at the RTL level, would like to define procedures with register pre/post conditions

### other backends to consider
WebAssembly, Python, C, Java, .NET, Erlang, LLVM, x86, ARM, FPGA, ...

### other platforms to consider
Electron, NW.js/Node.js, Python, POSIX, Java, .NET, Erlang, mobile etc., bare metal, ...

### alternative semantics
Support other forms of evaluation using the same syntax:
* functional logic programming
* probabilistic programming
* automatic differentiation

### temporal relational programming
* checked assertions
  * (== x y) :- (p w x) (p w y)
    * w is a unique key
  * (< J K) :- (file-parent J K)
    * filesystems are acyclic
  * (assert (q X)) :- (p X)
    * model checking
* transformation
  * not limited to queries
  * identify smallest set of referenced values per predicate column
    * partition predicate into new predicates when these sets are not Top?
  * negations with ground can sometimes be used recursively: transformation to stratifiable program?
* constructors/functions/aggregations can be used non-recursively
  * sometimes recursively when acyclic
* some infinite relations can be queried when ground:
  * e.g., p(X) :- not q(X); ?p(1)
  * successor/choice; temporal (aka acyclic) stratification
* stating/proving monotonicity
  * idempotent, commutative, associative, possibly between different aggregators
* arbitrary lattices w/ group-by
* <, <=, =/= constraints
* solvers
  * numeric constraint classification: linear, nonlinear, unclassified
* open/closed world caveats
* low level: sccs, fixed-point, relations, join, filter, project, union, difference, aggregate
* evaluation
  * naive, as simple as possible
  * semi-naive; scc dag
  * indexing; join plans; maybe try worst-case optimal joins
    * https://github.com/frankmcsherry/blog/blob/master/posts/2015-04-11.md
* concrete vs. variable join
  * how do you join infinite (but constrained) relations?
    * i.e., constrained variables are mentioned
    * lattice-join the constraints (conservatively)
    * apply those when filtering other (finite) relations for their joins
    * then finally, when joining with the infinite relation, apply the constraints accurately
* possible names: tKanren, underlog, icarus, wilt

### computational logic, building proofs
* proof-checking decision procedure: (proof? proposition candidate-proof) : Boolean
* basic props and inference rules include some minimal mix of: `->, ->*, |->, |->*, ~=`, term induction
  * bracketing (see Boyer-Moore translations) needs reformulation as a meta-level operator
    * requires everything to be quoted another level
    * decision procedures all the way down until term induction
  * term induction
    * a special case of universal quantifier proof (all t:Term. P(t))
      * doesn't require decision procedure for quantifier bounds checking
      * inference rule requires a way to hypothesize smaller induction proofs
  * can BHK interpretation ideas be used to manipulate Boyer-Moore style proofs?
    * BHK versions shown for comparison below are not the same structures as the Boyer-Moore proofs
  * Boyer-Moore style proofs:
```
      p. proof of #"A and B" where
        (proof? #"(and (proof? A ...) (proof? B ...)) ->* #t" p)
        BHK: p = (pair a b) where #"(proof? A a) ->* #t" and #"(proof? B b) ->* #t"
      p. proof of #"A or B" where
        (proof? #"(or (proof? A ...) (proof? B ...)) ->* #t" p)
        BHK: p = (pair 0 a) where #"(proof? A a) ->* #t"
               | (pair 1 b) where #"(proof? B b) ->* #t"
      p. proof of #"all x : S. P(x)" where
        (proof? #"(lam (x)
                    (if (equal? (bracket #t)
                                (step-complete #"(S #,(bracket x))"))
                      (proof? #"P(#,(bracket x))" ... x ...)
                      #t)) ->* (lam (x) #t)"
                p)
        note the bracket operation, which can only exist as a meta-level operation
        extra level of quotation/bracketing necessary to verify well-definedness via step-complete
          if checking "x is an S" gets stuck or infinite loops then all bets are off
            so S pretty much has to be a decision procedure because step-complete is *not*
            bounds checking analogous to type/kind checking
        BHK: p is a dependent product of the same form as described by the proven formula
      p. proof of #"A -> B" where
        (proof? #"(lam (a)
                    (if (equal? (bracket #t)
                                (step-complete #"(proof? A #,(bracket a))"))
                      (proof? B ... a ...)
                      #t)) ->* (lam (a) #t)"
                p)
        It can be seen that #"A -> B" = #"all a:(proof? A). B"
          implies the right BHK interpretation
      p. proof of #"not A" where
        (proof? #"(lam (x)
                    (if (equal? (bracket #t)
                                (step-complete #"(proof? A #,(bracket x))"))
                      'False'
                      #t)) ->* (lam (x) #t)"
                p)
        can be seen as: all x:(proof? A). 'False'
        BHK: substitute any prop 'False' you'd like; if handed a (proof? A), suddenly you've proven 'False'
```

### certified programming
* entirely optional and available at any sub-program granularity
  * no obligation to statically analyze or type-check programs before running
  * selectively analyze/certify/transform important parts of your program later on
* general framework for building reasoning/transformation tools
  * dictates inference rules describing operational semantics
  * everything else should be derivable
* analyses
  * memory safety
  * termination checking
  * effect analysis
  * information flow analysis
  * control flow analysis
  * type checking
  * model checking
* correctness-preserving transformations
  * refactoring
  * optimization, e.g. supercompilation
  * model concrete machines and operate abstract machines
    * simulation
    * compilation
    * analysis and optimization
      * resource analysis and management
        * e.g. region-based memory management: like compile-time garbage collection
      * symbolic profiling
* automated program elaboration
  * inference of types, terms, even tests
* proof-carrying code

### hyperprogramming


## Static approach to fexprs
* referring to this approach as "static"; referring to Kernel's fexprs as "dynamic"
  * in Kernel, applicative behavior is a dynamic property of values via wrap/unwrap
  * in this approach, applicative behavior is a static property of bound names
    * syntactically apparent, determined by how a name is bound
      * e.g., given the form `(f . X)` where `f` is a symbol
        * `f` is bound as operative: apply `f` to `current-env` and literal `X`
        * `f` is bound as applicative: apply `f` to list of evaluated elements of `X`
      * non-symbol heads are treated as applicative
    * any procedure may behave as an applicative or operative without modification
      * all procedures are lambdas without caveats or additional feature complexity
      * e.g., `(apply lambda an-env-that-binds-+ '(x) (list (list '+ 'x n)))`
        dynamically produces a `(lambda (x) (+ x n))` for some runtime value `n`
    * seems both more convenient and easier to reason about than applicative wrappers
    * seems just as expressive as the dynamic approach
* static approach simplifies encapsulation where desired
  * e.g., in the dynamic approach, an operative (called `f`) passed to a higher order
    procedure (such as `map`) will observe the procedure's implementation details; to
    prevent this, `map` would have to runtime-assert that `f` is wrapped (applicative)
  * in the static approach, `map` can bind `f` as applicative, guaranteeing
    the security of its source code without additional checks


## References by topic

### De Bruijn indices and explicit substitutions
* [Wikipedia: De Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index)
* [How I learned to stop worrying and love de Bruijn indices](http://disciple-devel.blogspot.ca/2011/08/how-i-learned-to-stop-worrying-and-love.html)
* [Wikipedia: Explicit substitution](https://en.wikipedia.org/wiki/Explicit_substitution)
* Blitz introduction to de Bruijn indices and explicit substitution: [How to implement dependent type theory III](http://math.andrej.com/2012/11/29/how-to-implement-dependent-type-theory-iii/)

### Reduction systems
* [Explicit evaluation](http://fexpr.blogspot.ca/2013/07/explicit-evaluation.html)
* [Continuations and term-rewriting calculi](http://fexpr.blogspot.ca/2014/03/continuations-and-term-rewriting-calculi.html)
* Part II of [Fexprs as the basis of Lisp function application; or, $vau: the ultimate abstraction](https://www.wpi.edu/Pubs/ETD/Available/etd-090110-124904/unrestricted/jshutt.pdf)

### Milawa proof checker implementation
* https://www.cs.utexas.edu/users/jared/milawa/Documentation/defense.pdf
* https://www.cs.utexas.edu/users/jared/milawa/Documentation/dissertation.pdf
* https://www.cs.utexas.edu/users/jared/publications/2015-jar-milawa.pdf
  * terms T; formulas F ::= T=T | not F | F or F
  * appeal (proof step) { method (rule name, such as 'axiom, 'theorem (something already proven), 'expansion (A or B), etc.), conclusion, subproofs, extras }
    * restructure appeal as variant, one alternative per inference rule
  * consider clauses instead of sequents; consider proofs with multiple conclusions

### Low-level verification
* http://plv.csail.mit.edu/bedrock/
* http://adam.chlipala.net/papers/BedrockPOPL15/BedrockPOPL15.pdf

### Theory of changes
* http://www.umut-acar.org/self-adjusting-computation
* http://ttic.uchicago.edu/~pl/sa-sml/
* http://lambda-the-ultimate.org/node/5115

### Live programming
* processes (discrete position updates per time step) vs. equational continuity (motion equations as function of time as if it's yet another spatial dimension on the canvas)
* http://research.microsoft.com/en-us/um/people/smcdirm/apx/
* http://research.microsoft.com/en-us/people/smcdirm/managedtime.aspx
* http://gbracha.blogspot.ca/2008/07/debugging-visual-metaphors.html

### Document-based computing
* https://www.luna-lang.org/
* https://github.com/zot/Leisure
* http://www.quantrix.com/en/
  * https://www.youtube.com/watch?v=LDKI8eeLf2M

### Relational programming
* http://scattered-thoughts.net/blog/2016/10/11/a-practical-relational-query-compiler-in-500-lines/
* http://scattered-thoughts.net/blog/2017/07/28/relational-ui/

### Related work
* [user actions as metaprogramming](https://groups.google.com/forum/#!topic/augmented-programming/gazxhLLXscQ)
* http://bracha.org/objectsAsSoftwareServices.pdf
* https://www.cl.cam.ac.uk/~afb21/CognitiveDimensions/CDtutorial.pdf
* https://en.wikipedia.org/wiki/AgentSheets
* http://openendedgroup.com/field/StandardLibrary.html
* https://web.archive.org/web/20070927190552/http://users.ipa.net/~dwighth/smalltalk/Fabrik/Fabrik.html
* http://arxiv.org/abs/0710.2358
* http://cs.brown.edu/~spr/research/env.html
* https://github.com/bloom-lang/bud/blob/master/docs/operational.md
* http://www.jgrasp.org/
* http://foswiki.cs.uu.nl/foswiki/Proxima/WebHome
* http://www.alice.org/index.php
