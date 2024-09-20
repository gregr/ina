# nScheme

nScheme is a low-tech, nonstandard Scheme implementation designed for
programming portable, flexible systems.

Compared with standard Scheme, the language has been simplified in many ways.
This simplification makes it easier to reason about control and data flow, and
also facilitates a simpler, more compact implementation that can be
straightforwardly ported to new platforms.

Unlike standard Scheme, nScheme does not prescribe any particular form of
program organization or approach to metaprogramming.  Where standard Scheme
implementations often assume the use of a complex library or module system and
ahead-of-time compilation model, nScheme instead encourages constructing,
combining, and evaluating programs at any time.

Beyond basic language support, an nScheme implementation will also inherit
primitive capabilities specific to its platform.  The primitives provided by
some platforms will include support for low level resource control and
virtualization, allowing a programmer to decide how a system works at every
level.  Some platforms will provide reflection primitives that enable saving
and resuming snapshots of a running system.

## Deviations from Scheme

- Some conventions from Racket are adopted:
  - Order of evaluation is left-to-right
    - e.g., in a procedure application, first the operator is evaluated, then
      the operands are evaluated in the order they appear, from left to right.
  - Internal definitions may be interleaved with expressions
  - `letrec` is implemented as `letrec*`
- By default, all definition scopes behave the same way.  No distinct top-level is specified.
  However, code-processing tools, such as REPLs, are free to choose alternative, ad-hoc behaviors
  that better suit their purpose.  Anything is possible.
- Definition scope construction is more specified than in Scheme: fewer programs are incorrect
  - Definition scope is constructed iteratively with each definition operator being parsed according
    to the identifiers known at the time, even if those identifiers are shadowed later in the same
    definition scope.
  - However, as in Scheme, an expression on the right-hand-side of a definition is not parsed until
    the complete definition scope is available.
  - Although not specified this way in Scheme, this behavior happens to be consistent with typical
    Scheme implementations.
    - Likely, this is because this behavior permits definition parsing that is efficient.  Detecting
      violations of the Scheme standard on this issue would require multiple passes with complex
      bookkeeping.
  - This behavior also allows us to introduce a shadowing identifier that may build off of the
    previous binding information of the same identifier.
- Expressions that would typically return a single `(void)` value (like `set!`, `when`, etc.),
  instead return 0 values (i.e., `(values)`).
- `(apply/values consumer produce)` is primitive syntax where `produce` is an ordinary expression.
  - `(call-with-values producer consumer)` is defined as `(apply/values consumer (producer))`.
- Variables are immutable by default.
  - `set!` is only supported for variables introduced by special binders.
- All s-expression types are immutable.
  - This even includes pairs, strings, bytevectors, and vectors.
  - Mutable vectors and bytevectors (i.e., mvectors and mbytevectors) are a distinct type of data,
    disjoint from the s-expression types.
  - Records are distinct, immutable, vector-like types.  They are disjoint from the s-expression
    types.
- Unlike Scheme, record type descriptors (RTDs) are normal vectors with the following layout:
  - `(vector field-count optional-user-defined-metadata ...)`
  - The field count always comes first so that the system (particularly the garbage collector) can
    find it easily.
  - It can also include user-specified metadata.  For instance, this could be used to list parent
    types for implementing some variant of inheritance.  It could also be used to list field names
    for dynamic field lookup.
- Equality and identity:
  - All values are `eq?` to themselves.
  - Newly-constructed mvectors and mbytevectors, even empty ones, are guaranteed to have unique
    identities according to `eq?`.
  - Structurally equal `()`, `#t`, `#f`, and symbols, are always `eq?`.
  - `eqv?` returns `#t` for structurally equal numbers, strings, and bytevectors, and in every case
    that `eq?` returns `#t`.
  - `equal?` returns `#t` for all structurally equal values of all s-expression types.  That is,
    structurally equal, immutable, non-procedure, non-record values.  For all procedures, records,
    mbytevectors, and mvectors, even empty ones, `equal?` returns the same result as `eq?`.
  - Any structurally equal, immutable values are allowed to share the same (immediate or allocated)
    object, even if they were constructed at different times.  This means that newly-constructed
    pairs and vectors are not guaranteed to have unique identities according to `eq?`.
  - Procedures that have indistinguishable application behavior may share the same object, even
    if they appear to close over mutable values.  (An optimizer may recognize that a closed-over
    mutable value does not impact the procedure's behavior.)
  - Note that evaluating a `quote` expression does not construct a new object.  A `quote` expression
    evaluates to the object that is contained in the expression itself.
    - Every `quote` expression is itself a value that must be allocated and constructed
      programmatically, containing another value, before it is evaluated.  That contained value must
      also either be an immediate object, or have been allocated before the `quote` expression is
      evaluated.  For instance, `read` will build such `quote` expressions, performing any
      allocations necessary to do so.  And so the values returned by evaluating any particular
      `quote` expression will always be the exact same object.
    - So each evaluation of the same `(quote #(123))` expression returns the same vector object.
      - Different `(quote #(123))` expressions may return different vector objects.  It depends
        on whether or not the contained vector is the same allocated object.
- No primitive character type: a string is indivisible until it is decoded as a bytevector
  - e.g., `string->utf8` and `utf8->string`
  - no `string-ref`, since basis of decomposition depends on context
  - code units (bytes) are not characters, but suffice for low-level string manipulation
  - code points are not characters, but suffice for some higher-level string manipulation
  - grapheme clusters (substrings) are the right notion of character for user interaction
- No primitive eof-object type: IO operations may return `#f` or `(values)` instead
- Inspired by: https://www.deinprogramm.de/sperber/papers/numerical-tower.pdf
  - Inexact numbers are not s-expressions, and are not part of the base language.  But platforms
    can provide primitives for floating-point data and arithmetic when they support it.
    - All numeric literals describe exact numbers by default, for reproducibility and portability.
  - Complex numbers are also not built in.
  - Exact/inexact arithmetic that emphasizes "exactness" on operators rather than values
  - The usual numeric operators will only be applicable to exact numbers.
    - A separate set of operators would need to be provided for manipulating inexact numbers.
  - `integer?` `rational?` etc. must return `#f` for all inexact numbers.
- Optional decimal representation for fractions, with repeating portion prefixed by ~
  - e.g., 0.~3 for 1/3, 0.~17 for 17/99, etc.
- There are no operators that directly capture continuations.  First-class coroutines manipulate
  disjoint call stacks, and avoid copying frames.  They have the same expressiveness as one-shot
  delimited continuations.
- There is no prescribed object-level error handling model.
  - You can define your own handling using coroutines and dynamically-scoped parameters.

## Run tests and bootstrap

Eventually:

`time racket bootstrap/test.rkt`

`time racket bootstrap/bootstrap.rkt`

## TODO

- cross-phase persistence/serialization transformation of E
  - memoizing shared E-quote values
    - replace non-procedure E-quotes with E:refs to shared constructor expressions
      - or directly to contructor expressions if used only once
      - or leave them as-is for small values that don't need to be shared
  - (procedure) values that cannot be written
    - transformation is parameterized by a lookup procedure
    - may replace a procedure (or other data as well) with a global E:ref
    - may replace a procedure with E:lambda when code and values of free variables are available
      - if lookup is backed by a capability such as procedure-metadata, for instance
    - otherwise, this transformation may fail in some standard way
  - useful for all platforms, so should be located in include/compiler/, probably in high-level-ir.scm
  - eq-hash-code ? eq-unreliable-hash-code (in JS use a Map or WeakMap?)
    - for amortized constant time hash lookup
    - we need something like this to solve the E:quote sharing analysis problem
      - because two different E:quotes might reference the exact same allocated object, and this needs to be preserved during compilation
    - alternatively, we can use a mostly-content-based hash code built as we analyze values bottom-up
      - we can also avoid lookups for parent values as soon as one child is not found in our table
        - we still have to insert them into the table, however, since they are newly seen

- maybe centralized system failures (platform/system-failure.scm?) which are used by stream io?
  - because we want to prevent accidentally ignoring
  - must guarantee no return by panicking if a handler for this does return (and by default if handler isn't set)
  - should we have low-level IO ops returning a status code rather than raising an exception after all?
    - at least partly to avoid allocation in an environment without memory management
    - this avoidance of allocation is only useful if there are no other forms of allocation at that
      level of IO (e.g., no buffers), which may not be realistic


```
high-level-ir.scm
;; TODO: just replace E:prim with uses of E:ref instead?
;; - if we do that, how will we recognize when we can optimize a primitive operation?
;; - maybe the primitive info can be installed in the environment
;;   - so, when we look up a ref, we either get concrete primitive info we can apply
;;     or we just get an indicator for a fully abstract variable that we can't do much with
;;   - we might be able to do other kinds of constant folding/inlining through refs this way too
;; TODO: E:let E:begin, and probably also E:let-values for the same reason

meta.scm
;; TODO: let-syntax let*-syntax letrec*-syntax letrec*-syntax&values ?
;; - splicing forms?
```

What is the next step?  We want to complete a Racket platform bootstrap.  We still need:
- include/compiler/backend/rkt.scm
  - module construction, literal code construction, and linking
- cross-compiler and primitive implementations: include/platform/posix/rkt.scm
  - build env.primitive.whatever with fresh primitive addresses, mapping each to a parser that produces its (E:ref addr)
  - build a map of `prim-addr=>id` (the usual Racket ids for each) that we can pass to E-compile-rkt
  - Racket platform stdio, file, and tcp streams, compatible with text/stream.scm interface
    - implementation for these platform-specific streams should be generated as literal Racket code, not nScheme code
      - we manually link to it during bootstrap, but also automatically link to it when compiling to posix Racket
    - expose only a limited set of IO primitives to nScheme
      - e.g., just a high-level interface for using filesystem, network, console
      - no need for lower level primitives, since high-level controllers can produce other encapsulated controllers
- provide high level control and io primitives from Racket
- include/text/read.scm and include/text/write.scm
  - initially test using bytevector ports
  - then try with ports backed by racket primitive io streams
- bootstrap/clean-bootstrap


include/text/port.scm and include/text/stream.scm may belong in include/io/* instead?  not sure
- maybe read.scm and write.scm belong there too


proper fraction notation
- more generally, allow any expanded addition/subtraction as input, though we will only ever output one proper or improper fraction

limit preference for decimal notation to radix-powered denominators only


list.scm
;; TODO: maybe specialize 2-list argument variants of for-each, map, and fold to avoid apply

(make-equal? super)
(define equal? (make-equal? (lambda (a b) #f)))
- alternative to providing super, provide an association list of type-predicate => type-destructurer (or type-component-iterator)
- though this is less general as far as the forms of equality that can be encoded (e.g., set equality would be challenging to do this way)

define a (make-write super) in the same way
- `#<unknown>` for opaque values without a generally-known predicate, such as records, unless overridden by super


start adopting a generic type-identification strategy across OOP-style procedures
- examples:
  - io streams (therefore affecting ports)
  - envs
  - programs from nscheme/program.scm
- the method-taking lambda is actually a case-lambda that can take 0 args, and in
  that case returns some (ideally human-readable) type identifier and possibly other descriptive info?

what is the right thread-safe stdio api?
- don't worry about thread-safety at the port level
- instead, come up with a higher-level concurrency-oriented io system
  - common logging procedures, like displayln or something analogous, should use this by default
    - they should implement their own line-based flushing (we don't support line-buffering in ports)
  - "hello world" should be straightforward
- alternatively, we can put optional thread-safety into some io streams, and make it easy to swap out the ports that wrap them
  - so dangerous threads don't corrupt your ports


bootstrap/include.rkt should build a database out of include/: an association-list-tree of directories, files, and their contents
- can chop off the .scm suffix? but maybe don't
- need to be able to distinguish directories from files
- then we will provide this database as an embedded constant to the nscheme program to use when running client programs
  - to build envs/programs by linking


top-level build/ and prebuilt/ for temporary and checked-in build results

top-level test/ directory

top-level bootstrap/ directory
- a (prebuilt or hand-written) nscheme.rkt program:
  - usage: racket nscheme.rkt program.scm program-args ...
  - or require it as a module
- test.rkt and/or test script
- clean-bootstrap script, which will clear out bootstrap/build/ and then re-populate it
  - run nscheme.rkt on cross-compilers for all platforms, compiling nscheme.scm for each, and place the resulting nscheme.X files in bootstrap/build/
    - running cross-compilers on nscheme.scm should itself be a shared program, like bootstrap.scm
    - so this means bootstrapping will run nscheme.rkt on bootstrap.scm

top-level include/ directory
- base/
  - misc.scm
  - record.scm
  - exception.scm
  - unicode.scm
  - etc.
- syntax.scm  ; this doesn't have to be specific to the nscheme language, and doesn't really belong anywhere else?
- text/
  - stream.scm
  - port.scm
    - a general form that doesn't depend on posix or files
    - an implementation of string/bytevector ports too
  - read.scm   ; parameterize over an annotator so we don't depend on syntax.scm
  - write.scm  ; parameterize over a styler so we don't depend on syntax.scm
  - terminal-control.scm
- codegen/
  - c.scm
  - js.scm
  - html.scm
  - css.scm
  - py.scm
  - x86-64.scm
  - wasm.scm
  - lua.scm
- compiler/
  ;; all of these passes include relevant IR definitions
  ;; if we end up needing more divisions, maybe these should be called level0.scm level1.scm etc.
  - high-level-ir.scm
  - high-level-passes.scm   ; transitions to either mid-level.scm or one of the high-level backends (only a language with proper tail calls(?), such as racket, scheme, or back to nscheme)
  - high-level-passes-optional.scm  ; stays within high-level
  - mid-level-ir.scm
  - mid-level-passes.scm    ; transitions to either low-level.scm or one of the mid-level backends (py, js, lua)
  - mid-level-passes-optional.scm   ; stays within mid-level
  - low-level-ir.scm
  - low-level-passes.scm    ; transitions to one of the low-level backends (c, x86-64, wasm)
  - low-level-passes-optional.scm   ; stays within low-level
  - backend/
    - c.scm
    - js.scm
    - lua.scm
    - py.scm
    - rkt.scm
    - wasm.scm
    - x86-64.scm
- nscheme/
  - it depends on restarts and raise, but shouldn't otherwise depend on a particular control implementation
  - base-primitives.scm
  - stage-simple.scm
  - parse.scm
  - program.scm
  - minimal.scm
  - meta.scm
  - match.scm
- gui/
  - library built on whatever native or emulated gui capabilities are provided
- platform/
  - common.scm, control.scm, privileged.scm
  - /low-level/
    - for low-level primitive operators, such as those used to implement the operators provided by control.scm
  - bootstrap.scm etc.
    - these currently test the process for building environments, but will probably be removed once nscheme.scm is implemented for each platform family (posix, www, etc.)
  - posix/ and www/ subdirectories describe platforms, and contain one or more platform-specific primitive-envs/runtime/compiler/linker compositions
  - posix  ; library for working in the context of a posix-like environment (simulated or otherwise)
    - privileged.scm
      - console streams/ports, file-system, network, process, shell-environment, command-line-arguments
        - and system paths (bootstrapped from find-systme-path in Racket)
          - (find-system-path 'orig-dir) (find-system-path 'run-file) (find-system-path 'exec-file)
    - port.scm
      - composes some of the above with the general form from text/port.scm
    - stty interaction
      - depends on a privileged capability to run an external process
    - other things related to IO
    - nscheme.scm  ; a (command-line) evaluation/compilation program that might also double as a library/module in the target platform language
      - this interface is allowed to run code that overrides, or even throws away, the interface, replacing it with something else
      - this repo may or may not include an implementation of a more featureful interactive editor/evaluator/desktop with documents, auto-save, snapshotting, and data export
        - both TUI and GUI variants
    ; each of these puts together a platform-specific (cross-)compiler that also bundles a copy of all of include/
    - racket/
    - c/         ; c/compile.scm will be more portable than c/x86-64/compile.scm
    - c/x86-64/  ; tail call and coroutine implementation differs from c/compile.scm
    - node.js/
    - python/
  - www  ; library for working in the context of a web browser-like environment (simulated or otherwise)
    - nscheme.scm  ; like posix/nscheme.scm but implemented with www capabilities instead of posix capabilities
      - it might be an .html file that you open in the browser that provides form-based IO (text fields or browser-based file opening)
        - we could also emulate a limited posix-like interface
    ; each of these puts together a platform-specific (cross-)compiler that also bundles a copy of all of include/
    - js/
    - js/wasm/


;;;;;;;;;;;;;;;;; DO THE ABOVE FIRST

- nscheme stage/unstage macro


when will a thread's resources be garbage collected when using reference counting?
- have to avoid cyclic references, which seems challenging in combination with threads that link themselves to other threads
- but should be acyclic if they just use (current-thread) to do so, right?


see what affect on timing we get from introducing E:begin/E:seq and E:let
- to reduce the amount of interrupt ticking


there are different kinds of posix programs, and/or ways to launch them
  - terminal with standard IO
  - desktop GUI
- the cross-compiler will know how to provide posix initialization scaffolding, and (re)bind procedures with the standard capability names
  - specifically, it knows how to set up the ambient capability environment


what if a program obtains new platform-specific capabilities at runtime?  how do we account for these?
- they should end up represented as closures whose underlying code object has a standard capability name, and whose
  closed-over data includes metadata describing how this capability was originally instantiated, so that it can be recreated
  - this metadata may include the arguments to the capability-construction call and/or implicit system state at the time of construction
  - for instance, if we open a file port, we record metadata with enough information about the arguments used to open the port
  - the platform can decide on the best closure representation to use for each kind of capability, to minimize metadata overhead
    - for instance, in some cases, (part of) the metadata may be used by the procedure code each time it is invoked
      - this example might be rare though, because in this case we should be able to split the capability more finely, right?
      - i.e., a base capability that takes parameters, and a non-capability wrapper that calls the base capability with
        the parameters it closes over
- for snapshots, we need a way to rebind these parameterized capabilities
  - we can have handlers both at snapshot time and at reinstantiation time (snapshot handlers and reinstantiation handlers are distinct)
    - at reinstantiation time, the handler figures out how to restore a capability given its metadata and any other data packaged in the snapshot
    - at snapshot time we sometimes want to package additional info with the capability, for instance the entire file content for an open file
      - the handler decides when we want to do extreme things like this
      - platform can provide some default handlers, and users can override and augment this set of handlers
        - default reinstantiation handler for an open file might just say, "sorry, can't reinstantiate this without more context"


reimplement read and write
- a variant for syntax objects
- a variant for styled document objects


continue with tty library and other posix-interfacing stuff
- maybe work on an editor / repl


a quasiquote intermediate language that can be used both for expressions and patterns?
- it would embed unquoted syntax fragments to be dealt with by the IR's consumer
- ellipsis too?

should we introduce a similar "constructors" intermediate language?
- for cons, list, vector, bytevector, records, etc.
- or is this too trivial?
- maybe a sublanguage for record construction with named fields is still worthy, though
  - though it could just be a higher-order utility that takes a subparser (e.g., parse-expression) for the field arguments
    - seems more reasonable than implementing a new language
    - should the same be true for quasiquote?
      - the value of a sublanguage is that it can be extended with additional operators, which we might want for qq, but wouldn't want for records?



clean up old notes in this file!


then go back to working on a compiler
- probably starting with an assembler, and/or other target code generators, so we can build something executable right away


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We can implement the above right now ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

local/global unique/shared
- should these be applied as annotations on variable bindings or as block scopes?
  - unambiguous binding notation: could use a vector to wrap an annotated variable
    - e.g., ```(let ((x __)) __)``` vs. ```(let ((#(x local unique) __)) __)```
  - how would block scope behave operationally? how do we achieve exclave? (how does exclave work exactly?)

- Scheme expressions should be embedded in LLL, not the other way around
  - LLL can be a "richer" language
  - Scheme can still express LLL ideas with unsafe primitives for memory and machine arithmetic, and by disabling interrupts
    - It's the compiler's job to enforce the efficiency of these ideas
      - e.g., if static analysis can show that a procedure is only ever called while interrupts are
        disabled, interrupt polling code will not be inserted into the procedure body

- non-semantic E:annotated directives can describe communication preferences about analysis results
  - e.g.,
    - "Ignore all errors within this expression."
    - "Ignore non-guaranteed errors within this expression."
      - same as "Show me only the guaranteed errors within this expression."
    - "Show me all potential errors within this expression."
    - "Assume no errors will occur within this expression."
      - i.e., omit safety checks for the sake of efficiency
      - Only necessary if analysis detects "potential" errors, because no safety checks would be included otherwise
    - "I expect this expression to not call unknown procedures."
      - or some other way(s) of ensuring that expressions are sufficiently specialized / inlined
    - "I expect this expression to be recognized as dead code."
      - e.g., you might have a debug assertion, and want a compile-time guarantee that it will
        always succeed.  If analysis can show the assertion predicate always returns `#t` (without
        side effects), then the assertion check becomes dead code.  If it's not clear that the
        predicate always returns `#t`, the check will not be dead code, indicating no guarantee.
        In fact, analysis may even show that the predicate definitely returns `#f` in at least
        some cases, which could communicate an even stronger warning.
    - "I consider it an error if this expression causes a panic."
    - "I consider it an error if this expression can be interrupted."
    - "I consider it an error if this expression allocates on the heap."
      - or something related to regions ...
    - "I consider it an error if this expression mutates anything."
    - "I consider it an error if this expression consumes input."
    - "I consider it an error if this expression produces output."
    - "I consider it an error if X escapes this expression through side channels."
    - "I consider it an error if X escapes this expression at all (even via direct return)."
  - non-semantic because these don't change the effects and result of simply running a program

lower the IR to at least make captured variables explicit, to allow safe-for-space closures
- assignment 12 convert-closures
- so maybe start with a variation of assignments 14 through 12

relevant parts of akeep/dybvig:
- Q: where does tail-call frame-fixing happen?
  - assignment 6 imposes calling conventions for tail calls
    - fixup is implicit, and does not require special compilation support
      - register and frame-location parameters are assigned to fresh locals which are then used
        - this includes the return-address register
      - as a result, the register and frame-locations used for the parameters are no longer live
        - so they can be reused by the register and frame allocator, e.g., to store tail call arguments
          - also handled by assignment, but this time into the register and frame-locations for parameters
            - return-point variable used in a tail call comes from enclosing lambda
            - frame-pointer does not have to change since the same call frame is reused for tail calls
              - unless stack is relocated due to insufficient size
  - assignment 7 deals with call-live vars and new-frames for non-tail calls
    - these do adjust the frame-pointer, and the current offset needs to be tracked for frame-var allocation
- challenge assignment B
  - optimize-source
    - constant folding
    - copy propagation
    - useless and dead code elimination
- assignment 14
  - convert-complex-datum
    - we should preserve shared-identity of values injected into quote expressions
      - if stratified/standalone-compiling, we would wrap the final program with let-bindings that achieve one-time construction of these shared values
      - otherwise, if live-on-the-fly-compiling, the original, live values should be directly referenced from the quoted locations
        - so that existing references to the same values are eq? to the new references
  - purify-letrec

- assignment 13
  - optimize-direct-call
  - remove-anonymous-lambda
  - sanitize-binding-forms
  - optimize-known-call
- challenge assignment C
  - uncover-well-known
  - optimize-free
  - optimize-self-reference
- assignment 12
  - uncover-free
  - convert-closures
  - introduce-procedure-primitives
- assignment 11
  - lift-letrec
  - normalize-context
  - optimize-jumps
- assignment 10
  - specify-representation
- assignment 9
  - uncover-locals
  - remove-let
- assignment 8 (UIL)


eval-compiled-X.scm
- maybe start with an SICP-style simulated register machine, also similar to old-1's eval-k.scm
  - naive register usage without much analysis
  - first a rkt version for testing
  - then a JS version for cross-compiling, and sooner development of a JS platform
  - maybe a C and x86-64 version, if it's worthwhile to start developing a native platform that way

instructions:
- label
  - no-op, and a symbolic name for the next instruction
- (perform (op name rand ...))
  - effectful operation, any result is ignored
- (assign loc rand)
- (assign loc (op name rand ...))
  - ops include high-level memory operations, like cons car cdr, for simplicity
- (branch (op name rand ...) label)
  - jump to label if the op returns true
- (jump rand)
  - absolute jump to computed label
- (save reg)
- (restore reg)
operands:
- (label name)
- (quote value)
- loc
locations:
- (reg name)

;  context:
;  - fuel
;  - pc
;  - stack
;  - registers

annotated instruction sequences
- registers used
- registers modified
- list of instructions

thread/stack-context:
- top-stack-frame
  - a stack is an implicitly-linked list of frames
  - each frame contains:
    - a return-closure
      - represented as a pair of function and previous-frame (basically an untagged closure, or we could tag it)
    - current-closure value
      - self-reference for the currently-called procedure
      - contains captured variable values
    - argument values
    - local variable values
- panic-handler
- timer-interrupt-handler
- interrupt-timer (also used for trampoline resetting)
- disable-interrupts-count
- current-coroutine
  - ccoroutine-register
- result-value accumulator
- procedure-to-be-called

figure out JS representation strategy
- ideally flat closures, not an env register
  - how should closures be represented? a plain function? a structure wrapping a function?
- registers for value-accumulator and procedure-to-be-called
- register for stack, which may include return-address (represented as a JS function)
- register for frame-base?
- register for argument count
- maybe some extra registers for arguments?
- values
  - null: null
  - boolean: true false
  - string: string
  - fixnum: 32-bit integer
  - arrays where field 0 contains a type header
    - symbol bigint rational f32 f64 pair vector mvector closure record
  - can bytevectors and mbytevectors go into a typed u8 array?
    - first byte is a type header indicating whether it's mutable

- the compiler should associate prim op names with general attribute flags (pure, terminates, etc.)
  - if the compiler doesn't recognize a prim op name, it can assume worst-case attributes, treating it as an opaque procedure
  - later note: maybe don't do this by name

weak refs w/ non-referencing finalizers
- finalizer is scavenged before determining whether weak ref is broken, so objects are not in danger of being resurrected
- weak refs introduce observable nondeterminism, unless well encapsulated
  - e.g., a referentially-transparent weak value dictionary used for caching
    - a key's membership is deterministic
      - referencing a particular key either always fails, or always produces an equivalent value
    - referencing a member key that is missing from storage must automatically load a new value
      - so the user can't detect it was missing
    - e.g., a symbol-interning table
- in what context is a finalizer called? allows abitrary code execution at an arbitrary time
- weak refs might be harder to support with reference-counting memory management

port dbk's 2-3 btree, old hamt, and/or other efficient data structures
- we'll use these as sets during program analysis

start working on an initial optimization pass inspired by cp0
- track variable references and single-references
  - are these the same as procedure parameter properties?
- track constraints learned from predicates in conditional branches
- track procedure properties:
  - parameter usage
    - unreferenced
    - singly-referenced
    - escaped
    - locally-transmitted (non-escaping reference)
      - boolean
      - called
      - type?/car/cdr/vector-ref/etc.
  - recursive
    - invariant parameters
  - lower and upper bound on effects (exact effects can depend on call arguments)
    - possible nontermination
    - irregular control transfer
    - mutation
- sub-zero cfa

### Implementation complexity notes

## June 2024

     516 ./include/nscheme/match.scm
     421 ./include/nscheme/parse.scm
     392 ./include/nscheme/minimal.scm
      28 ./include/nscheme/program.scm
      28 ./include/nscheme/stage.scm
     112 ./include/nscheme/meta.scm
      13 ./include/platform/primitive-control-low-level-privileged.scm
      31 ./include/platform/primitive-privileged.scm
      17 ./include/platform/bootstrap-stratified.scm
      30 ./include/platform/primitive.scm
      15 ./include/platform/bootstrap.scm
     229 ./include/syntax.scm
     173 ./include/compiler/high-level-ir.scm
      43 ./include/compiler/backend/rkt.scm
     175 ./include/base/string.scm
      44 ./include/base/raise.scm
      27 ./include/base/mbytevector.scm
      76 ./include/base/exception.scm
      24 ./include/base/misc.scm
      24 ./include/base/mvector.scm
      82 ./include/base/number.scm
      26 ./include/base/bytevector.scm
      49 ./include/base/record.scm
      12 ./include/base/pair.scm
     241 ./include/base/list.scm
      61 ./include/base/restart.scm
      13 ./include/base/vector.scm
     168 ./include/base/control.scm
     163 ./include/text/terminal-control.scm
    3233 total

## May 2024

[1.2K May  8 16:45]  include/
├── [ 800 May  8 16:42]  base/
│   ├── [ 335 Jan 24  2023]  bytevector.scm
│   ├── [7.4K May  8 16:41]  control.scm
│   ├── [3.7K Feb 27 16:29]  exception.scm
│   ├── [9.3K May  8 16:35]  list.scm
│   ├── [1.4K Jan 23  2023]  mbytevector.scm
│   ├── [ 973 May  3 12:28]  misc.scm
│   ├── [1.4K Mar  6 23:14]  mvector.scm
│   ├── [1.8K Sep  1  2023]  number.scm
│   ├── [ 408 Mar  1  2023]  pair.scm
│   ├── [1.5K Mar 24 15:26]  raise.scm
│   ├── [2.3K Mar 23 16:21]  record.scm
│   ├── [2.5K Mar  4 13:56]  restart.scm
│   ├── [ 964 May  8 16:30]  string.scm
│   └── [ 540 Jan 24  2023]  vector.scm
├── [  64 May  8 16:42]  boot/
├── [  96 May  7 14:42]  primitive-environments/
│   └── [   0 May  7 14:42]  put-platform-specific-primitives-in-separate-files
├── [1.1K May  8 17:01]  bootstrap-stratified.scm
├── [1.8K May  8 17:00]  bootstrap.scm
├── [ 11K Jul 12  2023]  compile-js-simple.scm
├── [3.1K May  3 14:31]  compile-rkt-simple.scm
├── [1.2K Sep  2  2023]  compiler-passes.scm
├── [5.9K May  8 16:01]  eval-simple.scm
├── [6.4K May  8 15:45]  extended.scm
├── [ 24K Jan 26  2023]  grammar.scm
├── [2.6K May  3 09:57]  ir.scm
├── [ 32K May  8 15:46]  match.scm
├── [ 23K May  8 15:24]  minimal.scm
├── [ 20K May  8 15:57]  parse.scm
├── [5.6K May  8 20:05]  primitive.scm
├── [ 28K Jan 26  2023]  read.scm
├── [ 980 Sep  2  2023]  stage-simple.scm
├── [8.6K May  7 17:53]  syntax.scm
├── [7.6K Apr  4  2021]  tty.scm
├── [6.1K Jan 26  2023]  unicode.scm
└── [8.4K Jan 26  2023]  write.scm

4 directories, 34 files

       9 include/base/bytevector.scm
     168 include/base/control.scm
      76 include/base/exception.scm
     239 include/base/list.scm
      27 include/base/mbytevector.scm
      24 include/base/misc.scm
      28 include/base/mvector.scm
      50 include/base/number.scm
      12 include/base/pair.scm
      44 include/base/raise.scm
      49 include/base/record.scm
      61 include/base/restart.scm
      24 include/base/string.scm
      13 include/base/vector.scm
      26 include/bootstrap-stratified.scm
      41 include/bootstrap.scm
     236 include/compile-js-simple.scm
      49 include/compile-rkt-simple.scm
      39 include/compiler-passes.scm
     108 include/eval-simple.scm
     143 include/extended.scm
     510 include/grammar.scm
      47 include/ir.scm
     515 include/match.scm
     420 include/minimal.scm
     426 include/parse.scm
     104 include/primitive.scm
     566 include/read.scm
      19 include/stage-simple.scm
     212 include/syntax.scm
     163 include/tty.scm
     135 include/unicode.scm
     192 include/write.scm
    4775 total

       9 include/base/bytevector.scm
     168 include/base/control.scm
      76 include/base/exception.scm
     239 include/base/list.scm
      27 include/base/mbytevector.scm
      24 include/base/misc.scm
      28 include/base/mvector.scm
      50 include/base/number.scm
      12 include/base/pair.scm
      44 include/base/raise.scm
      49 include/base/record.scm
      61 include/base/restart.scm
      24 include/base/string.scm
      13 include/base/vector.scm
      26 include/bootstrap-stratified.scm
      41 include/bootstrap.scm
     236 include/compile-js-simple.scm
      49 include/compile-rkt-simple.scm
      39 include/compiler-passes.scm
     108 include/eval-simple.scm
     143 include/extended.scm
      47 include/ir.scm
     515 include/match.scm
     420 include/minimal.scm
     426 include/parse.scm
     104 include/primitive.scm
      19 include/stage-simple.scm
     212 include/syntax.scm
    3209 total

     143 include/extended.scm
     515 include/match.scm
     658 total

       9 include/base/bytevector.scm
     168 include/base/control.scm
      76 include/base/exception.scm
     239 include/base/list.scm
      27 include/base/mbytevector.scm
      24 include/base/misc.scm
      28 include/base/mvector.scm
      50 include/base/number.scm
      12 include/base/pair.scm
      44 include/base/raise.scm
      49 include/base/record.scm
      61 include/base/restart.scm
      24 include/base/string.scm
      13 include/base/vector.scm
      26 include/bootstrap-stratified.scm
      41 include/bootstrap.scm
     108 include/eval-simple.scm
      47 include/ir.scm
     420 include/minimal.scm
     426 include/parse.scm
     104 include/primitive.scm
      19 include/stage-simple.scm
     212 include/syntax.scm
    2227 total

## 2023

implementation complexity cost:
- all of pattern matching: 492 lines
  - complex patterns: (+ 193 39) = 232 lines
    - ellipsis patterns: 193 lines
    - or and not patterns: 39 lines
  - simple patterns: (- 492 (+ 193 39)) = 260 lines
- all of bootstrap parsing at the moment: (+ 195 70 336 871) = 1472 lines
- all of bootstrap compiler at the moment: (+ 50 195 70 336 871) = 1522 lines

include
├── [ 476 Mar  1  9:46]  base/
│   ├── [ 335 Jan 24 12:08]  bytevector.scm
│   ├── [1.1K Jan 23 21:24]  compare.scm
│   ├── [8.1K Mar  1  9:45]  list.scm
│   ├── [1.4K Jan 23 11:33]  mbytevector.scm
│   ├── [  68 Jan 20 19:50]  misc.scm
│   ├── [1.4K Oct 24 15:42]  mvector.scm
│   ├── [2.2K Mar  1  9:45]  number.scm
│   ├── [ 408 Mar  1  9:45]  pair.scm
│   ├── [ 291 Jan 26 11:53]  string.scm
│   └── [ 540 Jan 24 12:08]  vector.scm
├── [ 204 Mar  1  9:46]  boot/
│   ├── [2.3K Feb 28 15:54]  error.scm
│   └── [2.3K Feb 28 15:57]  record.scm
├── [2.7K Feb 13 16:24]  ast.scm
├── [6.7K Feb 25  6:03]  extended.scm
├── [ 24K Jan 26 12:00]  grammar.scm
├── [ 31K Feb 27  9:54]  match.scm
├── [ 20K Mar  1  9:45]  minimal.scm
├── [ 16K Mar  1  9:54]  parse.scm
├── [4.3K Mar  1  9:46]  primitive.scm
├── [ 28K Jan 26 12:32]  read.scm
├── [8.8K Mar  1 15:56]  syntax.scm
├── [7.6K Apr  4  2021]  tty.scm
├── [6.1K Jan 26 12:34]  unicode.scm
└── [8.4K Jan 26 12:37]  write.scm

2 directories, 24 files

      50 ./include/ast.scm
       9 ./include/base/bytevector.scm
      30 ./include/base/compare.scm
     198 ./include/base/list.scm
      27 ./include/base/mbytevector.scm
       2 ./include/base/misc.scm
      28 ./include/base/mvector.scm
      65 ./include/base/number.scm
      12 ./include/base/pair.scm
       8 ./include/base/string.scm
      13 ./include/base/vector.scm
      61 ./include/boot/error.scm
      47 ./include/boot/record.scm
     520 ./include/match.scm
     373 ./include/minimal.scm
     345 ./include/parse.scm
      75 ./include/primitive.scm
     196 ./include/syntax.scm
    2059 total

### bootstrap

- Bootstrap by implementing a sequence of languages, gradually adding features
  - Each language is used to implement the next.
  - Each subsequent language is roughly a superset of the previous language.
    - Example of an exception: adding preemptive signals and timer interrupts changes the semantics
      significantly in a way that arguably does not create a strict superset of the previous,
      cooperative-only language.
  - We will implement some language layers by introducing optional compiler passes that add implicit
    behavior, such as safety checking and interrupts.
  - Early version of the compiler will use lower-tech definitions that don't rely on late-stage
    language features.  We can swap these definitions out as we introduce useful features.  For
    instance, parsing errors will likely `panic` in early stages, but `raise` in later stages.
  - Features and sequence will depend on the platform and its capabilities.
    - Platforms should be responsible for setting up primitive environments and some of the base
      libraries based on their capabilities, rather than writing generic base libraries that aim at
      a lowest common denominator across all platforms.
    - For instance, on a native platform we will implement almost all of the usual primitives
      using low-level arithmetic and memory manipulation.  But on a high-level platform our
      primitives might be based on relatively high-level capabilities.
    - Derived procedures in the base library will sometimes have platform-specific definitions.
  - Possible sequence for a native platform (though we could reorder some of these):
    - Start with no safety checks, and no garbage collection.
      - Not even automatic memory allocation from the OS
      - Possibly not even automatic call stack allocation?
        - We eventually need call stack limit checking and reallocation, but may not need these
          until we introduce coroutine primitives.
    - Add implicit type and bounds checking.
      - Compiler pass `explicate-safety-checks` to insert checking and `panic` code
    - Add general interrupts.
      - Compiler pass `explicate-interrupts` to insert code for interrupt ticking, expiration
        checking, and dispatch
    - Possibly add automatic call stack management.
      - Compiler pass `explicate-call-stack` to check limits and allocate or resize the call stack
    - Add automatic memory management.
    - Add data constructors and arbitrary precision arithmetic.
    - Add coroutines.
    - Add timer interrupts.
  - An optional compiler pass for static type checking or other static analysis

- Split base library code by topic and level of privilege
  - Privileges that may be needed: records, control operators, dynamic parameters
  - Split along two privilege levels only? privileged and unprivileged?
    - privileged:
      - dynamic-scope
      - error
      - thread
      - syntax
      - define-datatype
    - unprivileged:
      - env, parse
      - number, list, vector, bytevector, string, hash, btree
      - comparison-and-equality
      - quasiquote, match
      - read, write  ; document-based rather than just s-expression-based?

- How do we organize the bootstrap and user libraries? env-building, parsing, etc.
  - Don't worry about filesystem organization too much, worry about library/package definitions as
    persisted data values
  - Safe vs. unsafe ?  Varying degrees of safety?  Pure vs. impure?  IO?
  - languages: boot (privileged) vs. base (user) vs. io (platform-specific)

### Primitives

- Allow primitives to be procedures containing arbitrary code
  - Can still provide names for more reliable cross-referencing and optimization-rule association
    - Can also provide portable low-level code when possible
    - Maybe also original/optimized source for any high-low-level language that was used to define
      the primitive
  - Can add new primitives arbitrarily
  - Can implicitly learn new primitives by importing snapshot fragments containing them
    - Assuming the snapshot is compatible with our platform
    - This requires trust that each primitive implementation is safe
      - For this reason, primitives must be distinguished from user-defined procedures, so we know
        when snapshots are dangerous to import
      - User-defined procedures in snapshots also contain seemingly-arbitrary low-level code, but if
        we are worried, we can recompile them from the higher-level code they also (should) come
        with in their metadata
  - Primitives will often have algebraic-rewrite optimization rules
    - e.g., `(car (cons A B)) ==> (let ((x.0 A)) (begin B x.0))`
    - multiple primitives often involved in the same rule
    - Is there a good reason why we couldn't support these for non-primitives as well?
      - e.g., user-defined types with algebraic rewrite rules
        - dictionaries:
          - (get (put d k v) k) == v
          - (put (put d k v1) k v2) == (put d k v2)
        - matrices: linear algebra identities
        - symbolic reals: trig identities etc.
        - should also be able to specify CFA lattices for these types and operations?
          - or is each lattice derivable from the rewrite rules?
      - Just need a reliable way for the compiler to associate procedures and related rules
      - We could try to stratify inlining by rule domain
        - start by not inlining anything with any rules, then inline procedures that only have
          highest-level rules, then those that have only next-highest-level rules, and so on ...
        - no guarantee that this process will be complete in terms of finding optimal rewrites
      - Or try equality saturation
      - Or heuristics based on procedure/application shape and potential for uncovering rewrites

### Procedures and metadata

- We would like to treat captured variables as opaque to support compiler optimization of their
  representation. e.g., unboxing.  But if procedure-metadata exposes these, the representation
  must be standard enough to allow code to interact with it.
  - We don't have to return captured data verbatim.  We could have procedure-metadata perform
    just-in-time transformations on optimized representations to convert them to general values.

- No need for an IO capability to be a distinct class of procedure as long as we register its
  ephemeral state in system-owned mutable state.
  - e.g., to use a file descriptor, we retrieve it from a box and pass it to a primitive operator.
  - This will prevent the compiler from being able to inline the ephemeral state value since the
    system could change it at any time.
  - NOTE: snapshotting normally should not follow pointers into system-owned data.

- Primitive procedures normally cannot be given a portable AST: their definition depends on both
  platform AND compiler.
  - Each compiler has to supply its own definition (if supported) depending on the platform
    - Primitive inlining can be supported within other primitives, but not between primitive and
      portable procedures
  - Since primitive procedure values are compiled via reflection, where procedure values are
    embedded directly in ASTs, if we want to cross-compile, then we need to create stub
    (error-reporting) implementations for the foreign platform's primitives
    - i.e., `(error "unimplemented primitive" name.platform name.primitive)`
  - They should typically be given unique names (which can be arbitrary values), possibly including
    platform identifiers or other classification to disambiguate them.
    - Can provide a multi-part name with library prefix for extra uniqueness, e.g., `(base . cons)`,
      `(boot . procedure-metadata)`, etc.
    - Snapshots may also want to group these by library to remain organized
      - e.g.,:
        ```
        ((boot
          (procedure-metadata . <_>)
          (make-coroutine . <_>)
          etc. ...)
         (base
          (cons . <_>)
          (car . <_>)
          etc. ...)
         (www-js
          (alert . <_>)
          (console-log . <_>)
          (document-create-element . <_>)
          (add-event-listener . <_>)
          etc. ...)
         etc. ...)
        ```

- Procedure data and metadata:
  - `primitive? => name or #f`
    - If a procedure is marked as primitive, a compiler should be careful not to inline it into
      portable code, such as optimized source
      - NOTE: it is fine to inline it into other primitive/nonportable code
  - source code provenance
  - optimized source with type/flow analysis info
    - the AST after source-to-source optimization, but no environment (a flat list of captured
      variables is included in another part of the metadata)
    - If the procedure is nonprimitive, its optimized source should be portable across platforms
      - Primitive algebraic rules may have been used during source optimization, but primitives
        have not been inlined/open-coded yet at this stage
  - portable low-level code
    - compared to source code, this should be faster to translate to the platform-specific
      low-level code (for this and other platforms)
      - if represented as bytecode, it should also not require a graph walk, unlike source ASTs
    - still no primitives have been inlined
  - platform-specific low-level code
    - the x86-64/RISC-V/WASM/etc. corresponding to the platform (architecture+OS+ABI+runtime)
      being used
      - the platform is a tuple: a choice of
        - architecture (one of x86, ARM, WASM, etc. ...)
        - host OS (interface to devices and other resources)
        - ABI (may be nonstandard)
        - a runtime-library/kernel designed with the above choices in mind
    - at this point, primitives are likely to have been inlined/open-coded

### LLL

- simple non-polymorphic type inference with representation subtyping
  - typed lambda/let/letrec
  - existentially quantified type variables that must end up ground after inference
  - allow implicit subtyping coercion for structs, arrays, pointers
  - do not automatically coerce numbers/bitvectors, signed or unsigned
  - separate safe (subtyping) and unsafe (non-subtyping) casts
  - inferred numeric literals
    - how should quoting work?
- low-level representation types
  - (unsigned 16 value) or typedef-like shorthands such as (bytes 2 value) or (u16 value)
- unboxed call-by-value: any boxing is explicit via addresses
- explicit vectorized ops
- real OS threads, if available
- Any VM/runtime integration is explicit
  - allow VM interrupts only at explicit safe points
    - no virtual-thread preemption: only cooperative multitasking
  - GC integration, support, and metadata must be specified explicitly, if any
    - i.e., allocator and memory manager can be written in this language

- Maybe later, we can try to support dependent types with simple computations
  - #tuple(n:int #array(n X))
  - #tuple(n:int #ptr-to-array(n X))
  - #tuple(start:#ptr-to-array(n X) end:#ptr-to-array(0 X) where n:int = (ptr- end start))
    - distinguish internal ptrs from main ptrs that should be directly scavenged by GC?
  - dependent and polymorphic types are really functions from parameters to a concrete representation
    - a type might be an "application" of such a function
      - e.g., the array type is a function with two parameters: a length and an element type
    - dependent computation dependency graph must be acylic, with everything reachable from the root location
  - types may be mutually recursive
    - which is fine if we think of all types as functions, possibly with zero parameters
    - so a type is a function: parameters -> representation
      - where atomic types take zero parameters
      - array can be a builtin non-atomic type, but it could also be expressed as a sequence of nested, unboxed pair-products
        - same story for tuple
        - probably not great to reduce these types in this way, so provide array and tuple as primitive type constructors
        - nullary tuple/array is the unit type
      - similar story for n-ary sum type in terms of binary sums, but this is awkward
        - nullary sum is the void type
    - use either memoization or naming/folding to prevent infinite representation

### cee-flat

- for generating C code
  - design something analogous for generating JS, WASM, x86-64, RISC-V, etc.
- major (concise with sanity checking and inferred output) and minor (unambiguously tagged with verbatim output) modes
- major mode does some sanity-checking inference, and lowers to minor mode
  - minor mode is just a raw, tagged s-expression syntax for C, that does no analysis whatsoever
  - can embed `(minor etc. ...)` subforms in major code
- `introduce-type* introduce-struct* introduce-expression* introduce-function* introduce-macro*`
  - no need to forward declare, but still must describe header-sourced identifiers somewhere
  - we won't be defining any of our own macros, but we may need to invoke macros defined by included C headers
- `ref deref dot (we can have dot automatically infer arrows!) index := cast`
  - symbols as variables will parse to `vref` in cee-flat
  - dots on pointers become arrows in cee-flat
- start implementing a C-assisted runtime (which we'll eventually replace to eliminate dependence on C)

### old

- Examine these procedural macro test cases:
  - or (introducing temporary variables)
    - can lambda/thunkify subexpressions to avoid needing fresh temporaries
  - consistently-generated names for struct/record field accessors
  - let[rec]-syntax within definition contexts
    - a macro that references some "x" defined in the same context
      - have to eval the macro procedure, and somehow give it access to the *final* env/context to get "x" from
    - sibling expansion
    - child expansion
  - macro-generating macros
  - pattern matching sublanguage
  - module exports of macros

- Reader notes:
  - `#;#;` comments two expressions

- read-doc write-doc
  - recognize/preserve comments in doc structure
  - recognize/preserve formatting/styles
    - e.g., whether a pair of a list is explicitly dotted, #-based headers in
      #header(a b c ...) forms, numeric literal forms, etc.
    - indentation and alignment

- terminology: IO device vs. port

- TUI widget ideas: https://github.com/chjj/blessed

- posix-y platform process environment interface
  - command line arguments
  - environment variables
  - run system commands
  -   direct or via subprocess controller
  - other process information (e.g., PID)

rough component implementation order for bootstrap:
  * ~~io~~ (skip for now, ports are sufficient for prototyping)
  * ~~port~~
  * ~~unicode~~
  * ~~grammar~~ (abandoned for now)
  * ~~read~~
  * ~~write~~
  * syntax
  * library
  * parse
  * ast
  * nanopass
  * target-racket
  * base
  * virtualization, concurrency, io

Racket-compatible bootstrap in more detail:
  * shallow embedding of nScheme in Racket
    * ~~.rkt files that include .scm files~~
    * ~~simple io capabilities (see platforms for more io): files and stdio~~
    * avoid situations where racket features can be distinguished from nscheme
      * abort/shift/reset tags and dynamic parameters
        * could avoid their use entirely?  might be hard for dynamic parameters
  * base library
    * redefine values, let-values in terms of vectors
    * simple record types
      * optional common fields for multiple record types
    * simple match
      * maybe just implement more general pattern matching?
      * hack for matching record types, with optional implicit field matching:
        if `name` is not special, then (name (field1 x) field2) matches values
        that are `name?` and submatches on `name-field1` and `name-field2`.
      * special names: quote, quasiquote, cons, list, cons*, list*, vector
    * ~~input/output ports (build on simple io primitives)~~
  * bootstrap self-applicable compiler
    * syntax objects with source annotations
    * read-syntax/write
    * parse/unparse
      * design new AST including case-lambda and dynamic control
      * environment maps identifier labels to lists of properties
        * some identifier properties are for describing parsers applicable to
          different vocabularies, or classes of syntax.
          * e.g., `begin` has parsers/micros that allow it to be used in both
            expression and definition contexts
        * example syntactic classes and subclasses:
          * expression (variables, special form parsers, typical macro transformers)
            * ref (identifier may appear in any expression position)
            * operator (identifier may be used in operator position)
          * `set!` (for variables that may be assigned via `set!`)
            * could make use of both the `match` and `set!` vocabularies
              to define a pattern-matching assignment operator
          * definition (operators for declaration/definition contexts)
          * template (for syntax-case pattern variables)
            * track ellipsis level
          * module (identifies a Chez-style module)
          * match (a pattern constructor in the match vocabulary (match is a micro))
          * grammar (name of a production rule constructor in a grammar definition)
          * formula (name of a logical connective or relation in a logical definition)
            * e.g., allows overloading `=`, `<`, `and`, `or`, etc. for use in building
              logical formulas and assertions, without shadowing their typical
              expression-oriented definitions as operators/procedures, allowing them
              to retain their usual meaning within formula `terms`
              * contrived example: `(implies (= #t (= A B)) (= A B))`
                * the conclusion `(= A B)` uses `=` to construct a logical constraint
                  that the terms `A` and `B` are equal
                * the hypothesis's outer use of `=` constructs the constraint that
                  `#t` is equal to the result of evaluating the term `(= A B)`,
                  which itself uses `=` as the usual numeric equality procedure
                * note: depending on the underlying logic, the reverse implication
                  will not necessarily be a theorem if the `=` used to build equality
                  constraints is also applicable to types other than numbers
      * hygienic macros
        * syntax pattern matching
        * syntax-case, syntax-rules
        * syntax-parameterize
    * nanopass framework
      * automatic record definition, validation, translation, parsing/unparsing
        * more general grammar definition and match?
          * terminal-aware patterns
      * define-language, define-pass
      * nanopass-case, with-output-language
      * language->s-expression, diff-languages
      * echo and trace
    * Racket as compilation target
      * if output is too large, apply simple code-shrinking optimizations
    * modularity and libraries
      * worst case, could bootstrap with just a manual include-based system
        * include/load programs and/or data from file, compile/eval
      * ideally, use a typical library approach with implicit phasing
        * r[6,7]rs libraries, s48 structures and configuration language
        * Chez-style modules for precise binding visibility within expressions
        * Chez-inspired meta defs, but block-structured and cross-phase
        * rebind exports/imports to allow non-interfering library-local set!
          * e.g., invoke applies an import lambda that returns an export vector
        * invoke must return all bindings that exported macros may leak
          * including imports, transitively... this could explode
          * unless indirect exports are manually controlled
        * library components:
          - environment mapping labels to properties (includes macros)
          - scope/substitution mapping exported names to labels
          - invoke-code returning a vector of exported runtime values
            * due to macro leak, export all top-level definitions (by label)
          - a vector of exported runtime value labels in the same order
      * build on top of low-level programmatic construction and linking
      * dynamic library installation and invocation
        * r6rs environment, eval
      * link-preserving library renaming/repointing to support live upgrade
      * relative lib name paths and auto-prefixing child libs; packages
    * virtual systems
      - for virtualization, we need more than just time limits (alarms), we also need memory limits
        - a way to asynchronously yield when we're allocating beyond the limit
        - option to resume with a larger limit
      - virtualization-friendly compilation
        - compiler decorates each procedure with metadata retrievable by a special primitive
          - this primitive can make extensional reasoning about object-level programs unsound by leaking
            implementation details, so meta-level programs must treat it carefully
          - metadata includes source information sufficient for helpful tracing
          - metadata includes a code representation for dynamically recompiling, sufficient to support:
            - inlining and other interprocedural optimization
            - live upgrade, including swapping in updated/upgraded dependencies
            - live migration to a different platform
            - live migration to a different concurrency/memory model
            - adding instrumentation for alternative execution semantics
              - time travel debugging
              - distributed execution for collaborative, interactive evaluation
              - incremental/adaptive computation
              - automatic differentiation
              - relational evaluation queries
              - probabilistic programming queries
              - symbolic evaluation for static analysis and other forms of verification
          - metadata includes the version of its format
            - to allow a metaprogram to work with code compiled with different compiler versions
            - to allow a metaprogram to migrate code with an older format to the new format
        - handle io requests, signals/interrupts/preemption, and fatal errors
        - fine-grained process control, resource-budgeting
        - timers and concurrency via one-shot delimited continuations?
          - reset/timeout? reset/ticks?
          - optional deterministic scheduling
        - dynamically-compiled code that may refer to existing heap values
        - heap/image dumping and resumption
        - rewinding/undo
      * a real platform process may run multiple virtual systems at once
        * each may have a different configuration (libraries, devices, etc.)
        * isolation/distribution at the system level
          * as in Racket places, E vats, Erlang processes
        * user processes as subsystems
          * process event handling for communication
            * communicate via effects such as io and mutable read/write
              * effects escaping a process produce events
            * meta-level can use this to implement arbitrary event-driven
              systems, e.g., dataflow or reactive computation, fancy IDEs/REPLs
          * hierarchy of [sub]system evaluation/state contexts
            * effects escaping a subsystem bubble up into parent
              * may be paused/blocked/redirected
          * duplication of arbitrary subset of context
            * explore multiple worlds of hypothetical evaluation
            * fork with mulltiple incompatible changes that then work together
          * multiple concurrency modes
            * effect transactions
              * process may affect the context without race conditions
            * running uninterruptibly/atomically/critical-section
              * prevents effect interference by pausing sibling events
            * unrestricted
              * must coordinate explicitly (CAS/locks/etc) to prevent races

## Naming conventions

```
_    = blank
x-y  = x <space> y     ; for multi-word phrases, e.g., launch-all-missiles
x/y  = x with y
x^y  = x superscript y
x.y  = x subscript y   ; emphasizing grouping by x
y:x  = x subscript y   ; emphasizing grouping by y, the subscript; e.g., a type
                       ; y with constructors x, or implementing a common
                       ; interface of operations x, or other situations where
                       ; if the context is clear, the y: could be dropped from
                       ; the name
x@y  = x at y          ; result of projection or access using address/key y
x->y = x to y          ; procedure mapping type x to y
x=>y = x to y          ; finite map (e.g., hash or vector) with key type x
x&y  = x and y         ; a pair, or sometimes a 2-element list or vector
x*   = 0 or more xs    ; (typically homogeneous) lists, sometimes vectors
x?   = x huh           ; x is either a boolean(-ish) value itself, or a
                       ; predicate (procedure returning a boolean(-ish) value)
x!   = x bang          ; x may cause important side effects, such as mutation
                       ; or throwing an error; I/O operations often don't use
                       ; this convention
x?!  = assert x        ; a predicate/guard that throws an error if false
```

## Possible low-level type tagging schemes

One possible set of type-tagging scheme:
- 16-byte-aligned, 64-bit
  - 2 x fixnum
  - small immediate constant
  - other number
  - closure
  - code
  - symbol
  - cons
  - string
  - special-vector
  - immutable-bytevector
  - mutable-bytevector
  - immutable-vector
  - mutable-vector
  - immutable-other
  - mutable-other
- 8-byte-aligned, 32-bit or 64-bit
  - 2 x fixnum
  - immediate constant
    - () #t #f undefined etc.
    - single-precision flonum
    - gc-forwarding-mark
      - possibly also a gc-locked-mark if we attempt parallel gc
  - procedure
    - always a closure, non-closure code will be wrapped like a 0-ary closure
    - code pointer is tagged like an immediate and appears in 0th position
      - code embeds closure payload size, among other things
  - cons
  - are these good choices?
    - other number
      - double-precision flonum
      - bignum
      - ratnum
      - complex-exact
      - complex-flonum
    - immutable-other
      - immutable-vector and special-vector
        - tagged as 2 x fixnum
      - string and immutable-bytevector
        - using at least 2 non-immediate tags (safe because their segments won't be scavenged)
          - using more tags can support larger lengths
      - more, using immediate tags if they embed objects, otherwise could use remaining 4 non-immediate tags
    - mutable-other (or more like, pointer-equivalent-other, or other-with-identity)
      - mutable-vector
      - symbol
        - even though these are not mutable, they are compared via eq
        - using the same non-immediate tag as a string
      - mutable-bytevector
- ideas for hardware-aligned array types (which fall under the "other" category):
  - use double indirection to ensure data alignment
    - first indirection finds description paired with data pointer
    - second indirection into the aligned data pointer
  - cache-line-aligned arrays
    - allocated in multiples of cache lines
  - page-aligned arrays
    - allocated in multiples of pages
  - optionally pinned to avoid GC copying

Alternative, better type-tagging 8-byte-aligned scheme, both 32-bit and 64-bit:
- 0-least-significant-bit
  - 2 x fixnum
  - small immediate constants
    - () #t #f undefined etc.
    - single (and maybe half) precision flonums
  - cons
- 1-least-significant-bit
  - secondary type
    - tertiary type
      - for safe scavenging use 1 of a 16-byte-aligned-split immediate constant tag
        - unless all code-headered procedures live in a separate segment, and can use a non-immediate tag
      - gc-forwarding-mark
        - possibly also a gc-locked-mark if we attempt parallel gc
      - ratnum
        - can we find a good way to promote this to an immediate secondary, stored like a pair?
          - maybe lift to primary by taking the other split of the 16-byte-aligned-split immediate constant tag
          - or lift to primary by splitting the cons tag
        - components may be either fixnums or bignums
      - high precision floating point numbers
      - hardware-aligned array/struct types
        - use double indirection to ensure data alignment
          - first indirection finds description paired with data pointer
          - second indirection into the aligned data pointer
        - cache-line-aligned arrays
          - allocated in multiples of cache lines
        - page-aligned arrays
          - allocated in multiples of pages
        - optionally pinned to avoid GC copying
    - code
      - for safe scavenging use the other 1 of a 16-byte-aligned-split immediate constant tag
        - or put code-headered procedures in a separate segment
          - may make sense if the closure includes some untagged data that shouldn't be scavenged
      - acts as the header of a closure
        - points to both executable code and a header with closure layout information
    - 4 x bytevector
      - may use 4 non-immediate tags, which is safe because bytevector segments won't be scavenged
        - using more tags supports larger lengths
      - mutable unless primary tag is immutable, special, or symbol
    - 1 x vector
      - using one fixnum tag for safe scavenging
      - mutable unless primary tag is immutable, or special
    - non-code procedure
      - are these even valid?
        - if a procedure is well-known enough to not need a code pointer, it also shouldn't need dedicated tagging
          - so it could just be a vector
          - well, except in the case of storing non-scavengeable data, like unboxed double-precision floats ...
            - but here we'd need layout info anyway, which might as well come from a code pointer
      - using the other fixnum tag for safe scavenging
      - closure length encoded in tag
      - or put non-code-headered procedures in a separate segment (with code-headered procedures?)
        - may make sense if the closure includes some untagged data that shouldn't be scavenged
    - bignum
      - may use 1 non-immediate tag, which is safe because bignum segments won't be scavenged
      - encodes size in tag
  - immutable
    - signify that the secondary type vector or bytevector referred to is immutable
    - possibly other uses
  - special
    - signifies that either
      - the secondary type vector is immutable and intended to be interpreted like a record
      - the secondary type bytevector is immutable and intended to be interpreted like a string
        - must contain valid utf-8 data for this tagging to be possible
    - possibly other uses
  - symbol
    - the bytevector pointed to is both a valid string, and is the backing data for a symbol

## Ideas about control-transfer conventions

### Asymmetric returning and calling conventions with stack-based control contexts

Based on:
[An Efficient Implementation of Multiple Return Values in Scheme](https://dl.acm.org/doi/10.1145/182590.156784)

Code objects for procedures only have one entry point.  But code objects for returns have two: a
multi-value entry point followed by a single-value entry point.  The purpose is to support efficient
multi-value returns, yet also make the common case of single-value returns just as efficient as it
would be if multi-value returns were not part of the language.

code object layout for a return point:
```
  return-address points here
         ||
         \/
[metadata][first instruction][remaining instructions]
         /\                 /\
         ||                 ||
  call with argc =/= 1      call with 1 return value
```

For a call with n arguments:
- Pass the first k values in registers, the rest on the stack
- Set argc to n
- Jump to the first instruction of the procedure's code object

For a return of n values:
- Pass the first k values in registers, the rest on the stack (like a call with n arguments)
- If n = 1: jump to the single-value entry offset of the return-address (no need to set argc)
- If n =/= 1: set argc to n and jump to the multi-value entry offset of the return-address

Instruction layouts for different kinds of return points:

Single-value continuation:
- The common case when evaluating a sub-expression
```
L.multi: error
L.single: ...
```

Any-value continuation :
- e.g. the kind produced by `begin` for expressions that are only computed for effect.
```
L.multi: no-op ; fall through to L.single
L.single: ...
```

Multi-value continuation:
- Used for the general case of `apply/values`
```
L.multi: jump L.rest
L.single: argc := 1
L.rest: ...
```

The general case of `(apply/values callee values-producer)` installs a multi-value continuation
that dispatches to `callee`.  The return convention places return values the same way that the
calling convention places arguments, so this dispatch should be straightforward.

Control transfer to a first-class coroutine will be implemented as a call that returns to a
different stack (except in the special case of a self-transfer).

### Symmetric returning and calling conventions with closure-based control contexts

With closure-based control contexts, the same kind of code object and control-transfer could be used
for procedures, return points, and coroutines.

For instance, an illustration of symmetry between calling and returning:
- `(apply/values values-producer callee)` would compose the current continuation with the `callee`
  procedure before evaluating `values-producer`.  When `values-producer` returns normally, the
  behavior is exactly like calling `callee` with the return values as arguments.

code object layout:
```
             tagged-address points here
                       ||
                       \/
[other-metadata][header][first instruction][remaining instructions]
                       /\                 /\
                       ||                 ||
          call with 1 arg                 call with any number of args
          (no need to set argc register   (must set argc register before call)
           before call)
```

General convention for unknown callers (and returners) and callees (and returnees):
- The motivation of this convention is to make the common case for returns more efficient, and to
  simplify the implementation of `apply/values`.  Single-value returns are more common than
  multi-value returns.  By having a dedicated instruction offset to handle the single-value case,
  the caller (the returner) may omit setting the argc register, and the callee (the returnee) may
  specialize its handling of that case.
- For n arguments, pass the first k arguments (or return values) in registers, and the rest in
  the control-transfer buffer.
  - The control-transfer buffer is similar to the stack when using a stack-based control context.
    The difference is that when using a closure-based control context, all calls behave like tail
    calls, and so there is no stacking.  During a call, the buffer contents are placed in a closure
    representing the return point, and the buffer contents are then replaced by the new call.
- When n is not equal to 1, the caller must also set the argc register to communicate the number
  of arguments to the callee.
- When n is not equal to 1, we must jump to the second instruction in the code object.  The code
  can dispatch based on the argc register in whatever way it sees fit.
- When n is equal to 1, we may instead, but are not required to, jump to the very first
  instruction in the code object.  This instruction will dispatch in whatever way is appropriate
  to handle a single argument.  A simple implementation possibility would be to itself set argc to
  1, and automatically fall into the second instruction that handles the general case.
- Because we may still jump to the second instruction when n is equal to 1, it must be capable of
  handling that case.

When more information is known about the caller or callee, more efficient conventions could be
chosen on a case-by-case basis:
- When a callee may appear unknown in some contexts, its instructions must still adhere to the
  general convention.  However, when it is called in a context where it is known, the caller
  may use its knowledge of the callee to jump directly to an unconventional instruction that
  handles the caller's case.
  - For instance, when calling with n arguments, if the caller knows which instruction the callee
    dispatches to for handling n arguments, the caller can jump there directly.
- When a callee is always known when called, its instructions do not need to adhere to the general
  convention.

## First-class coroutines

Instead of building on first-class continuations, we build a foundation for control operators using
first-class coroutines.

These coroutines are full and symmetric.
- Full means they can "yield" at any level of procedure call, not just the outermost level.
  - Contrast this with a Python generator, which can only yield from the outermost level.
- Symmetric means there is no general yield operator.  Instead, a coroutine transfers control to
  another by calling it explicitly.

They are as expressive as one-shot delimited continuations, and may be used to implement exception
handling, engines, threads, generators, and one-shot algebraic effects.  They seem to be simpler
to use, and may be implementable with less call-stack copying than first-class continuations.

`(make-coroutine register-value proc)` creates a new calling context whose control-flow is
independent of any other.  Code running within this coroutine has access to a private register via
`(current-coroutine-register)`.  It also creates and returns a control procedure that, when
called, transfers control from the current coroutine to the created one.  The first time this
procedure is called, it passes its arguments to `proc` on a new call stack.

`(current-coroutine)` returns the control procedure corresponding to the currently-active coroutine.

Whenever a control procedure is called, the caller's own control procedure is immediately updated
such that calling it will to return to the point of this call.  That is, transferring control back
to the caller at some later time corresponds to the caller returning from its original call.  The
callee's coroutine then becomes the active one.

When the callee becomes active after having become inactive due to calling another coroutine, then
it will behave like it is returning from that earlier call.  The arguments that were just passed to
the callee become the return values of that earlier call.

In other words, calling a coroutine resumes that coroutine's computation by returning from its own
last call to a coroutine.
- And the call arguments become the return values of the resumption.

If the callee is becoming active for the first time, the arguments that were just passed to it
become the arguments of the `proc` in the `(make-coroutine register-value proc)` that created
the callee.

It is safe for a coroutine to transfer control directly to itself.  In that case, the call arguments
immediately become the return values, as if `values` had been called instead.

## Control operators

- No first-class control operator will copy any part of the current continuation/stack
  - Each coroutine uses a disjoint call stack
  - Creating a virtual thread produces a new coroutine
    - Invoking a virtual thread transfers control to its coroutine, computes until the given
      resource budget is exhausted, then transfers control back to the invoking coroutine
  - Nonvirtual threads do not involve transfers of control and are not first-class values

- Dynamically-scoped parameters can be implemented by storing a coroutine-local scope value:
  - The scope can adhere to this grammar:
    ```
    SCOPE ::= (cons (cons KEY VALUE) SCOPE) ; a binding followed by more bindings
            | (mvector SCOPE)               ; a reassignable link to more bindings
            | #f                            ; empty, no bindings available
    ```
    - Lookup will scan bindings in order until it finds the desired key, or `#f` if the desired key
      is not bound, following any `mvector` links it encounters along the way.
  - By using the local register, each coroutine remembers its own set of dynamic bindings.
  - Coroutine calls are symmetric, and a coroutine does not automatically remember its caller.  This
    is a fine default for coroutines that act as virtual threads or coroutines that are not intended
    to behave as if they are nested within one another.  But in some cases, such as generators or
    structured concurrency, where there should be a parent-child relationship, we want to call a
    child coroutine in a way that it can access dynamically-scoped parameters from the parent.  And
    this parent coroutine may not be fixed.  It could change from call to call.  For instance, a
    single generator may be partially consumed from one site, and then partially consumed from a
    different site.
    - To support access to dynamically-scoped parameters from a parent coroutine, where that parent
      may change, we can set the child's initial scope value to include a base
      `(mvector ,parent-scope)` cell.  When a parent calls the child coroutine, it should install
      its own scope value in the child's cell.
  - Alternatively, instead of an embedded `mvector`, we could traverse the outer parent link that is
    stored in `current-coroutine-register` alongside the dynamic scope value(s).

- Control handlers for `panic` and timed interruption
  - Unlike a coroutine register, these handler settings are OS-thread-local.
    - i.e., these settings do not stick to each coroutine, but do stick to an OS-level thread.
    - To implement `panic` handling that sticks to a coroutine, set an initial handler that consults
      `current-coroutine-register` to find the coroutine-local handler to dispatch to.
      - Don't forget to have the initial handler re-establish itself.
  - When `panic` is invoked, interrupts are disabled (disable-interrupts-count is incremented), and
    the panic-handler is reset to `#f` after being retrieved.
    - Setting the panic-handler to `#f` prevents infinite looping in case we `panic` while running
      the panic handling procedure itself.
    - If a panic-handling procedure is meant to persist across multiple panics, then it is
      responsible for re-establishing itself.
    - If the panic-handler wishes to resume normal computation, it is responsible for re-enabling
      interrupts when appropriate.
    - If a panic-handler intends to recover, it should do so by an explicit control transfer, not
      by returning normally.  By calling `panic` in the first place, we indicate that we wish to
      leave a failed computation, so the call's continuation is not safe to resume.  As a safeguard,
      if a panic-handler returns normally, it will invoke the platform's default panic-handler,
      which will likely exit the program.
  - When the interrupt timer expires, the timer-interrupt-handler is not reset, and so does not need
    to be re-established to persist.
    - It is dangerous to restart the timer with `set-timer` within the timer-interrupt-handler if
      making further calls, such as an explicit control transfer before the end of the handler.
      Setting the timer here risks making negative progress towards the remaining computation, as
      the handler's own calls may consume some of the allocated time.  It is safer to call the
      control recipient with the desired tick count, and make them responsible for calling
      `set-timer` right before resuming normal computation.
  - Non-timer interrupts do not invoke the timer-interrupt-handler

- Nestable preemptive multitasking
  - Related: https://legacy.cs.indiana.edu/~dyb/pubs/engines.pdf
  - Built on virtual timer interrupts and cooperative control transfers
    - `timer-interrupt-handler`
      - called automatically when time budget is exceeded
    - `(set-timer new-ticks) ==> previous-remaining-ticks`
      - if `ticks` is zero, the corresponding budget is unbounded
    - `(enable-interrupts) ==> decremented-disable-count`
    - `(disable-interrupts) ==> incremented-disable-count`
    - `(make-coroutine register-value proc)`
      `(current-coroutine)`
      `(current-coroutine-register)`
  - Engines and preemptive threads are managed with thread states that coordinate timed interrupts.
  - A thread state has this representation:
    - ```
      (mvector
        ,resume        ; the control procedure for resuming this thread's coroutine
        ,child         ; #f if this thread is not currently acting as a scheduler
        ,parent        ; #f if root or detached
        ,next-ancestor ; #f, or the ancestor to be resumed when the timer interrupt fires
        ,ticks         ; #f to run until voluntary pause, negative if next-ancestor has more ticks
        ,status        ; one of these symbols: running, ready, blocked, done
        ,signal?       ; #f, stop, or terminate, where terminate overrides stop
        )
      ```
  - The current thread's state is stored alongside the scope stored in `current-coroutine-register`,
    with this structure:
    - ```
      (mvector
        ,thread-state  ; #f if detached
        ,parent        ; #f if root scope, searched for more dynamic bindings, reassignable for generators and engines
        ;; dynamic bindings for the current scope
        ,parameters
        ,condition-handlers
        ,restarts-and-finalizers
        ,etc. ...
        )
      ```
  - Thread control signals:
    - A thread state can be requested to stop (pause evaluation indefinitely) or terminate early by
      setting the `signal-requested?` field.  When the timer-interrupt-handler is deciding how to
      resume, it will `raise` a signal condition if `signal?`.
      - The program's root scheduler is likely listening for operating system signals and
        translating them into appropriate thread control signals.
        - e.g., SIGTSTP, SIGTERM on POSIX systems
      - (thread-terminate t force?)  SIGTERM-like if force? is #f, otherwise SIGKILL-like
      - (thread-stop      t force?)  SIGTSTP-like if force? is #f, otherwise SIGSTOP-like
      - if force? = #f, the default handlers for each should re-invoke with force? = #t on
        the current thread (because the signalled thread is the one running the handler) to
        complete the signalled action
        - user-defined handlers should finish their cleanup, then do the same force? = #t for
          the same reason
    - To safely stop or terminate a thread it must be possible to asynchronously interrupt it, and
      give it a chance to clean up or otherwise prepare.
      - This is similar to Racket's `break`.
      - A signal will be communicated with `raise` and its condition can be handled in the usual way.
    - If a signal arrives while a thread is blocked, the condition we `raise` will indicate this in
      case the handling behavior should differ in this situation.
  - Schedulers for preemptive threads should respect structured concurrency:
    - A scheduler should scope the the lifetime of all threads they run.
      - The scheduler is only finished once all of its threads are finished.
    - A protective catch-all error handler needs to be installed to gracefully terminate all of a
      scheduler's threads before propagating the error.
    - A catch-all signal handler needs to be installed to prevent threads from leaking the signals
      sent to them.
  - The installed timer-interrupt-handler is responsible for updating the tick budget for a chain of
    nested thread states, transfer control to the appropriate point in the chain, and either deliver
    pending signals with `raise`, or just return to resume normally.
    - When `interrupt` is invoked, we determine how many unused ticks remain using
      `ticks <- (set-timer 0)`.  If `ticks` is zero, then it means `interrupt` was invoked
      because the timer ran out.  If `(> ticks 0)`, it means the youngest currently-running thread
      has voluntarily paused, so we resume its parent after `(set-timer ticks)` to make those ticks
      available to the parent.
    - When the timer runs out, it means one of the currently-executing threads has run out of ticks.
      We determine which thread expired by following next-ancestor links until we find one with
      negative ticks, or its next-ancestor is `#f`.
      - Having negative ticks means that the next-most-constrained thread, which is one of the
        expired thread's ancestors, has `(- ticks)` more ticks than the expired thread.
      - We resume the parent of the expired thread after `(set-timer (- ticks))` to make those
        additional ticks available to the parent.
    - When a thread runs a chain of nested threads for some number of `new-ticks`, we first assign
      `ticks` in the top thread of the chain to be `new-ticks`, then we compare `new-ticks` with
      `ticks-remaining <- (set-timer 0)` as well as update tick accounting and next-ancestor links.
      - If `(<= ticks-remaining new-ticks)` then the chain we are starting may be less constrained
        than one of the currently-running threads, reachable from `next-ancestor`.  We walk the
        chain downward, decrementing `ticks-remaining` from each thread's `ticks` as long as
        `(<= ticks-remaining ticks)`, and setting its `next-ancestor` to the current
        `next-ancestor`.  If we get to the bottom this way, then the entire chain was less
        constrained.  Otherwise, if we encounter a thread where `(< ticks ticks-remaining)`, then
        this thread is more constrained.  We set its ticks to be `(- ticks ticks-remaining)`, which
        is negative, and continue the traversal downward, using this more-constrained thread as the
        `next-ancestor` below, and using `ticks` as `ticks-remaining`.
      - If `(< new-ticks ticks-remaining)` then the chain we are starting is immediately more
        constrained than `next-ancestor`.  We follow the same downward process as above, but we
        start with the top of our chain as `next-ancestor`, set its `ticks` to
        `(- new-ticks ticks-remaining)`, which should be negative, and use `new-ticks` as
        `ticks-remaining` as we traverse.
      - If `ticks-remaining` is ever updated to `0` it means we came across a thread which is
        already expired.  We can stop the traversal early, resuming its parent after
        `(set-timer previous-ticks-remaining)`.
    - Any thread with `#f` `ticks` is considered to be running indefinitely, until voluntarily
      pausing.  No arithmetic updating needs to be done on their `ticks`.  We just treat these like
      infinities for the purpose of the processes described above.  For instance, these threads will
      never be the most-constrained, and so will only be resumed either when they are the youngest
      in the chain, or when an immediate child pauses, either voluntarily, or because it ran out of
      ticks.  This implies that a parent with `#f` `ticks` that is running a child, also with `#f`
      `ticks`, then the parent will never be resumed due to simple tick exhaustion.

- A system signal is a meta-level concept and should not be implicitly dispatched as an asynchronous
  exception.  Instead, the platform should provide a way to register explicit signal handlers and
  use cooperative control transfers, if any.
  - These include hardware-level interrupts, such as those coming from input devices.
    - These do not include the virtual interrupts for memory management and the interrupt-timer,
      which must interrupt a user program asynchronously.
  - If desired, the signal handler can `raise` asynchronous conditions to implement a multitasking
    system.
  - An alternative to preemptive multitasking is to provide an input channel/stream that we can
    choose to synchronously listen to for signals.
    - e.g., we can have a thread that blocks on this channel, and translates incoming signals
      to events that can be consumed by other threads, possibly downstream.
  - Signals are handled top-down, not bottom-up, and their handling mechanism can be virtualized.
    That is, the host system sees a signal first, and optionally delegates its handling to a
    subprocess via the same channel-like mechanism as described above, with the subprocess behaving
    as a host itself.
  - Consider a main meta-level program (such as an IDE or debugger) that is evaluating an
    object-level program in time slices using a virtual-thread, and checking for signals after
    each time slice.  If the user presses ctrl-c or ctrl-z on the keyboard, the corresponding
    signal will be received by the meta-level program, which might react by either terminating
    (for ctrl-c) or pausing (for ctrl-z) the object-level program virtual-thread.

- Parallel processing
  - Platform-specific primitives for spawning parallel threads/processes
  - `(mvector-cas! mv i expected new) ==> boolean`
  - High-level synchronizable actions interface
    - as in Concurrent ML and Racket synchronizable events

- Programming mistakes should `panic`, not `raise`
  - Examples of programming mistakes:
    - assertion violations
    - type errors
    - calling a procedure with the wrong number of arguments
    - accessing an out-of-bounds index
    - accessing an uninitialized variable (possible with letrec)
    - arithmetic errors such as:
      - divide-by-zero
      - overflow
      - underflow
  - The compiler will generate code for implicit error-checking that calls panic upon failure
    - This implicit error-checking will take place inside primitive operator definitions, sites that
      call unknown procedures, arity-checking preludes of non-primitive procedure definitions, and
      sites that reference variables whose initialization status is unknown (possible with letrec)
  - Panic handling is independent of raise handling and restart handling by default
    - `panic` behavior is determined by current-panic-handler
- Errors or failures that are not programming mistakes should `raise`
  - e.g., IO errors
  - Unlike `panic`, `raise` is not a primitive, it can be defined in a library

## Numbers

- Primitive numeric types: `rational integer f32 f64`
  - A `rational` is an exact, arbitrary-precision integer or fraction.
  - `integer` is a subtype of `rational`.  An `integer` is an exact, arbitrary-precision integer.
    - Primitive operators for `integer` values include:
      `bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right bitwise-not bitwise-and bitwise-ior bitwise-xor integer-floor-divmod`
      - For consistency with bitwise arithmetic shifting on negative integers,
        `integer-floor-divmod` performs a flooring division/modulo operation rather than the typical
        truncating operation.
        - To understand the motivation for this choice, see:
          [Arithmetic Shifting Considered Harmful](https://dspace.mit.edu/bitstream/handle/1721.1/6090/AIM-378.pdf)
  - A `f32` or `f64` is an inexact, IEEE 754 floating-point value represented with 32 or 64 bits.
    - Primitive operators for `fN` values include:
      `fN-cmp fN-floor fN-ceiling fN-truncate fN-round fN+ fN- fN* fN/`

## A possible "environment soup" model for interactive evaluation

For a well-behaved "top-level", make it clear when a group of definitions
and/or expressions should be evaluated in the same environment, producing a new
environment.

- An interactive evaluation session works with a collection of these evaluation
  groups, which may acyclically depend on each other through explicit linking.
- When a child group depends on one or more parent groups, it means that the
  environments produced by the parents are sequentially composed, with bindings
  in later environments shadowing earlier ones, producing the environment that
  the child group's definitions and expressions are evaluated in.
  - A child may also filter or rename the bindings inherited from a parent.
- Definitions can only be mutually recursive with other definitions from the
  same group.
- Changes to a group can optionally be propagated to its dependent groups, with
  live upgrade.

## Old TODO that needs to be reorganized

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
  * stdin/stdout/stderr, stty/terminfo, filesystem, network sockets, gui
  * threads/places/futures, timers, sleep, exception/break/interrupt handling
    * with-handlers, uncaught-exception-handler, call-with-exception-handler
    * exn:break?, exn:break-continuation (usable via ueh or cweh, not with-handlers)
    * (parameterize-break #t|#f body ...)
  * cmdline/shell/env/subprocesses, racket-eval
  * crypto-random-bytes
* define ports, read, write
  * start with high level ops for now, then reimplement in terms of lower level ops
    * open-input-file, open-output-file, close-input-port, close-output-port, flush-output
    * high level: eof?, printf, write-string, write, read-string, read
    * low level: raw unbuffered io streams with block read/write
      * implement ports on top of these
      * read-bytes!, write-bytes, file-position (via `*` version for safety)
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
* symbolic evaluation
* abstract interpretation
* theorem proving
* functional logic/relational programming
* incremental/adaptive computation
* automatic differentiation
* probabilistic programming

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
