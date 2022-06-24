# nScheme

This is a nonstandard Scheme for programming portable, performant, image-based
systems that can run forever.

Compared with standard Scheme, the language semantics and data model have been
simplified.  This simplification supports more aggressive compiler optimization
while also simplifying the implementation itself.  A simpler implementation
is easier to port and adapt to new purposes.

Rather than piling feature on top of feature, nScheme provides a small set of
virtualization primitives that let the programmer decide how their system works
at any level.  These primitives allow programs to exert low-level control over
computing resources, making it feasible to build any kind of application.
These primitives also provide reflection and introspection capabilities without
interpretive overhead.  Unlike standard Scheme, nScheme does not prescribe any
particular form of program organization or approach to metaprogramming.
Transformations and features that would typically be built into a complex,
ahead-of-time compilation model can instead be achieved at runtime in a
straightforward way by manipulating programs and live processes as data.

## Deviations from Scheme

- Some conventions from Racket are adopted:
  - Order of evaluation is left-to-right
    - e.g., in a procedure application, first the operator is evaluated, then
      the operands are evaluated in the order they appear.
  - Internal definitions may appear after expressions
  - `letrec` behaves like Scheme's `letrec*`
- Expressions that would typically return `(void)` (like set!, non-matching
  cond/case, etc.), instead return 0 values (i.e., `(values)`)
- variables are immutable by default
  - `set!` is only supported for variables introduced by special binders
- All S-expression types are immutable
  - This even includes pairs, strings, bytevectors, and vectors
  - Mutable vectors and bytevectors (i.e., mvectors and mbytevectors) are a
    distinct type of data
  - Special vectors (i.e., svectors) are a distinct type of immutable vector.
    These are used to represent immutable record types that are disjoint from
    the s-expression types.
- Numbers, pairs, strings, bytevectors, and vectors don't have stable identities
  - i.e., result of `eq?` on these types is unpredictable for structurally
    identical values
    - the result will always be `#f` for values that differ structurally
  - `eqv?` will return `#t` for structurally identical numbers
- Booleans, null, symbols, procedures, mbytevectors, and mvectors do have stable
  identities
  - i.e., the behavior of `eq?` is analogous to its behavior in R7RS Scheme
- No primitive character type: a string is indivisible until it is (utf-8) decoded as a vector
  - utf-8 encoding is assumed for vector and list conversions
  - no `string-ref`, since basis of decomposition depends on context
    - well, we could still support a O(n) `string-ref` based on a stream of code points
  - code units (bytes) are not characters, but suffice for low-level string manipulation
  - code points are not characters, but suffice for some higher-level string manipulation
  - grapheme clusters (substrings) are the right notion of character for user interaction
- A numerical tower inspired by: https://www.deinprogramm.de/sperber/papers/numerical-tower.pdf
  - Exact/inexact arithmetic that emphasizes "exactness" on operators rather than values
    - Floating-point values will still exist, but a mismatch will either be
      coerced or signal an error
  - Most of the usual numeric operators will only be applicable to exact numbers
    - Typical arithmetic operators could coerce to exact
      - Different from `fx+`, etc., which assume arguments are already in some exact format
    - A separate set of operators will be provided for manipulating inexact numbers
      - Possibly `.+`, `.*`, `.<=`, etc., which coerce to inexact
      - These are different from `fl+`, etc., which assume arguments are already inexact
  - `integer?` `rational?` etc. will return `#f` for all inexact numbers.
  - All numeric literals describe exact numbers by default.  Use `#i` for inexact literals.
  - If built-in complex numbers are provided, they will always be inexact.
- First-class control operators produce continuations that are delimited and one-shot.
- There is no prescribed object-level error handling model.
  - You can define your own handling using the virtualization primitives.
- The empty symbol `||` is used as an implicit keyword for all non-keyword
  syntax, including literals, variable references, and procedure application.
  - The behavior of these forms can be changed by redefining this keyword.
  - `5` is parsed as `(|| 5)`
  - a normal variable reference `x` is parsed as `(|| x)`
  - `(f e ...)` where `f` is not a syntactic keyword, is parsed as `(|| (f e ...))`

## Run tests

~~`raco test *.rkt`~~

`time racket test.rkt`

## Bootstrap

TODO

## TODO

- consider supporting Olin's multi-return function calls

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
  - `#<symbol>` and `#<symbol>(...)` programmable reader

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
    - () #t #f eof void undefined etc.
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
    - () #t #f eof void undefined etc.
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

## Ideas about control operators

- efficient delimited continuations and dynamic binding
  - continuation-procedure-builder for one-shot continuation procedures
    - multi-shot continuation procedures could be expressed by wrapping the continuation-procedure-builder
      - but we probably don't want built-in support for multi-shot semantics
  - maybe we should not implement delimited continuations directly
    - instead, implement exceptional aborts, nestable pre-emptive multitasking, and threads with synchronous channels
      - exceptional abort handlers can guarantee cleanup of resource/thread-nursery custodians
    - one-shot delimited continuations can be expressed by threads
      - and non-primitive pre-emptive multitasking is awkward to express via delimited continuations

- error handlers that begin in the dynamic context of a signal being raised
  - supports restarts without needing to abort and then resume

- control operators
  - call-in-os-thread
  - delimited
    - suspend, abort, capture (aka control0)
    - catch/suspend, catch/abort, catch/capture (aka prompt0)
    - dynamic context
      - can use these to implement dynamic binding (aka parameterize)
      - `(call-with-current-context prompt-tag f)`
        - `f : context -> any`
        - for inspecting context marks

- alternative design:
  - use hierarchically-subclassed prompt-tags
  - only provide catch, abort, call-with-dynamic-context, are these right?: call-with-interruption (and a fuel/tick count), call-without-interruption, time-until-interruption
    - but how do we efficiently handle nested interrupts?
    - previously we could use a tie-back continuation, such that a parent could jump directly to a deeply-nested child computation
    - can we still do this efficiently by structuring the context/stack according to hierarchical prompt tags?
    - aside from alarms, interruptions should include garbage collection and possibly other asynchronous signals that may optionally be handled?

- alternative alternative design:
  - We will provide the catch/suspend catch/abort catch/capture interface to end users.
  - But we will implement these using:
    - `catch : tag -> (() -> initial-result) -> (exceptional-request -> exceptional-result) -> (initial-result -> normal-result) -> (normal-result or exceptional-result)`
    - `abort-to : tag -> _no-return_`
      - this interface won't make sense, because call-with-delimited-context will end up capturing the abort-to!
    - `call-with-delimited-context : tag -> delimited-context`
      - a delimited context is a list of (tagged-frame or parameter-assignment)s
        - a tagged-frame combines a tag, a delimited continuation, an exception handler, and a success handler
    - need these unsafe versions instead?
     - `unsafe-abort-to : undelimited-context -> _no-return_`
     - `apply-delimited-context : delimited-context -> result-of-delimited-context`
     - `call-with-unsafe-undelimited-context : () -> undelimited-context`
       - it is an error to retain and use an undelimited-context after returning or aborting to an earlier point than it represents
       - i.e., the prefix of that context will have been invalidated, and may unsafely point to bogus memory
       - unless we don't care about stack-based implementations
         - heap-based contexts should be safe to retain, though they have call/cc-like abort semantics, which isn't great
     - `undelimited-context-car -> undelimited-context -> frame`
     - `undelimited-context-cdr -> undelimited-context -> undelimited-context`
     - `delimited-context-empty`
     - `delimited-context-cons : frame -> delimited-context -> delimited-context`
     - `alarm-start : num-ticks -> ()`, `alarm-stop : () -> num-ticks-remaining`
     - some way to disable garbage collection and other interrupts

- We should also be able to inspect marks on continuation segments for suspended computations
- nested engines via a pre-emptive subcomputation scheduling delimiter
  - `(suspend)`
  - `(catch/suspend thunk fuel-amount k.suspended k.finished)`
  - `k.suspended : (() -> any) -> any`
  - `k.finished  : (fuel-remaining any) -> any`
  - we can suspend through other delimiter boundaries

- Should we merge abort-catching with capture-catching, via hierarchical prompt-tags?
  - then the difference between capture and abort would be that abort doesn't bother building a continuation
- abort-catching handlers for safe resource management
  - `(abort value)`
  - `(catch/abort thunk k.aborted k.finished)`
  - `k.aborted  : any -> any`
  - `k.finished : any -> any`
  - we can abort through a `catch/suspend` boundary
  - we can abort through a `catch/capture` boundary

- multi-prompt multi-shot delimited continuations
  - note: we probably don't want to support multi-shot continuations
  - `(capture prompt-tag value)`
  - `(catch/capture prompt-tag thunk k.captured k.finished)`
  - `k.capture  : ((any -> any) any) -> any`
  - `k.finished : any -> any`
  - we can capture through a `catch/abort` boundary
  - we cannot capture through a `catch/suspend` boundary
  - prompt-tags are hierarchically subclassed
    - catch/capture with a parent prompt-tag will catch captures thrown with child prompt-tags

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
    * Instead of `X/enable-break` variants of `X` to safely perform the op or break, but
      not both, provide a synchronizable event for temporarily enabled breaks, e.g.,
      `(sync (choice-evt X break-evt))`.
      Also provide a `timeout-evt` for similar orthogonality.

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
