# nScheme

## TODO

### compiler

#### frontend

* minimalistic lisp
  * purpose: target multiple backend systems with one simple, self-processing language
  * real-world [effect?] interaction/reaction
    * monolithic request handling
      * value: `(value ,val)
      * request: `(request ,payload ,continuation)
      * continuation: -> `(lambda (response) ...)
      * eval: program -> value|request
      * handle: value|request -> ...
    * a non-monolithic channel-like alternative that also supports concurrency
      * (produce key value) => #t  ;; aka send, as in (send channel value)
      * (consume key) => value     ;; aka recv or receive
      * keys can be any eqv?-able data (e.g., numbers or mvectors)
      * user programs should not directly use produce/consume, or manage keys
        * host systems provide procedures that manage these interactions
          * for immediate, synchronous uses (e.g., system calls), compile
            produce/consume pairs into simple procedure calls
    * a thread-based alternative that just deals with concurrency
      * external interaction can be modelled as systems manipulating shared mvectors
      * (spawn definition-body ...) => thread-control-procedure?
      * (mvector-compare-and-swap! mvec index expected-value new-value) => boolean?
      * analyze mvector uses for efficient compilation:
        * along with aliasing, infer thread-local and uncontended mvector accesses
  * syntax extension ideas
    * hygienic, non-macro syntax extension inspired by f-exprs, but static
    * open eval recursion
    * set!-based eval (don't forget dynamic-wind protection)

* representation type system
  * an intermediate representation, but also a lower level user notation
  * aliasing/uniqueness annotations
    * e.g., mvector->vector need not copy if the mvector was unique and is no longer used
    * single/multi-threaded aliasing constraints can inform mvector optimizations
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


#### backend

* support runtime code generation and rebooting
  * ideally support separate compilation, which might make this easier
* support foreign interaction safely
  * must not violate optimization and linking assumptions
  * must not break garbage collection or other resource management


##### backend for racket

Support this both to bootstrap and to quickly support:
* a non-web host system
* a terminal interface


##### backend for web

* muJS
  * purpose: implement a simple RTL as a compilation target
  * data
    * undefined, null, booleans, js-int, js-float, strings, arrays, records?, jump-labels?
  * control flow
    * if, for (w/ labels), while, switch, functions
      * maybe these are too high-level to consider for a compilation target
    * MLton-style CPS/SSA tc/jump continuations?

* javascreme
  * purpose: implement a JS-like notation for building a runtime system
  * e.g., console.log, event handling, dom manipulation
    * maybe present an interface like SDL or pygame (would this require shift/reset?)


##### other backends to consider

WebAssembly, Python, C, Java, .NET, x86, ARM, ...


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
* basic props and inference rules include some minimal mix of: ->, ->*, |->, |->*, ~=, term induction
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
