# nScheme

## TODO

### web-based bootstrap

* text-based program construction
  * simplify existing reader/writer
    * syntax-object: (metadata, s-expr-containing-syntax-objects)
    * annotate: text-position-info -> metadata
    * read/annotate: annotate -> text -> syntax-object
    * write: syntax-object|s-expr -> text

* minimalistic lisp
  * purpose: implement a simple JS compiler to minimize amount of manually written JS
  * data
    * pairs, nil, booleans, symbols(, simple js-like numbers? or numbers as symbols?)
    * string representations for communicating with outside world:
      * ;; nested list representation allows efficient concat, but more complex to interpret externally
      * string ::= symbol | [string]
      * ;; a final normalization step on output can simplify external interpretation
      * normalized-string ::= [symbol]
  * real-world [effect?] interaction/reaction
    * request handling
      * value: `(value ,val)
      * request: `(request ,payload ,continuation)
      * continuation: -> `(lambda (response) ...)
      * eval: program -> value|request
      * handle: value|request -> ...
    * minimal web-platform effects
      * only need to convert symbol lists (normalized strings) to actual strings or text files
      * but some REPL and debugging support should be helpful
  * extensible denotational syntax
    * look at unsafe2 for an initial solution (rename seval to denote)
      * but depend less on dynamic definition of operatives in actual programs by using modules
    * hygienic, non-macro-based syntax extension inspired by f-exprs, but static
    * open denote/eval recursion
    * avoid leaking details about AST and environment representation

* muJS
  * purpose: implement a simple RTL as a compilation target
  * data
    * undefined, null, booleans, js-int, js-float, strings, arrays, records?, jump-labels?
  * control flow
    * if, for (w/ labels), while, switch, functions
      * maybe these are too high-level to consider for a compilation target
    * MLton-style CPS/SSA tc/jump continuations?


### logic

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


## References by topic

### De Bruijn indices and explicit substitutions
- [Wikipedia: De Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index)
- [How I learned to stop worrying and love de Bruijn indices](http://disciple-devel.blogspot.ca/2011/08/how-i-learned-to-stop-worrying-and-love.html)
- [Wikipedia: Explicit substitution](https://en.wikipedia.org/wiki/Explicit_substitution)
- Blitz introduction to de Bruijn indices and explicit substitution: [How to implement dependent type theory III](http://math.andrej.com/2012/11/29/how-to-implement-dependent-type-theory-iii/)

### Reduction systems
- [Explicit evaluation](http://fexpr.blogspot.ca/2013/07/explicit-evaluation.html)
- [Continuations and term-rewriting calculi](http://fexpr.blogspot.ca/2014/03/continuations-and-term-rewriting-calculi.html)
- Part II of [Fexprs as the basis of Lisp function application; or, $vau: the ultimate abstraction](https://www.wpi.edu/Pubs/ETD/Available/etd-090110-124904/unrestricted/jshutt.pdf)
