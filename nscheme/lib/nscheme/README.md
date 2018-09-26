# nScheme language implementation library

## TODO

### bootstrap with only simple code

* Implement bootstrap interpeter for nScheme in Racket
  * don't need import/export primitives
  * instead of poorly emulating nScheme in Racket as we're doing now
  * must define a compatible apply
    * apply must support passing non-list argument to a variadic procedure

* Define $ (staged eval) for syntactic extension

* flexible module body interpretation: (language evaluation-adaptor ...)

* small-step evaluation with transparent values
  * for interleaving evaluation and compilation
    * guarantees serializability of residual programs
    * e.g., transformations such as inlining, staged compilation
    * flexible syntactic extension
      * parsers can safely produce code containing computed values
        * the computed values will be serializable, amenable to analysis
