(splicing-local
  ((define (primitive*->env primitive*)
     (let ((env.scope (make-env)))
       (for-each (lambda (id) (env-bind! env.scope id vocab.expression
                                         (parse/constant-expression ($prim id))))
                 primitive*)
       (env-freeze! env.scope)
       env.scope)))
  ;; TODO: provide low-level, possibly platform-dependent, privileged primitives
  ;; for allocation, mutation, interrupts, control flow, etc.
  ;;
  ;; A compiler may transform some uses of high-level primitives into uses of these low-level
  ;; primitives.  A runtime may also implement high-level primitives in terms of these low-level
  ;; primitives.  For instance, we may use these to define a single bignum arithmetic implementation
  ;; that is mostly-portable across multiple platforms.
  ;;
  ;; There will be enough similarities across platforms that different runtime implementations can
  ;; share a large amount of code if these low-level primitives are used.  Many details of
  ;; allocation and garbage collection, memory layouts, tag manipulation, stack and procedure
  ;; metadata, virtual interrupts, etc. will be similar.
  ;;
  ;; Because they assume the use of raw, untagged values, these low-level primitives are going to be
  ;; extremely unsafe to use in normal code.  For instance, interrupts need to be carefully managed
  ;; to avoid garbage collection while untagged numbers and addresses are still reachable.  Limiting
  ;; their use to code written in a representation-type-checked sublanguage will be safer.
  ;(define env.primitive.low-level
  ;  (make-array word-count) ==> addr  ; elements are machine-words
  ;  (memory-u{8,16,32,64}-ref  addr offset)
  ;  (memory-u{8,16,32,64}-set! addr offset x)
  ;  (address+ addr size) => addr
  ;  (address- addr addr) => size  ; is result signed, or does left operand have to be larger?
  ;  (call-address addr arg ...)  ; may need type info to respect platform ABI w.r.t. floats
  ;  raw (untagged) sN,uN,fN arithmetic and bitwise ops, with carry, overflow, etc.
  ;  extend/truncate between different N of sN,uN
  ;  vectorized ops
  ;  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Platform-independent capabilities ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define env.primitive.privileged
    (primitive*->env
      '(
        panic set-panic-handler!
        procedure-metadata
        record? record record-type-descriptor record-ref
        ;; TODO: use these to implement string->utf8 utf8->string via a utf8? check
        string->bytevector bytevector->string)))
  (define env.primitive
    (primitive*->env
      '(
        apply call-with-values values
        eq? eqv? null? boolean? procedure? symbol? string? rational? integer? f32? f64?
        pair? vector? mvector? bytevector? mbytevector?
        string->symbol symbol->string
        cons car cdr
        vector vector-length vector-ref
        make-mvector mvector->vector mvector-length mvector-ref mvector-set!
        bytevector bytevector-length bytevector-u8-ref
        make-mbytevector mbytevector->bytevector mbytevector-length
        mbytevector-u8-ref mbytevector-u8-set!
        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length integer-floor-divmod
        numerator denominator = <= >= < > + - * /
        ;; TODO: these shouldn't be primitive.  We should be able to use the bitwise representation
        ;; to compute the nearest exact rationals.
        f32->rational rational->f32 f64->rational rational->f64
        ;;; NOTE: u32->f32 and u64->f64 must quiet any NaNs produced.
        f32->u32 u32->f32 f64->u64 u64->f64 f32->f64 f64->f32
        f32-cmp f32-floor f32-ceiling f32-truncate f32-round f32+ f32- f32* f32/
        f64-cmp f64-floor f64-ceiling f64-truncate f64-round f64+ f64- f64* f64/)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Platform-dependent capabilities ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define env.primitive.control
    (primitive*->env
      '(
        current-control-context make-control-context
        ;; TODO: remove call-with-escape-continuation and call-in-empty-context
        call-with-escape-continuation call-in-empty-context
        control-context-register set-control-context-register!
        yield set-yield-handler! set-timer enable-interrupts disable-interrupts)))
  ;; TODO: since these are already platform-dependent, we should define these in terms of
  ;; lower-level native memory primitives.
  (define env.primitive.native-bytevector
    (primitive*->env
      '(
        bytevector-u16-ref bytevector-u32-ref bytevector-u64-ref
        mbytevector-u16-ref mbytevector-u32-ref mbytevector-u64-ref
        mbytevector-u16-set! mbytevector-u32-set! mbytevector-u64-set!)))
  )
