(splicing-local
  ((define (name*primitive*->env name* primitive*)
     (let ((env.scope (make-env)))
       (for-each (lambda (id p) (env-bind! env.scope id vocab.expression
                                           (parse/constant-expression ($quote p))))
                 name* primitive*)
       (env-freeze env.scope))))
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
  ;; TODO: generate this repetitive code to avoid careless mistakes:
  (define env.primitive.privileged
    (name*primitive*->env
      '(
        set-panic-handler!
        procedure-metadata
        record? record record-type-descriptor record-ref
        ;; TODO: use these to implement string->utf8 utf8->string via a utf8? check
        string->bytevector bytevector->string)
      (list
        set-panic-handler!
        procedure-metadata
        record? record record-type-descriptor record-ref
        ;; TODO: use these to implement string->utf8 utf8->string via a utf8? check
        string->bytevector bytevector->string)))
  (define env.primitive.privileged.control
    (name*primitive*->env
      '(
        native-thread-local-register with-raw-escape-prompt raw-escape-to-prompt
        current-raw-coroutine make-raw-coroutine
        set-timer-interrupt-handler! set-timer enable-interrupts disable-interrupts)
      (list
        native-thread-local-register with-raw-escape-prompt raw-escape-to-prompt
        current-raw-coroutine make-raw-coroutine
        set-timer-interrupt-handler! set-timer enable-interrupts disable-interrupts)))
  (define env.primitive
    (name*primitive*->env
      '(
        panic apply values
        eq? eqv? null? boolean? procedure? symbol? string? rational? integer?
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
        numerator denominator = <= >= < > + - * /)
      (list
        panic apply values
        eq? eqv? null? boolean? procedure? symbol? string? rational? integer?
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
        numerator denominator = <= >= < > + - * /)))
  )
