(splicing-local
  ((define (primitive*->env primitive*)
     (let ((env.scope (make-env)))
       (for-each (lambda (p) (let* ((pm   (procedure-metadata p))
                                    (id   (if (and (vector? pm)
                                                   (= (vector-length pm) 2)
                                                   (eq? (vector-ref pm 0) 'primitive))
                                              (vector-ref pm 1)
                                              (error "not a primitive" p)))
                                    (addr (identifier->fresh-address id)))
                               (env-bind! env.scope id addr)
                               (env-set!  env.scope vocab.expression addr
                                          (parse-variable-ref/address addr))))
                 primitive*)
       (env-extend env.empty env.scope))))

  (define env.primitive.privileged
    (primitive*->env
      (list
        ;; TODO: include these later
        ;call-with-escape-continuation call-in-empty-context
        ;thread-register set-thread-register!
        ;panic-handler set-panic-handler!
        ;set-time-budget time-exceeded-handler set-time-exceeded-handler!
        ;set-space-budget space-exceeded-handler set-space-exceeded-handler!
        procedure-metadata
        svector? svector->vector vector->svector
        ;; TODO: use these to implement string->utf8 utf8->string via a utf8? check
        string->bytevector bytevector->string)))

  (define env.primitive
    (primitive*->env
      (list
        panic apply call-with-values values
        eq? eqv? null? procedure? symbol? string? rational? integer? f32? f64?
        pair? vector? mvector? bytevector? mbytevector?

        string->symbol symbol->string
        cons car cdr
        vector-length vector-ref
        make-mvector mvector->vector mvector-length mvector-ref mvector-set!

        bytevector-length bytevector-b8-ref bytevector-b16-native-ref bytevector-b32-native-ref bytevector-b64-native-ref
        make-mbytevector mbytevector->bytevector mbytevector-length
        mbytevector-b8-ref mbytevector-b16-native-ref mbytevector-b32-native-ref mbytevector-b64-native-ref
        mbytevector-b8-set! mbytevector-b16-native-set! mbytevector-b32-native-set! mbytevector-b64-native-set!
        native-big-endian?

        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        bitwise-not bitwise-and bitwise-ior bitwise-xor integer-floor-divmod
        numerator denominator cmp + - * /

        f32->f64 f64->f32
        f32->rational rational->f32 f64->rational rational->f64
        ;;; NOTE: b32->f32 and b64->f64 must quiet any NaNs produced.
        f32->b32 b32->f32 f64->b64 b64->f64
        f32-cmp f32-floor f32-ceiling f32-truncate f32-round f32+ f32- f32* f32/
        f64-cmp f64-floor f64-ceiling f64-truncate f64-round f64+ f64- f64* f64/))))
