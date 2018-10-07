(provide type-predicates primitive-op-descriptions)

;; TODO: build and sanity check compiler backend primitive op tables.
(define type-predicates
  (list (cons 'boolean?   boolean?)
        (cons 'null?      null?)
        (cons 'flonum?    flonum?)
        (cons 'fixnum?    fixnum?)
        (cons 'integer?   integer?)
        (cons 'number?    number?)
        (cons 'string?    string?)
        (cons 'pair?      pair?)
        (cons 'vector?    vector?)
        (cons 'mvector?   mvector?)
        (cons 'procedure? procedure?)))

(define primitive-op-descriptions
  '((mvector?   (#f) boolean?)
    (vector?    (#f) boolean?)
    (pair?      (#f) boolean?)
    (null?      (#f) boolean?)
    (string?    (#f) boolean?)
    (number?    (#f) boolean?)
    (integer?   (#f) boolean?)
    (boolean?   (#f) boolean?)
    (procedure? (#f) boolean?)

    (boolean=?   (boolean? boolean?)     boolean?)
    (number=?    (number? number?)       boolean?)
    (string=?    (string? string?)       boolean?)
    (mvector=?   (mvector? mvector?)     boolean?)
    (procedure=? (procedure? procedure?) boolean?)

    (string->vector (string?)  vector?)
    (vector->string (vector?)  string?)

    (cons (#f #f) pair?)
    (car  (pair?) #f)
    (cdr  (pair?) #f)

    (vector-ref    (vector? fixnum?) #f)
    (vector-length (vector?)         fixnum?)

    (make-mvector    (fixnum? #f)          mvector?)
    (mvector->vector (mvector?)            vector?)
    (mvector-set!    (mvector? fixnum? #f) #t)
    (mvector-ref     (mvector? fixnum?)    #f)
    (mvector-length  (mvector?)            fixnum?)

    ;; TODO: flonum variants.
    (=  (number? number?) boolean?)
    (<= (number? number?) boolean?)
    (<  (number? number?) boolean?)
    (+  (number? number?) number?)
    (*  (number? number?) number?)
    (-  (number? number?) number?)
    (/  (number? number?) number?)

    ;bitwise-and
    ;bitwise-ior
    ;bitwise-xor
    ;bitwise-not
    ;bitwise-bit-set?
    ;bitwise-bit-field
    ;arithmetic-shift
    ;integer-length

    ;round
    ;quotient
    ;remainder
    ))

;; Sanity check primitive-op-descriptions.
(let ()
  (define (valid-type? t)
    (or (equal? #t t) (not t) (assoc t type-predicates)))
  (define (valid-primop? op)
    (and (list? op) (= 3 (length op)) (string? (car op))
         (andmap valid-type? (cadr op))
         (valid-type? (caddr op))))
  (define malformed (filter-not valid-primop? primitive-op-descriptions))
  (unless (null? malformed)
    (error '"malformed primitive-op-descriptions:" malformed)))
