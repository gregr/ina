(provide type-predicates primitive-op-descriptions)

;; TODO: build and sanity check compiler backend primitive op tables.
(define type-predicates
  (list (cons 'procedure? procedure?)
        (cons 'mvector?   mvector?)
        (cons 'vector?    vector?)
        (cons 'pair?      pair?)
        (cons 'null?      null?)
        (cons 'boolean?   boolean?)
        (cons 'string?    string?)
        (cons 'number?    number?)
        (cons 'integer?   integer?)
        (cons 'fixnum?    fixnum?)
        (cons 'flonum?    flonum?)))

(define primitive-op-descriptions
  (append (map (lambda (tp) (cons (car tp) '((#f) boolean?))) type-predicates)
          '((boolean=?   (boolean? boolean?)     boolean?)
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

            ;; TODO: derive these.
            (string<? (string? string?) boolean?)
            (string>? (string? string?) boolean?)
            ;; TODO: flonum variants.
            (=  (number? number?) boolean?)
            (<= (number? number?) boolean?)
            (<  (number? number?) boolean?)
            (>= (number? number?) boolean?)
            (>  (number? number?) boolean?)
            (+  (number? number?) number?)
            (*  (number? number?) number?)
            (-  (number? number?) number?)
            (/  (number? number?) number?)

            (truncate (number?) integer?)

            ;bitwise-and
            ;bitwise-ior
            ;bitwise-xor
            ;bitwise-not
            ;bitwise-bit-set?
            ;bitwise-bit-field
            ;arithmetic-shift
            ;integer-length
            )))

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
