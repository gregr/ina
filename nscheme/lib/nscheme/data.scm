(provide type-predicates primitive-ops)

;; TODO: build and sanity check compiler backend primitive op tables.
(define type-predicates
  '(boolean?
    null?
    char?
    flonum?
    fixnum?
    integer?
    number?
    string?
    pair?
    vector?
    mvector?
    procedure?
    ))

(define primitive-ops
  '((mvector?   (#f) boolean?)
    (vector?    (#f) boolean?)
    (pair?      (#f) boolean?)
    (null?      (#f) boolean?)
    (string?    (#f) boolean?)
    (char?      (#f) boolean?)
    (number?    (#f) boolean?)
    (integer?   (#f) boolean?)
    (boolean?   (#f) boolean?)
    (procedure? (#f) boolean?)

    (boolean=? (boolean? boolean?) boolean?)
    (char=?    (char? char?)       boolean?)
    (string=?  (string? string?)   boolean?)
    (mvector=? (mvector? mvector?) boolean?)

    (char->integer  (char?)    fixnum?)
    (integer->char  (integer?) char?)
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

;; Sanity check primitive-ops.
(let ()
  (define (valid-type? t)
    (or (equal? #t t) (not t) (member t type-predicates)))
  (define (valid-primop? op)
    (match op
      (`(,name ,in* ,out)
        (guard (symbol? name))
        (and (valid-type? out)
             (andmap valid-type? in*)

             ))
      (_ #f)))
  (define malformed (filter-not valid-primop? primitive-ops))
  (unless (null? malformed)
    (error '"malformed primitive-ops:" malformed)))
