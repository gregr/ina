(define package.primitive
  (cons
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
