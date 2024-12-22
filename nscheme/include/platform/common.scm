(define package.common
  (cons
    '(
      panic apply values
      eqv? null? boolean? procedure? symbol? string? rational? integer?
      pair? vector? mvector? bytevector? mbytevector?
      utf8->string string->utf8 string->symbol symbol->string
      cons car cdr
      vector vector-length vector-ref
      make-mvector mvector->vector mvector-slice mvector-length mvector-ref mvector-set!
      bytevector bytevector-length bytevector-ref
      make-mbytevector mbytevector->bytevector mbytevector-slice
      mbytevector-length mbytevector-ref mbytevector-set!
      bitwise-asl bitwise-asr bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length
      integer-floor-divmod numerator denominator = <= >= < > + - * /)
    (list
      panic apply values
      eqv? null? boolean? procedure? symbol? string? rational? integer?
      pair? vector? mvector? bytevector? mbytevector?
      utf8->string string->utf8 string->symbol symbol->string
      cons car cdr
      vector vector-length vector-ref
      make-mvector mvector->vector mvector-slice mvector-length mvector-ref mvector-set!
      bytevector bytevector-length bytevector-ref
      make-mbytevector mbytevector->bytevector mbytevector-slice
      mbytevector-length mbytevector-ref mbytevector-set!
      bitwise-asl bitwise-asr bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length
      integer-floor-divmod numerator denominator = <= >= < > + - * /)))
