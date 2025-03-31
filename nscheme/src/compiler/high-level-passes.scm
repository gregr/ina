(splicing-let
  ((name=>primitive
     (aquote
       panic apply values make-record-type
       eqv? null? boolean? procedure? symbol? string? rational? integer?
       pair? vector? mvector? bytevector? mbytevector?
       bytevector->string string->bytevector string->symbol symbol->string
       cons car cdr vector vector-length vector-ref
       make-mvector mvector->vector mvector-length mvector-ref mvector-set!
       bytevector bytevector-length bytevector-ref
       make-mbytevector mbytevector->bytevector mbytevector-length mbytevector-ref mbytevector-set!
       bitwise-asl bitwise-asr bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length
       integer-floor-divmod numerator denominator = <= >= < > + - * /
       make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
       current-thread-group make-thread-group current-thread thread thread-wait thread-dead-evt
       sync sync/default handle-evt choice-evt guard-evt nack-guard-evt replace-evt never-evt
       make-channel channel-get channel-put channel-put-evt
       current-platform)))
  (define addr=>primitive-id (map (lambda (n) (cons (make-address n #f) n)) (map car name=>primitive)))
  (define primitive=>addr (map cons (map cdr name=>primitive) (map car addr=>primitive-id))))

(define (E-map-quote E f)
  (let loop ((E E))
    (cond
      ((E:quote?        E) (f E))
      ((E:ref?          E) E)
      ((E:if?           E) (E:if (E-note E) (loop (E:if-condition E))
                                 (loop (E:if-consequent E))
                                 (loop (E:if-alternative E))))
      ((E:call?         E) (E:call (E-note E) (loop (E:call-operator E))
                                   (map loop (E:call-operand* E))))
      ((E:apply/values? E) (E:apply/values (E-note E) (loop (E:apply/values-operator E))
                                           (loop (E:apply/values-operand E))))
      ((E:case-lambda?  E) (E:case-lambda (E-note E) (E:case-lambda-param*~* E)
                                          (map loop (E:case-lambda-body* E))))
      ((E:letrec?       E) (E:letrec (E-note E) (E:letrec-binding-left* E)
                                     (map loop (E:letrec-binding-right* E))
                                     (loop (E:letrec-body E))))
      (else                (mistake 'E-map-quote "not an E" E)))))

(define (E-simplify-quote E)
  (E-map-quote
    E
    (lambda (E)
      (let ((note (E-note E)))
        (let loop ((v (E:quote-value E)))
          (cond ((pair?   v) (E:call note (E:quote #f cons) (list (loop (car v)) (loop (cdr v)))))
                ((vector? v) (E:call note (E:quote #f vector) (map loop (vector->list v))))
                (else        (E:quote note v))))))))

(define (E-replace-primitive E primitive=>addr)
  (E-map-quote E (lambda (E) (alist-ref/k primitive=>addr (E:quote-value E) (lambda () E)
                                          (lambda (addr) (E:ref (E-note E) addr))))))
