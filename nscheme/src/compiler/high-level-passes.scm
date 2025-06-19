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
    (E-case
      E (lambda (E) (mistake 'E-map-quote "not an E" E))
      E:quote?        (lambda (v)              (E-annotate (f v) (E-note E)))
      E:ref?          (lambda (_)              E)
      E:if?           (lambda (c t f)          (E:if (E-note E) (loop c) (loop t) (loop f)))
      E:call?         (lambda (rator rand*)    (E:call (E-note E) (loop rator) (map loop rand*)))
      E:apply/values? (lambda (rator vrand)    (E:apply/values (E-note E) (loop rator) (loop vrand)))
      E:case-lambda?  (lambda (param*~* body*) (E:case-lambda (E-note E) param*~* (map loop body*)))
      E:letrec?       (lambda (lhs* rhs* body) (E:letrec (E-note E) lhs* (map loop rhs*) (loop body))))))

(define (E-simplify-quote E)
  (E-map-quote
    E
    (lambda (v)
      (let loop ((v v))
        (cond ((pair?   v) (E:call #f (E:quote #f cons) (list (loop (car v)) (loop (cdr v)))))
              ((vector? v) (E:call #f (E:quote #f vector) (map loop (vector->list v))))
              (else        (E:quote #f v)))))))

(define (E-replace-primitive E primitive=>addr)
  (E-map-quote E (lambda (v) (alist-ref/k primitive=>addr v (lambda () (E:quote #f v))
                                          (lambda (addr) (E:ref #f addr))))))
