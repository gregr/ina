#lang racket/base
(provide
  ;interruptible-lambda
  )
(require (prefix-in rkt: racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control transfer and interrupts ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-syntax-rule (define-global-parameter name default)
;  (define name
;    (let ((value #f))
;      (case-lambda
;        (() value)
;        ((x) (set! value x))))))
;
;(define-global-parameter timer-interrupt-handler #f)
;(define timer-ticks.remaining #f)
;
;(define interrupt-pending?       #f)
;(define disable-interrupts-count 0)
;(define poll-ticks.max           1000)
;(define poll-ticks.remaining     poll-ticks.max)
;
;(define (tick-interrupts!)
;  (set! poll-ticks.remaining (- poll-ticks.remaining 1))
;  (when (eq? 0 poll-ticks.remaining) (poll-interrupts!)))
;
;(define (poll-interrupts!)
;  (define (poll-etc!) (void))  ; We may add more polling here later.
;  (set! poll-ticks.remaining poll-ticks.max)
;  (when (eq? 0 disable-interrupts-count)
;    (when interrupt-pending?
;      (set! interrupt-pending? #f)
;      (cond
;        ((eq? timer-ticks.remaining 0)
;         (set! timer-ticks.remaining #f)
;         (poll-etc!)
;         ((or (timer-interrupt-handler) panic)))
;        (timer-ticks.remaining
;         (let ((poll-ticks.next (min poll-ticks.max timer-ticks.remaining)))
;           (set! timer-ticks.remaining (- timer-ticks.remaining poll-ticks.next))
;           (set! poll-ticks.remaining poll-ticks.next)
;           (set! interrupt-pending? #t))
;         (poll-etc!))
;        (else (poll-etc!))))))
;
;(define (enable-interrupts)
;  (if (eq? disable-interrupts-count 1)
;      (begin
;        (set! disable-interrupts-count 0)
;        (poll-interrupts!)
;        0)
;      (begin
;        (unless (eq? disable-interrupts-count 0)
;          (set! disable-interrupts-count (- disable-interrupts-count 1)))
;        disable-interrupts-count)))
;
;(define (disable-interrupts)
;  (when (and (eq? disable-interrupts-count 0) timer-ticks.remaining)
;    (set! timer-ticks.remaining (+ timer-ticks.remaining poll-ticks.remaining)))
;  (set! poll-ticks.remaining -1)
;  (set! disable-interrupts-count (+ disable-interrupts-count 1))
;  disable-interrupts-count)
;
;(define (set-timer ticks)
;  (let ((timer-ticks.prev
;         (if timer-ticks.remaining
;             (+ (if (eq? disable-interrupts-count 0) poll-ticks.remaining 0)
;                timer-ticks.remaining)
;             0)))
;    (if (< 0 ticks)
;        (begin
;          (set! timer-ticks.remaining ticks)
;          (set! interrupt-pending? #t))
;        (set! timer-ticks.remaining #f))
;    (poll-interrupts!)
;    timer-ticks.prev))
;
;(define-syntax-rule (interruptible-lambda param . body)
;  (lambda param (tick-interrupts!) . body))
;
;(define-global-parameter native-thread-local-value #f)
;
;(define prompt-tag.raw-escape (make-continuation-prompt-tag 'raw-escape))
;
;(define (with-raw-escape-prompt on-escape thunk)
;  (call-with-continuation-prompt thunk prompt-tag.raw-escape on-escape))
;
;(define (raw-escape-to-prompt . x*) (apply abort-current-continuation prompt-tag.raw-escape x*))
;
;(define (thread->coroutine t)
;  (lambda arg*
;    (thread-send t arg*)
;    (apply values (thread-receive))))
;(define (current-raw-coroutine) (thread->coroutine (current-thread)))
;(define (make-raw-coroutine proc)
;  (thread->coroutine (rkt:thread (lambda () (apply proc (thread-receive))))))

;(define (make-case-clause param body) (vector param body))
;(define (case-clause-param cc)        (vector-ref cc 0))
;(define (case-clause-body  cc)        (vector-ref cc 1))
;
;(define (make-code prov case-clauses cvar-count) (vector prov case-clauses cvar-count))
;(define (code-provenance              c)         (vector-ref c 0))
;(define (code-case-clauses            c)         (vector-ref c 1))
;(define (code-captured-variable-count c)         (vector-ref c 2))

;(define (procedure-metadata p)
;  (unless (procedure? p) (error "not a procedure" p))
;  (let ((pmd (hash-ref procedure=>metadata p (lambda () (error "procedure has no metadata" p)))))
;    (match-define (proc-metadata primitive captured code*) pmd)
;    ;; When a procedure is constructed in a letrec alongside captured values, those values may not
;    ;; have been initialized by the time the procedure is constructed.  By lazily placing captured
;    ;; values in a mvector, we can attach metadata to the procedure immediately after constructing
;    ;; it without having to worry about evaluation order.  We convert the mvector to a vector on
;    ;; demand.  (We assume metadata is requested only after captured values are initialized).
;    (vector primitive (and captured (mvector->vector captured)) code*)))
;
;(define procedure=>metadata (make-weak-hash))
;(struct proc-metadata (primitive captured code*) #:prefab)
;(define proc-metadata.empty (proc-metadata #f #f '()))
;(define (proc-metadata-primitive-set pmd primitive)
;  (match-define (proc-metadata _ captured code*) pmd)
;  (proc-metadata primitive captured code*))
;(define (proc-metadata-captured-set pmd captured)
;  (match-define (proc-metadata primitive _ code*) pmd)
;  (proc-metadata primitive captured code*))
;(define (proc-metadata-code-add pmd code)
;  (match-define (proc-metadata primitive captured code*) pmd)
;  (proc-metadata primitive captured (cons code code*)))
;(define (update-procedure-metadata! p update)
;  (hash-update! procedure=>metadata p update proc-metadata.empty))
;;; A primitive is an operation that is implemented directly in the platform
;;; layer.  These operations will typically be portable, but it is possible to
;;; define platform-specific operations.  To remain portable when snapshotting
;;; part of a system, code that makes use of a platform-specific operation
;;; should factor out any values that refer to the operation, so they can be
;;; marked as shared values.  When the snapshot is built, shared values will be
;;; omitted, and a host which loads the snapshot is responsible for supplying
;;; substitutes for these values.  This is analogous to dynamic linking with a
;;; shared library in mainstream operating systems.  A substitute would be
;;; implemented in a way that is both consistent with the original, and
;;; compatible with the new host, solving the portability issue.
;(define (procedure-primitive! p name)
;  (update-procedure-metadata! p (lambda (pmd) (proc-metadata-primitive-set pmd name))))
;(define (procedure-closure! p code cvals)
;  (update-procedure-metadata!
;    p (lambda (pmd) (proc-metadata-code-add (proc-metadata-captured-set pmd cvals)
;                                            (vector 'case-lambda
;                                                    (code-provenance code)
;                                                    (code-case-clauses code))))))

;;;;;;;;;;;;;;;;;;
;;; Primitives ;;;
;;;;;;;;;;;;;;;;;;

;(define-syntax-rule (declare-primitives! name ...)
;  (for-each procedure-primitive! (list name ...) '(name ...)))
;
;(declare-primitives!
;  ;; privileged primitives
;  current-panic-handler
;  native-thread-local-value with-raw-escape-prompt raw-escape-to-prompt
;  current-raw-coroutine make-raw-coroutine
;  timer-interrupt-handler set-timer enable-interrupts disable-interrupts
;
;  panic apply values
;  eqv? null? boolean? procedure? symbol? rational? integer? f32? f64?
;  pair? vector? mvector? bytes? mbytes?
;  cons car cdr
;  vector vector-length vector-ref
;  make-mvector mvector->vector mvector-length mvector-ref mvector-set!
;  bytes bytes-length bytes-ref bytes->symbol symbol->bytes
;  make-mbytes mbytes->bytes mbytes-length mbytes-ref mbytes-set!
;  bitwise-asl bitwise-asr bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length
;  integer-floor-divmod numerator denominator = <= >= < > + - * /
;
;  f32->u32 u32->f32 f64->u64 u64->f64
;  f32->f64 f64->f32 f32->rational rational->f32 f64->rational rational->f64
;  f32-cmp f32-floor f32-ceiling f32-truncate f32-round f32+ f32- f32* f32/
;  f64-cmp f64-floor f64-ceiling f64-truncate f64-round f64+ f64- f64* f64/)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snapshot saving and loading ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: port this to nscheme
;; TODO: update this ast representation
;(define (ast-lift-complex-values ast value->ast)
;  (let loop ((ast ast))
;    (match ast
;      (`#(quote ,value)            (if (ormap (lambda (?) (? value))
;                                              (list symbol? bytes? pair? vector?
;                                                    record? mvector? mbytes? procedure?))
;                                     (value->ast value)
;                                     ast))
;      (`#(ref ,name)               ast)
;      (`#(if ,c ,t ,f)             (ast:if (loop c) (loop t) (loop f)))
;      (`#(begin ,effect* ,result)  (ast:begin (map loop effect*) (loop result)))
;      (`#(call ,sinfo ,proc ,arg*) (ast:call sinfo (loop proc) (map loop arg*)))
;      (`#(case-lambda ,sinfo ,cc*) (ast:case-lambda
;                                     sinfo (map (lambda (cc) (make-case-clause
;                                                               (case-clause-param cc)
;                                                               (loop (case-clause-body cc))))
;                                                cc*)))
;      (`#(letrec ,bpair* ,body)    (ast:letrec
;                                     (map (lambda (bp) (ast:binding-pair
;                                                         (ast:binding-pair-left bp)
;                                                         (loop (ast:binding-pair-right bp))))
;                                          bpair*)
;                                     (loop body))))))
;
;(struct snapshot (value=>name primitive* io* value* initialization* root))
;
;(define (snapshot-ast ss ast:primitive ast:io external-binding-pairs)
;  (ast:let external-binding-pairs
;           (ast:letrec (append (map (lambda (name pname)
;                                      (ast:binding-pair name (ast:primitive pname)))
;                                    (map car (snapshot-primitive* ss))
;                                    (map cdr (snapshot-primitive* ss)))
;                               (map (lambda (name pname&desc)
;                                      (ast:binding-pair name (ast:io (car pname&desc) (cdr pname&desc))))
;                                    (map car (snapshot-io* ss))
;                                    (map cdr (snapshot-io* ss)))
;                               (snapshot-value* ss))
;                       (ast:begin (snapshot-initialization* ss) (snapshot-root ss)))))
;
;(define (make-snapshot library? value.root id->name external-value=>name)
;  (let ((value=>name      (make-hasheq))
;        (primitive*       '())
;        (io*              '())
;        (procedure*       '())
;        (other*           '())
;        (initialization** '()))
;    (define (args-name)      (id->name (- -1 (hash-count value=>name))))
;    (define (gen-name value) (let ((name (id->name (hash-count value=>name))))
;                               (hash-set! value=>name value name)
;                               name))
;    (define-syntax-rule (push! stack name value)
;      (set! stack (cons (cons name value) stack)))
;    (define (ast:ref/loop v) (ast:ref (loop v)))
;    (define (loop value)
;      (or (let ((name (or (hash-ref value=>name          value #f)
;                          (hash-ref external-value=>name value #f))))
;            (and name (ast:ref name)))
;          (match value
;            ((or '() #f #t (? fixnum?)) (ast:quote value))
;            (_ (let ((name (gen-name value)))
;                 (match value
;                   ((? procedure?)
;                    (match (procedure-metadata value)
;                      (`#(primitive ,pname) (push! primitive* name pname))
;                      (`#(io ,pname ,desc)  (push! io*        name (cons pname desc)))
;                      ;; TODO: redefine this to use new metadata shape:
;                      ;;   `#(case-lambda ,stx ,vector-of-case-clauses ,vector-of-captured-values)
;                      ;; The new case-clauses will correspond to a later-stage language AST that
;                      ;; references variables using lexical addresses rather than parsed names.
;                      ;; This means the code representation only needs a captured variable count,
;                      ;; not a list of captured names.
;                      (`#(closure ,code ,cvalues)
;                        ;; We want to avoid duplicating code shared by multiple closures.  To accomplish
;                        ;; this, we need to build a separate case-lambda corresponding to the potentially-
;                        ;; shared code, and have each closure reference it.  However, each closure may bind a
;                        ;; different set of values for the captured variables.  To handle these, the shared
;                        ;; code must be abstracted over the captured variables, and each closure must inject
;                        ;; its own captured values.
;                        (let* ((prov (code-provenance code))
;                               (name.code
;                                 (or (hash-ref value=>name          code #f)
;                                     (hash-ref external-value=>name code #f)
;                                     (let ((name (gen-name code)))
;                                       (push! procedure* name
;                                              (ast:lambda
;                                                prov (error "TODO: replace this section") ;(code-captured-variables code)
;                                                (ast:case-lambda
;                                                  prov (map (lambda (cc) (make-case-clause
;                                                                           (case-clause-param cc)
;                                                                           (ast-lift-complex-values
;                                                                             (case-clause-body cc) loop)))
;                                                            (code-case-clauses code)))))
;                                       name))))
;                          (push! procedure* name
;                                 (if (null? cvalues)
;                                   ;; When there are no cvalues, there is nothing to inject, so it is safe to
;                                   ;; immediately unwrap the shared code procedure.
;                                   (ast:call (ast:ref name.code))
;                                   ;; Because closures and captured values may reference each other
;                                   ;; recursively, if we inject captured values naively, we may mistakenly
;                                   ;; reference a snapshotted value before it has been evaluated.  To avoid
;                                   ;; this problem we eta expand the closure, i.e., turn it into a variadic
;                                   ;; lambda.  Each time the closure's procedure is applied, it will perform
;                                   ;; the injection just-in-time, and then forward its arguments to the
;                                   ;; resulting shared-code procedure.
;                                   (let ((name.args (args-name)))
;                                     (ast:lambda prov name.args
;                                                 (ast:call (loop apply)
;                                                           (apply ast:call (ast:ref name.code)
;                                                                  (map loop cvalues))
;                                                           (ast:ref name.args))))))))))
;                   ((? mvector?)
;                    (push! other* name (ast:call (loop make-mvector) (loop (mvector-length value)) (loop 0)))
;                    (set! initialization**
;                      (cons (map (lambda (i)
;                                   (ast:call (loop mvector-set!)
;                                             (ast:ref name) (loop i)
;                                             (loop (mvector-ref value i))))
;                                 (range (mvector-length value)))
;                            initialization**)))
;                   ;((? record?)
;                   ; ;; TODO: if the RTD transitively points to this record, this will fail to terminate.
;                   ; ;; This could be solved by thunking initializer creation.
;                   ; (push! other* name (ast:call (loop make-record) (loop (record-type-descriptor value)) (loop 0)))
;                   ; (set! initialization**
;                   ;   (cons (map (lambda (i)
;                   ;                (ast:call (loop record-set!)
;                   ;                          (ast:ref name) (loop i)
;                   ;                          (loop (record-ref value i))))
;                   ;              (range (vector-ref (record-type-descriptor value) 0)))
;                   ;         initialization**)))
;                   (_ (let ((ast (match value
;                                   ((cons v.a v.d) (ast:call (loop cons) (loop v.a) (loop v.d)))
;                                   ((? vector?)    (apply ast:call (loop vector)
;                                                          (map loop (vector->list value))))
;                                   ((? mbytes?)
;                                    (set! initialization**
;                                      (cons (map (lambda (i) (ast:call (loop mbytes-set!)
;                                                                       (ast:ref name) (loop i)
;                                                                       (loop (mbytes-ref value i))))
;                                                 (range (mbytes-length value)))
;                                            initialization**))
;                                    (ast:call (loop make-mbytes)
;                                              (loop (mbytes-length value)) (loop 0)))
;                                   ((or (? number?) (? symbol?) (? bytes?))
;                                    (ast:quote value)))))
;                        (push! other* name ast))))
;                 (ast:ref name))))))
;    (let ((ast.root (loop value.root)))
;      (snapshot value=>name
;                (reverse primitive*)
;                (reverse io*)
;                (append (reverse procedure*) (reverse other*))
;                (foldl append '() initialization**)
;                (if library?
;                  (let ((names (hash-values value=>name)))
;                    (if (null? names)
;                      (loop '())
;                      (let ((ast.cons (loop cons)))
;                        (foldr (lambda (name ast)
;                                 (ast:call ast.cons (ast:call ast.cons (ast:quote name) (ast:ref name)) ast))
;                               (loop '())
;                               names))))
;                  ast.root)))))
