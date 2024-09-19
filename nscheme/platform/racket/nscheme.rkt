#lang racket/base
(provide
  apply/values case-values case case1 let-values assert mlet mdefine interruptible-lambda
  ;; privileged primitives
  native-signal-handler
  ;; procedure-metadata returns a vector with this shape:
  ;;   #(,primitive ,captured ,code*)
  ;; where:
  ;;   - primitive is either #f or a name
  ;;   - captured is either #f or a vector of captured values
  ;;   - code* is a possibly-empty list of code descriptions
  ;; This operator can allocate the vector on demand (which will hopefully be unboxed during
  ;; optimization), populating it based on a lower-level code/closure representation.
  procedure-metadata
  record? record record-type-descriptor record-ref
  string->bytevector bytevector->string
  native-thread-local-value with-raw-escape-prompt raw-escape-to-prompt
  current-raw-coroutine make-raw-coroutine
  timer-interrupt-handler set-timer enable-interrupts disable-interrupts

  make-parameter panic-handler current-custodian make-custodian custodian-shutdown-all
  current-thread-group make-thread-group current-thread thread thread/suspend-to-kill
  thread-resume thread-suspend thread-kill thread-wait
  thread-resume-evt thread-suspend-evt thread-dead-evt
  make-channel channel-get channel-put channel-get-evt channel-put-evt
  make-semaphore semaphore-post semaphore-wait semaphore-try-wait? semaphore-peek-evt
  call-with-semaphore sleep alarm-evt always-evt never-evt
  sync sync/timeout handle-evt choice-evt guard-evt nack-guard-evt replace-evt

  panic apply values
  eq? eqv? null? boolean? procedure? symbol? string? rational? integer? f32? f64?
  pair? vector? mvector? bytevector? mbytevector?
  string->symbol symbol->string
  cons car cdr
  vector vector-length vector-ref
  make-mvector mvector->vector mvector-slice mvector-length mvector-ref mvector-set!
  bytevector bytevector-length bytevector-ref
  make-mbytevector mbytevector->bytevector mbytevector-slice
  mbytevector-length mbytevector-ref mbytevector-set!
  bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
  bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length integer-floor-divmod
  numerator denominator = <= >= < > + - * /
  f32->u32 u32->f32 f64->u64 u64->f64
  f32->f64 f64->f32 f32->rational rational->f32 f64->rational rational->f64
  f32-cmp f32-floor f32-ceiling f32-truncate f32-round f32+ f32- f32* f32/
  f64-cmp f64-floor f64-ceiling f64-truncate f64-round f64+ f64- f64* f64/

  with-native-signal-handling

  stdio filesystem tcp udp tty console
  racket:eval)
(require
  racket/control racket/file racket/flonum racket/list racket/match racket/port racket/pretty
  racket/string racket/struct racket/system racket/tcp racket/udp racket/vector
  (prefix-in rkt: racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control transfer and interrupts ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eqv? a b)
  (or (rkt:eqv? a b)
      (if (string? a)
          (and (string? b) (string=? a b))
          (and (bytevector? a) (bytevector? b) (bytes=? a b)))))

(define (make-parameter default)
  (let ((rkt-param (rkt:make-parameter default)))
    (case-lambda
      (()                (rkt-param))
      ((new-value thunk) (parameterize ((rkt-param new-value)) (thunk))))))

(define (rkt-parameter->parameter rkt-param)
  (case-lambda
    (()                (rkt-param))
    ((new-value thunk) (parameterize ((rkt-param new-value)) (thunk)))))
(define current-custodian    (rkt-parameter->parameter rkt:current-custodian))
(define current-thread-group (rkt-parameter->parameter rkt:current-thread-group))

(define (thread-kill t) (kill-thread t))

(define (channel-get-evt ch) ch)

(define-syntax-rule (define-global-parameter name default)
  (define name
    (let ((value #f))
      (case-lambda
        (() value)
        ((x) (set! value x))))))

(define raw-panic-handler (make-parameter #f))
(define panic-handler
  (case-lambda
    (()          (raw-panic-handler))
    ((new thunk) (let ((old (raw-panic-handler)))
                   (raw-panic-handler
                    (lambda x* (raw-panic-handler old (lambda () (apply new x*) (apply panic x*))))
                    thunk)))))
(define (panic . x*)
  (let ((handle (panic-handler))) (when handle (apply handle x*)))
  (displayln "unhandled panic:")
  (pretty-write (cons 'panic x*))
  (for-each (lambda (x) (when (exn? x) ((error-display-handler) (exn-message x) x))) x*)
  (exit 1))
(uncaught-exception-handler (lambda (exn) (panic 'uncaught-exception exn)))

(define-global-parameter native-signal-handler #f)
(define (with-native-signal-handling thunk)
  (parameterize-break
   #f
   (let ((cust (make-custodian)))
     (dynamic-wind
      (lambda () (void))
      (lambda ()
        (parameterize ((rkt:current-custodian cust))
          (let ((self (current-thread)))
            (thread (lambda () (thread-wait self) (custodian-shutdown-all cust))))
          (let* ((ch   (make-channel))
                 (body (thread
                        (lambda ()
                          (with-handlers (((lambda _ #t)
                                           (lambda (x) (channel-put ch (lambda () (raise x))))))
                            (let-values ((x* (thunk)))
                              (channel-put ch (lambda () (apply values x*)))))))))
            (let loop ()
              (with-handlers ((exn:break? (lambda (x)
                                            ((or (native-signal-handler)
                                                 (lambda (kind) (panic 'native-signal kind)))
                                             (cond
                                               ((exn:break:hang-up?   x) 'hang-up)
                                               ((exn:break:terminate? x) 'terminate)
                                               (else                     'interrupt)))
                                            (loop))))
                (sync/enable-break (handle-evt ch (lambda (^return) (^return)))
                                   (handle-evt (thread-dead-evt body) void)))))))
      (lambda () (custodian-shutdown-all cust))))))

(define-global-parameter timer-interrupt-handler #f)
(define timer-ticks.remaining #f)

(define interrupt-pending?       #f)
(define disable-interrupts-count 0)
(define poll-ticks.max           1000)
(define poll-ticks.remaining     poll-ticks.max)

(define (tick-interrupts!)
  (set! poll-ticks.remaining (- poll-ticks.remaining 1))
  (when (eq? 0 poll-ticks.remaining) (poll-interrupts!)))

(define (poll-interrupts!)
  (define (poll-etc!) (void))  ; We may add more polling here later.
  (set! poll-ticks.remaining poll-ticks.max)
  (when (eq? 0 disable-interrupts-count)
    (when interrupt-pending?
      (set! interrupt-pending? #f)
      (cond
        ((eq? timer-ticks.remaining 0)
         (set! timer-ticks.remaining #f)
         (poll-etc!)
         ((or (timer-interrupt-handler) panic)))
        (timer-ticks.remaining
         (let ((poll-ticks.next (min poll-ticks.max timer-ticks.remaining)))
           (set! timer-ticks.remaining (- timer-ticks.remaining poll-ticks.next))
           (set! poll-ticks.remaining poll-ticks.next)
           (set! interrupt-pending? #t))
         (poll-etc!))
        (else (poll-etc!))))))

(define (enable-interrupts)
  (if (eq? disable-interrupts-count 1)
      (begin
        (set! disable-interrupts-count 0)
        (poll-interrupts!)
        0)
      (begin
        (unless (eq? disable-interrupts-count 0)
          (set! disable-interrupts-count (- disable-interrupts-count 1)))
        disable-interrupts-count)))

(define (disable-interrupts)
  (when (and (eq? disable-interrupts-count 0) timer-ticks.remaining)
    (set! timer-ticks.remaining (+ timer-ticks.remaining poll-ticks.remaining)))
  (set! poll-ticks.remaining -1)
  (set! disable-interrupts-count (+ disable-interrupts-count 1))
  disable-interrupts-count)

(define (set-timer ticks)
  (let ((timer-ticks.prev
         (if timer-ticks.remaining
             (+ (if (eq? disable-interrupts-count 0) poll-ticks.remaining 0)
                timer-ticks.remaining)
             0)))
    (if (< 0 ticks)
        (begin
          (set! timer-ticks.remaining ticks)
          (set! interrupt-pending? #t))
        (set! timer-ticks.remaining #f))
    (poll-interrupts!)
    timer-ticks.prev))

(define-syntax-rule (interruptible-lambda param . body)
  (lambda param (tick-interrupts!) . body))

(define-global-parameter native-thread-local-value #f)

(define prompt-tag.raw-escape (make-continuation-prompt-tag 'raw-escape))

(define (with-raw-escape-prompt on-escape thunk)
  (call-with-continuation-prompt thunk prompt-tag.raw-escape on-escape))

(define (raw-escape-to-prompt . x*) (apply abort-current-continuation prompt-tag.raw-escape x*))

(define (thread->coroutine t)
  (lambda arg*
    (thread-send t arg*)
    (apply values (thread-receive))))
(define (current-raw-coroutine) (thread->coroutine (current-thread)))
(define (make-raw-coroutine proc)
  (thread->coroutine (thread (lambda () (apply proc (thread-receive))))))

(define-syntax-rule (assert test ...) (begin (unless test (panic 'violation 'assert 'test)) ...))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Data primitives ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; - small constant: null boolean
;; - number:
;;   - exact rational
;;     - integer
;;       - fixnum
;;       - bignum
;;     - ratnum
;;   - inexact
;;     - flonum (various precisions)
;;   - machine [unsigned] integer (various precisions)
;; - symbol, string, bytevector, vector, pair
;; - record, mvector, mbytevector (mutable)
;; - procedure:
;;   - primitive
;;   - closure

(define (rational? x) (and (rkt:rational? x) (exact? x)))
(define (integer?  x) (rkt:exact-integer? x))
(define (f32?      x) (single-flonum? x))
(define (f64?      x) (double-flonum? x))

(define (b32? x) (and (integer? x) (<= 0 x #xffffffff)))
(define (b64? x) (and (integer? x) (<= 0 x #xffffffffffffffff)))

(define (f32->f64      n) (assert (f32? n)) (* 1.0 n))
(define (f64->f32      n) (assert (f64? n)) (flsingle n))
(define (f32->rational n) (assert (f32? n)) (inexact->exact n))
(define (f64->rational n) (assert (f64? n)) (inexact->exact n))
(define (rational->f32 n) (assert (rational? n)) (real->single-flonum n))
(define (rational->f64 n) (assert (rational? n)) (real->double-flonum n))

(define (f32->u32 n) (assert (f32? n)) (integer-bytes->integer (real->floating-point-bytes n 4) #f))
(define (f64->u64 n) (assert (f64? n)) (integer-bytes->integer (real->floating-point-bytes n 8) #f))
;;; NOTE: Any NaNs produced have already been automatically quieted by these operations.
(define (u32->f32 n) (assert (b32? n)) (floating-point-bytes->real (integer->integer-bytes n 4 #f)))
(define (u64->f64 n) (assert (b64? n)) (floating-point-bytes->real (integer->integer-bytes n 8 #f)))

(define (f32-cmp a b)
  (assert (f32? a) (f32? b))
  (cond ((< a b) -1)
        ((> a b)  1)
        (else     0)))
(define (f64-cmp a b)
  (assert (f64? a) (f64? b))
  (cond ((< a b) -1)
        ((> a b)  1)
        (else     0)))

(define (f32-floor    n) (assert (f32? n)) (flfloor    n))
(define (f32-ceiling  n) (assert (f32? n)) (flceiling  n))
(define (f32-truncate n) (assert (f32? n)) (fltruncate n))
(define (f32-round    n) (assert (f32? n)) (flround    n))
(define (f64-floor    n) (assert (f64? n)) (flfloor    n))
(define (f64-ceiling  n) (assert (f64? n)) (flceiling  n))
(define (f64-truncate n) (assert (f64? n)) (fltruncate n))
(define (f64-round    n) (assert (f64? n)) (flround    n))

(define (f32+ a b) (assert (f32? a) (f32? b)) (fl+ a b))
(define (f32- a b) (assert (f32? a) (f32? b)) (fl- a b))
(define (f32* a b) (assert (f32? a) (f32? b)) (fl* a b))
(define (f32/ a b) (assert (f32? a) (f32? b)) (fl/ a b))
(define (f64+ a b) (assert (f64? a) (f64? b)) (fl+ a b))
(define (f64- a b) (assert (f64? a) (f64? b)) (fl- a b))
(define (f64* a b) (assert (f64? a) (f64? b)) (fl* a b))
(define (f64/ a b) (assert (f64? a) (f64? b)) (fl/ a b))

(define (bitwise-arithmetic-shift-left  n k) (rkt:arithmetic-shift n    k))
(define (bitwise-arithmetic-shift-right n k) (rkt:arithmetic-shift n (- k)))
(define (bitwise-length                 n)   (integer-length n))

(define (integer-floor-divmod dividend divisor)
  (assert (integer? dividend) (integer? divisor))
  (let ((q (rkt:floor (/ dividend divisor))))
    (values q (- dividend (* q divisor)))))

(struct mbytevector (bv) #:name mbytevector-struct #:constructor-name mbytevector:new #:prefab)
(struct mvector (v) #:name mvector-struct #:constructor-name mvector:new #:prefab)
(struct record (type-descriptor field*) #:name record-struct #:constructor-name record:new #:prefab)

(define (record rtd . x*)
  (unless (and (vector? rtd) (< 0 (vector-length rtd)))
    (error "record-type-descriptor is not a non-empty vector" rtd))
  (let ((field-count (vector-ref rtd 0)))
    (unless (fixnum? field-count) (error "not a field count" field-count))
    (unless (= (length x*) field-count) (error "incorrect record argument count" field-count x*))
    (record:new rtd (list->vector x*))))

(define (record-ref x i)
  (unless (record? x) (error "not a record" x))
  (vector-ref (record-field* x) i))

(define (make-mvector    len x)  (mvector:new   (make-vector len x)))
(define (mvector-length  mv)     (vector-length (mvector-v mv)))
(define (mvector-ref     mv i)   (vector-ref    (mvector-v mv) i))
(define (mvector-set!    mv i x) (vector-set!   (mvector-v mv) i x))
(define (mvector-slice mv start count)
  (mvector:new (vector-copy (mvector-v mv) start (+ start count))))
(define mvector->vector
  (case-lambda
    ((mv)             (vector-copy (mvector-v mv)))
    ((mv start count) (vector-copy (mvector-v mv) start (+ start count)))))

(define (bytevector->string bv) (bytes->string/utf-8 bv))
(define (string->bytevector bv) (string->bytes/utf-8 bv))

(define (bytevector        . x*) (apply bytes x*))
(define (bytevector?       x)    (bytes?       x))
(define (bytevector-length bv)   (bytes-length bv))
(define (bytevector-ref    bv i) (bytes-ref bv i))

(define (make-mbytevector        len n)   (mbytevector:new (make-bytes len n)))
(define (mbytevector-length      mbv)     (bytevector-length (mbytevector-bv mbv)))
(define (mbytevector-ref         mbv i)   (bytevector-ref    (mbytevector-bv mbv) i))
(define (mbytevector-set!        mbv i n) (bytes-set!        (mbytevector-bv mbv) i n))
(define (mbytevector-slice mbv start count)
  (mbytevector:new (subbytes (mbytevector-bv mbv) start (+ start count))))
(define mbytevector->bytevector
  (case-lambda
    ((mbv)             (bytes-copy (mbytevector-bv mbv)))
    ((mbv start count) (subbytes   (mbytevector-bv mbv) start (+ start count)))))

(define (make-case-clause param body) (vector param body))
(define (case-clause-param cc)        (vector-ref cc 0))
(define (case-clause-body  cc)        (vector-ref cc 1))

(define (make-code prov case-clauses cvar-count) (vector prov case-clauses cvar-count))
(define (code-provenance              c)         (vector-ref c 0))
(define (code-case-clauses            c)         (vector-ref c 1))
(define (code-captured-variable-count c)         (vector-ref c 2))

(define (procedure-metadata p)
  (unless (procedure? p) (error "not a procedure" p))
  (let ((pmd (hash-ref procedure=>metadata p (lambda () (error "procedure has no metadata" p)))))
    (match-define (proc-metadata primitive captured code*) pmd)
    ;; When a procedure is constructed in a letrec alongside captured values, those values may not
    ;; have been initialized by the time the procedure is constructed.  By lazily placing captured
    ;; values in a mvector, we can attach metadata to the procedure immediately after constructing
    ;; it without having to worry about evaluation order.  We convert the mvector to a vector on
    ;; demand.  (We assume metadata is requested only after captured values are initialized).
    (vector primitive (and captured (mvector->vector captured)) code*)))

(define procedure=>metadata (make-weak-hash))
(struct proc-metadata (primitive captured code*) #:prefab)
(define proc-metadata.empty (proc-metadata #f #f '()))
(define (proc-metadata-primitive-set pmd primitive)
  (match-define (proc-metadata _ captured code*) pmd)
  (proc-metadata primitive captured code*))
(define (proc-metadata-captured-set pmd captured)
  (match-define (proc-metadata primitive _ code*) pmd)
  (proc-metadata primitive captured code*))
(define (proc-metadata-code-add pmd code)
  (match-define (proc-metadata primitive captured code*) pmd)
  (proc-metadata primitive captured (cons code code*)))
(define (update-procedure-metadata! p update)
  (hash-update! procedure=>metadata p update proc-metadata.empty))
;; A primitive is an operation that is implemented directly in the platform
;; layer.  These operations will typically be portable, but it is possible to
;; define platform-specific operations.  To remain portable when snapshotting
;; part of a system, code that makes use of a platform-specific operation
;; should factor out any values that refer to the operation, so they can be
;; marked as shared values.  When the snapshot is built, shared values will be
;; omitted, and a host which loads the snapshot is responsible for supplying
;; substitutes for these values.  This is analogous to dynamic linking with a
;; shared library in mainstream operating systems.  A substitute would be
;; implemented in a way that is both consistent with the original, and
;; compatible with the new host, solving the portability issue.
(define (procedure-primitive! p name)
  (update-procedure-metadata! p (lambda (pmd) (proc-metadata-primitive-set pmd name))))
(define (procedure-closure! p code cvals)
  (update-procedure-metadata!
    p (lambda (pmd) (proc-metadata-code-add (proc-metadata-captured-set pmd cvals)
                                            (vector 'case-lambda
                                                    (code-provenance code)
                                                    (code-case-clauses code))))))

;;;;;;;;;;;;;;;;;;
;;; Primitives ;;;
;;;;;;;;;;;;;;;;;;

(define-syntax-rule (declare-primitives! name ...)
  (for-each procedure-primitive! (list name ...) '(name ...)))

(declare-primitives!
  ;; privileged primitives
  panic-handler native-signal-handler
  procedure-metadata
  record? record record-type-descriptor record-ref
  string->bytevector bytevector->string
  native-thread-local-value with-raw-escape-prompt raw-escape-to-prompt
  current-raw-coroutine make-raw-coroutine
  timer-interrupt-handler set-timer enable-interrupts disable-interrupts

  panic apply values
  eq? eqv? null? boolean? procedure? symbol? string? rational? integer? f32? f64?
  pair? vector? mvector? bytevector? mbytevector?
  string->symbol symbol->string
  cons car cdr
  vector vector-length vector-ref
  make-mvector mvector->vector mvector-slice mvector-length mvector-ref mvector-set!
  bytevector bytevector-length bytevector-ref
  make-mbytevector mbytevector->bytevector mbytevector-slice
  mbytevector-length mbytevector-ref mbytevector-set!
  bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
  bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length integer-floor-divmod
  numerator denominator = <= >= < > + - * /
  f32->u32 u32->f32 f64->u64 u64->f64
  f32->f64 f64->f32 f32->rational rational->f32 f64->rational rational->f64
  f32-cmp f32-floor f32-ceiling f32-truncate f32-round f32+ f32- f32* f32/
  f64-cmp f64-floor f64-ceiling f64-truncate f64-round f64+ f64- f64* f64/)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax extensions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (apply/values rator vrand) (call-with-values (lambda () vrand) rator))
(define-syntax-rule (case-values e.values case-clauses ...)
  (apply/values (case-lambda case-clauses ...) e.values))

(define-syntax case
  (syntax-rules (else =>)
    ((_ x)                              (values))
    ((_ x (else rhs ...))               (let () rhs ...))
    ((_ x (=> proc))                    (proc x))
    ((_ x ((d ...) rhs ...) clause ...) (if (memv x '(d ...))
                                            (let () rhs ...)
                                            (case x clause ...)))))

(define-syntax case1
  (syntax-rules (else =>)
    ((_ x)                        (values))
    ((_ x (else rhs ...))         (let () rhs ...))
    ((_ x (=> proc))              (proc x))
    ((_ x (d rhs ...) clause ...) (if (eqv? x 'd)
                                      (let () rhs ...)
                                      (case1 x clause ...)))))

;; WARNING: this is only complete enough to run our bootstrapping process
(define-syntax let-values
  (syntax-rules ()
    ((_ (((param ...) rhs) ...) body ...)
     (rkt:let-values (((param ...) rhs) ...) body ...))
    ((_ ((param rhs)) body ...)
     (apply/values (lambda param body ...) rhs))))

(define-syntax-rule (mlet . body) (let . body))
(define-syntax-rule (mdefine . body) (define . body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snapshot saving and loading ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: update this ast representation
(define (ast:quote v)                      `#(quote ,v))
(define (ast:ref name)                     `#(ref ,name))
(define (ast:if ast.c ast.t ast.f)         `#(if ,ast.c ,ast.t ,ast.f))
(define (ast:begin ast.effect* ast.result) `#(begin ,ast.effect* ,ast.result))
(define (ast:call ast.proc . ast.args)     `#(call #f ,ast.proc ,ast.args))
(define (ast:case-lambda sinfo clause*)    `#(case-lambda ,sinfo ,clause*))
(define (ast:letrec bpair* body)           `#(letrec ,bpair* ,body))
(define (ast:lambda sinfo param ast.body)  (ast:case-lambda sinfo (list (make-case-clause param ast.body))))
(define (ast:let bpair* body)              (apply ast:call (ast:lambda #f (map car bpair*) body) (map cdr bpair*)))
(define (ast:list ast*)                    (apply ast:call (ast:lambda #f 'xs (ast:ref 'xs)) ast*))

(define (ast:binding-pair left right)  (cons left right))
(define (ast:binding-pair-left  bpair) (car bpair))
(define (ast:binding-pair-right bpair) (cdr bpair))

(define (ast-lift-complex-values ast value->ast)
  (let loop ((ast ast))
    (match ast
      (`#(quote ,value)            (if (ormap (lambda (?) (? value))
                                              (list symbol? string? pair? vector?
                                                    record? mvector? mbytevector? procedure?))
                                     (value->ast value)
                                     ast))
      (`#(ref ,name)               ast)
      (`#(if ,c ,t ,f)             (ast:if (loop c) (loop t) (loop f)))
      (`#(begin ,effect* ,result)  (ast:begin (map loop effect*) (loop result)))
      (`#(call ,sinfo ,proc ,arg*) (ast:call sinfo (loop proc) (map loop arg*)))
      (`#(case-lambda ,sinfo ,cc*) (ast:case-lambda
                                     sinfo (map (lambda (cc) (make-case-clause
                                                               (case-clause-param cc)
                                                               (loop (case-clause-body cc))))
                                                cc*)))
      (`#(letrec ,bpair* ,body)    (ast:letrec
                                     (map (lambda (bp) (ast:binding-pair
                                                         (ast:binding-pair-left bp)
                                                         (loop (ast:binding-pair-right bp))))
                                          bpair*)
                                     (loop body))))))

(struct snapshot (value=>name primitive* io* value* initialization* root))

(define (snapshot-ast ss ast:primitive ast:io external-binding-pairs)
  (ast:let external-binding-pairs
           (ast:letrec (append (map (lambda (name pname)
                                      (ast:binding-pair name (ast:primitive pname)))
                                    (map car (snapshot-primitive* ss))
                                    (map cdr (snapshot-primitive* ss)))
                               (map (lambda (name pname&desc)
                                      (ast:binding-pair name (ast:io (car pname&desc) (cdr pname&desc))))
                                    (map car (snapshot-io* ss))
                                    (map cdr (snapshot-io* ss)))
                               (snapshot-value* ss))
                       (ast:begin (snapshot-initialization* ss) (snapshot-root ss)))))

(define (make-snapshot library? value.root id->name external-value=>name)
  (let ((value=>name      (make-hasheq))
        (primitive*       '())
        (io*              '())
        (procedure*       '())
        (other*           '())
        (initialization** '()))
    (define (args-name)      (id->name (- -1 (hash-count value=>name))))
    (define (gen-name value) (let ((name (id->name (hash-count value=>name))))
                               (hash-set! value=>name value name)
                               name))
    (define-syntax-rule (push! stack name value)
      (set! stack (cons (cons name value) stack)))
    (define (ast:ref/loop v) (ast:ref (loop v)))
    (define (loop value)
      (or (let ((name (or (hash-ref value=>name          value #f)
                          (hash-ref external-value=>name value #f))))
            (and name (ast:ref name)))
          (match value
            ((or '() #f #t (? fixnum?)) (ast:quote value))
            (_ (let ((name (gen-name value)))
                 (match value
                   ((? procedure?)
                    (match (procedure-metadata value)
                      (`#(primitive ,pname) (push! primitive* name pname))
                      (`#(io ,pname ,desc)  (push! io*        name (cons pname desc)))
                      ;; TODO: redefine this to use new metadata shape:
                      ;;   `#(case-lambda ,stx ,vector-of-case-clauses ,vector-of-captured-values)
                      ;; The new case-clauses will correspond to a later-stage language AST that
                      ;; references variables using lexical addresses rather than parsed names.
                      ;; This means the code representation only needs a captured variable count,
                      ;; not a list of captured names.
                      (`#(closure ,code ,cvalues)
                        ;; We want to avoid duplicating code shared by multiple closures.  To accomplish
                        ;; this, we need to build a separate case-lambda corresponding to the potentially-
                        ;; shared code, and have each closure reference it.  However, each closure may bind a
                        ;; different set of values for the captured variables.  To handle these, the shared
                        ;; code must be abstracted over the captured variables, and each closure must inject
                        ;; its own captured values.
                        (let* ((prov (code-provenance code))
                               (name.code
                                 (or (hash-ref value=>name          code #f)
                                     (hash-ref external-value=>name code #f)
                                     (let ((name (gen-name code)))
                                       (push! procedure* name
                                              (ast:lambda
                                                prov (error "TODO: replace this section") ;(code-captured-variables code)
                                                (ast:case-lambda
                                                  prov (map (lambda (cc) (make-case-clause
                                                                           (case-clause-param cc)
                                                                           (ast-lift-complex-values
                                                                             (case-clause-body cc) loop)))
                                                            (code-case-clauses code)))))
                                       name))))
                          (push! procedure* name
                                 (if (null? cvalues)
                                   ;; When there are no cvalues, there is nothing to inject, so it is safe to
                                   ;; immediately unwrap the shared code procedure.
                                   (ast:call (ast:ref name.code))
                                   ;; Because closures and captured values may reference each other
                                   ;; recursively, if we inject captured values naively, we may mistakenly
                                   ;; reference a snapshotted value before it has been evaluated.  To avoid
                                   ;; this problem we eta expand the closure, i.e., turn it into a variadic
                                   ;; lambda.  Each time the closure's procedure is applied, it will perform
                                   ;; the injection just-in-time, and then forward its arguments to the
                                   ;; resulting shared-code procedure.
                                   (let ((name.args (args-name)))
                                     (ast:lambda prov name.args
                                                 (ast:call (loop apply)
                                                           (apply ast:call (ast:ref name.code)
                                                                  (map loop cvalues))
                                                           (ast:ref name.args))))))))))
                   ((? mvector?)
                    (push! other* name (ast:call (loop make-mvector) (loop (mvector-length value)) (loop 0)))
                    (set! initialization**
                      (cons (map (lambda (i)
                                   (ast:call (loop mvector-set!)
                                             (ast:ref name) (loop i)
                                             (loop (mvector-ref value i))))
                                 (range (mvector-length value)))
                            initialization**)))
                   ;((? record?)
                   ; ;; TODO: if the RTD transitively points to this record, this will fail to terminate.
                   ; ;; This could be solved by thunking initializer creation.
                   ; (push! other* name (ast:call (loop make-record) (loop (record-type-descriptor value)) (loop 0)))
                   ; (set! initialization**
                   ;   (cons (map (lambda (i)
                   ;                (ast:call (loop record-set!)
                   ;                          (ast:ref name) (loop i)
                   ;                          (loop (record-ref value i))))
                   ;              (range (vector-ref (record-type-descriptor value) 0)))
                   ;         initialization**)))
                   (_ (let ((ast (match value
                                   ((cons v.a v.d) (ast:call (loop cons) (loop v.a) (loop v.d)))
                                   ((? vector?)    (apply ast:call (loop vector)
                                                          (map loop (vector->list value))))
                                   ((? mbytevector?)
                                    (set! initialization**
                                      (cons (map (lambda (i) (ast:call (loop mbytevector-set!)
                                                                       (ast:ref name) (loop i)
                                                                       (loop (mbytevector-ref value i))))
                                                 (range (mbytevector-length value)))
                                            initialization**))
                                    (ast:call (loop make-mbytevector)
                                              (loop (mbytevector-length value)) (loop 0)))
                                   ((or (? number?) (? symbol?) (? string?) (? bytevector?))
                                    (ast:quote value)))))
                        (push! other* name ast))))
                 (ast:ref name))))))
    (let ((ast.root (loop value.root)))
      (snapshot value=>name
                (reverse primitive*)
                (reverse io*)
                (append (reverse procedure*) (reverse other*))
                (foldl append '() initialization**)
                (if library?
                  (let ((names (hash-values value=>name)))
                    (if (null? names)
                      (loop '())
                      (let ((ast.cons (loop cons)))
                        (foldr (lambda (name ast)
                                 (ast:call ast.cons (ast:call ast.cons (ast:quote name) (ast:ref name)) ast))
                               (loop '())
                               names))))
                  ast.root)))))


;;; TODO: reorganize the remainder of this file.

(define (lambda/handle-fail handle inner)
  (define (fail x datum) (handle (vector 'fail datum (exn-message x))))
  (lambda args
    (with-handlers*
      ((exn:fail:filesystem:errno?  (lambda (x) (fail x `(filesystem ,(exn:fail:filesystem:errno-errno x)))))
       (exn:fail:filesystem:exists? (lambda (x) (fail x '(filesystem exists))))
       (exn:fail:filesystem?        (lambda (x) (fail x '(filesystem #f))))
       (exn:fail:network:errno?     (lambda (x) (fail x `(network ,(exn:fail:network:errno-errno x)))))
       (exn:fail:network?           (lambda (x) (fail x '(network #f))))
       (exn:fail?                   (lambda (x) (fail x '(#f #f)))))
      (apply inner args))))

(define (method-unknown name . args) (error "unknown method:" name args))
(define (method-except m names)
  (lambda (name . args)
    (apply (if (member name names) method-unknown m) name args)))
(define (method-only m names)
  (lambda (name . args)
    (apply (if (member name names) m method-unknown) name args)))

(define-syntax method-choose
  (syntax-rules (else)
    ((_ ((name ...) body ...) ... (else else-body ...))
     (lambda (method-name . args)
       (apply (case method-name
                ((name ...) body ...) ...
                (else       else-body ...))
              method-name args)))
    ((_ body ...) (method-choose body ... (else method-unknown)))))

(define-syntax method-lambda
  (syntax-rules (else)
    ((_ ((name . param) body ...) ... (else else-body ...))
     (method-choose ((name) (lambda (_ . param) body ...)) ... (else else-body ...)))
    ((_ body ...) (method-lambda body ... (else method-unknown)))))

(define (mvector-copy!/ref mv start src src-start src-end ref)
  (let loop ((i src-start))
    (cond ((<= src-end i) (- i src-start))
          (else (mvector-set! mv (+ start (- i src-start)) (ref src i))
                (loop (+ i 1))))))
(define (mvector-copy! mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end mvector-ref))
(define (mvector-copy!/bytes mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end bytes-ref))
(define (mvector-copy!/vector mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end vector-ref))
;; TODO: this operation needs to be removed
(define (mvector-copy!/string mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end
                     (lambda (s i) (char->integer (string-ref s i)))))

(define (call-with-input-string s k) (k (string:port:input s)))
;; TODO: consider wrapping out to limit capability given to k
(define (call-with-output-string  k) (let ((out (string:port:output)))
                                       (k out)
                                       (out 'string)))

;; TODO: synchronizable events for ports

(define (port-close    p)   (p 'close))
(define (port-flush    p)   (p 'flush))
(define (port-put      p b) (p 'put  b))
;; TODO: this should take a (byte)vector as input
(define (port-put*     p s) (p 'put* s))
;; TODO: define port-put-string separately for convenience

(define (port-forget p amount) (p 'forget amount))
(define (port-get    p)        (p 'get))
(define (port-peek   p skip)   (p 'peek skip))
;; TODO: ideally these would be a general implementation in terms of port-get/peek.
;; Revisit these when testing compiler optimizations.
;; TODO: also, define non-! versions that return a new (byte)vector for convenience
(define (port-get*!  p mv start len)        (p 'get*!  mv start len))
(define (port-peek*! p mv start skip until) (p 'peek*! mv start skip until))

(define (port-truncate         p)   (p 'truncate))
(define (port-position         p)   (p 'position-ref))
(define (port-position-set!    p i) (p 'position-set! i))
(define (port-buffer-mode      p)   (p 'buffer-mode-ref))
(define (port-buffer-mode-set! p m) (p 'buffer-mode-set! m))

;; TODO: model file descriptors and their operations directly?

;; TODO: for simplicity, have get* directly return a string rather than fill a buffer?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic bytestream IO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bytestream:port port)
  (method-lambda
    ((buffer-mode-ref)       (file-stream-buffer-mode port))
    ((buffer-mode-set! mode) (file-stream-buffer-mode port mode))))

;; TODO: IO operations should return (values) or #f instead of eof-object
(define (bytestream:port:input super port)
  (define (eof->false d) (if (eof-object? d) #f d))
  (method-lambda
    ((close)     (close-input-port port))
    ((get)       (eof->false (read-byte port)))
    ((peek skip) (eof->false (peek-byte port skip)))
    ((get*! mv start len)
     (define bs (eof->false (read-bytes len port)))
     (if bs (mvector-copy!/bytes mv start bs 0 (bytes-length bs)) 0))
    ((peek*! mv start skip until)
     (define bs (eof->false (peek-bytes (- until skip) skip port)))
     (if bs (mvector-copy!/bytes mv start bs 0 (bytes-length bs)) 0))
    ((forget amount) (define bs (eof->false (read-bytes amount port)))
                     (if bs (bytes-length bs) 0))
    (else        super)))

(define (bytestream:port:output super port)
  (method-lambda
    ((close)  (close-output-port port))
    ((put b)  (write-byte b port))
    ;; TODO: this should take a (byte)vector as input
    ((put* s) (write-string s port))
    ((flush)  (flush-output port))
    (else     super)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File IO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filesystem root)
  ;; NOTE: this is not a secure abstraction of a subtree of the file system.
  ;; There is no protection against using ".." to access parent directories.
  (define (path/root path)
    (append root (if (string? path)
                   ;; #:trim? works around weird string-split default behavior
                   (string-split path "/" #:trim? #f)
                   path)))
  (define (resolve path) (string-join (path/root path) "/"))
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((subsystem path) (filesystem (path/root path)))

      ((open-input        path                ) (file:port:input  (open-input-file  (resolve path))))
      ((open-output       path (exists 'error)) (file:port:output (open-output-file (resolve path) #:exists exists)))
      ((open-input-output path (exists 'error)) (define-values (in out)
                                                  (open-input-output-file (resolve path) #:exists exists))
                                                (list (file:port:input in) (file:port:output in)))

      ((directory              path) (map path->string         (directory-list (resolve path))))
      ((file-exists?           path) (file-exists?                             (resolve path)))
      ((directory-exists?      path) (directory-exists?                        (resolve path)))
      ((link-exists?           path) (link-exists?                             (resolve path)))
      ((make-directory         path) (make-directory                           (resolve path)))
      ((make-directory*        path) (make-directory*                          (resolve path)))
      ((make-parent-directory* path) (make-parent-directory*                   (resolve path)))
      ((make-link           to path) (make-file-or-directory-link (resolve to) (resolve path)))
      ((delete-file            path) (delete-file                              (resolve path)))
      ((delete-directory       path) (delete-directory                         (resolve path)))
      ((delete-directory/files path) (delete-directory/files                   (resolve path)))

      ((rename old new (exists-ok? #f)) (rename-file-or-directory (resolve old) (resolve new) exists-ok?))
      ((copy   old new (exists-ok? #f)) (copy-file                (resolve old) (resolve new) exists-ok?))
      ((copy-directory/files src dest (keep-modify-time? #f) (keep-links? #f))
       (copy-directory/files (resolve src) (resolve dest)
                             #:keep-modify-seconds? keep-modify-time?
                             #:preserve-links? keep-links?))

      ((size             path)         (file-size                        (resolve path)))
      ((permissions-ref  path)         (file-or-directory-permissions    (resolve path) 'bits))
      ((permissions-set! path mode)    (assert (integer? mode))
                                       (file-or-directory-permissions    (resolve path) mode))
      ((modify-time-ref  path)         (file-or-directory-modify-seconds (resolve path)))
      ((modify-time-set! path seconds) (file-or-directory-modify-seconds (resolve path) seconds))

      ((wait path) (define evt (filesystem-change-evt (resolve path)))
                   (sync evt)
                   (filesystem-change-evt-cancel evt)))))

(define (file:port port)
  (define super (bytestream:port port))
  (method-lambda
    ((position-ref)        (file-position* port))
    ((position-set! index) (file-position* port index))
    (else                  super)))

(define (file:port:input  port) (bytestream:port:input (file:port port) port))
(define (file:port:output port)
  (define super (file:port port))
  (bytestream:port:output
    (method-lambda
      ((truncate size) (file-truncate port size))
      (else            super))
    port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network communication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tcp
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((listen hostname port (max-wait 4) (reuse? #f))
       (tcp:listener (tcp-listen port max-wait reuse? hostname)))
      ((connect host port (localhost #f) (localport #f))
       (define-values (in out) (tcp-connect host port localhost localport))
       (tcp:port in out)))))

(define (tcp:listener listen)
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((close)  (tcp-close listen))
      ;; TODO: use a synchronizable event instead, based on tcp-accept-evt
      ((ready?) (tcp-accept-ready? listen))
      ((accept) (define-values (in out) (tcp-accept listen))
                (tcp:port in out)))))

(define (tcp:port in out)
  (define (abandoner port)
    (define super (bytestream:port port))
    (lambda/handle-fail
      (lambda (x) x)
      (method-lambda
        ((abandon) (tcp-abandon-port port))
        (else super))))
  (define a.in  (abandoner in))
  (define a.out (abandoner out))
  (define super
    (lambda/handle-fail
      (lambda (x) x)
      (method-lambda
        ((addresses)  (define-values
                        (host.local port.local host.remote port.remote)
                        (tcp-addresses in #t))
                      (list host.local port.local host.remote port.remote))
        ((in  . args) (if (null? args) (bytestream:port:input  a.in  in)
                        (apply a.in  args)))
        ((out . args) (if (null? args) (bytestream:port:output a.out out)
                        (apply a.out args))))))
  (bytestream:port:output (bytestream:port:input super in) out))

;; TODO: synchronizable events for udp sending, receiving, and readiness
(define udp
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((open host.family port.family (port.local #f) (reuse? #f))
       (define p (udp:port (udp-open-socket host.family port.family)))
       (when port.local (p 'bind #f port.local reuse?))
       p)
      ((listen host.local port.local (reuse? #f))
       (define p (udp:port (udp-open-socket host.local port.local)))
       (p 'bind host.local port.local reuse?)
       p)
      ((connect host port (port.local #f) (reuse? #f))
       (define p (udp:port (udp-open-socket host port)))
       (when port.local (p 'bind #f port.local reuse?))
       (p 'connect host port)
       p))))

(define (udp:port socket)
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((close)                      (udp-close    socket))
      ((bind host port (reuse? #f)) (udp-bind!    socket host port reuse?))
      ((connect host port)          (udp-connect! socket host port))
      ((disconnect)                 (udp-connect! socket #f   #f))
      ((addresses)                  (define-values
                                      (host.local port.local host.remote port.remote)
                                      (udp-addresses socket #t))
                                    (list host.local port.local host.remote port.remote))

      ;; TODO: this should take a (byte)vector as input
      ((put* s)        (udp-send socket (string->bytes/utf-8 s)))
      ((aim host port) (lambda/handle-fail
                         (lambda (x) x)
                         (method-lambda
                           ;; TODO: synchronizable events
                           ;; TODO: this should take a (byte)vector as input
                           ((put* s) (udp-send-to socket host port (string->bytes/utf-8 s))))))

      ;; TODO: omit remote host and port when connected?
      ((get*! mv start len)
       (define        bs                 (make-bytes len 0))
       (define-values (amount host port) (udp-receive! socket bs 0 len))
       (mvector-copy!/bytes mv start bs 0 amount)
       (cons amount (cons host port)))

      ((buffer-size-set! amount) (udp-set-receive-buffer-size! socket amount))
      ((ttl-ref)                 (udp-ttl                      socket))
      ((ttl-set! ttl)            (udp-set-ttl!                 socket ttl))

      ((multicast-join  addr host.local)    (udp-multicast-join-group!    socket addr host.local))
      ((multicast-leave addr host.local)    (udp-multicast-leave-group!   socket addr host.local))
      ((multicast-iface-ref)                (udp-multicast-interface      socket))
      ((multicast-iface-set! host.local)    (udp-multicast-set-interface! socket      host.local))
      ((multicast-loopback?-ref)            (udp-multicast-loopback?      socket))
      ((multicast-loopback?-set! loopback?) (udp-multicast-set-loopback!  socket loopback?))
      ((multicast-ttl-ref)                  (udp-multicast-ttl            socket))
      ((multicast-ttl-set! ttl)             (udp-multicast-set-ttl!       socket ttl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard IO and consoles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (console in out err)
  (lambda/handle-fail (lambda (x) x)
                      (method-lambda ((in) in) ((out) out) ((error) err))))

(define (stdio:port:input  port) (bytestream:port:input  (bytestream:port port) port))
(define (stdio:port:output port) (bytestream:port:output (bytestream:port port) port))

(define stdio (console (stdio:port:input  (current-input-port))
                       (stdio:port:output (current-output-port))
                       (stdio:port:output (current-error-port))))

(define null:port:output
  (method-lambda
    ((put  _) #t)
    ((put* _) #t)
    ((close)  #t)
    ((flush)  #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String IO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: these should be built on (byte)vector:port:input/output

;; TODO: we don't want these operations.  Use bytevectors instead.
(define (string->vector s)
  (list->vector (bytes->list (string->bytes/utf-8 s))))
(define (vector->string v)
  (bytes->string/utf-8 (list->bytes (vector->list v))))

(define (string:port:input s)
  (define v (string->vector s))
  (define i 0)
  (define (ref i) (and (< i (vector-length v)) (vector-ref v i)))
  (define (ref* mv start i skip until)
    (mvector-copy!/vector mv start v (+ i skip)
                          (min (vector-length v) (+ i until))))
  (method-lambda
    ((get)                        (define b (ref i))
                                  (when b (set! i (+ i 1)))
                                  b)
    ((peek skip)                  (ref (+ i skip)))
    ((get*! mv start len)         (define amount (ref* mv start i 0 len))
                                  (set! i (+ i amount))
                                  amount)
    ((peek*! mv start skip until) (ref* mv start i skip until))
    ((forget amount) (define next-i (min (vector-length v) (+ i amount)))
                     (define actual (- next-i i))
                     (set! i next-i) actual)
    ((position-ref)        i)
    ((position-set! index) (set! i (min (max index 0) (vector-length v))))))

(define (string:port:output)
  (define buffer (make-mvector 32 0))
  (define i 0)
  (define (grow mult)
    (define current buffer)
    (set! buffer (make-mvector (* mult (mvector-length buffer)) 0))
    (mvector-copy! buffer 0 current 0 (mvector-length current)))
  (method-lambda
    ((string) (define out (make-mvector i 0))
              (mvector-copy! out 0 buffer 0 i)
              (vector->string (mvector->vector out)))
    ((put b)  (when (= i (mvector-length buffer)) (grow 2))
              (mvector-set! buffer i b)
              (set! i (+ i 1)))
    ;; TODO: this should take a (byte)vector as input
    ;; TODO: do not use these string operations
    ((put* s) (define u (- (string-length s) (- (mvector-length buffer) i)))
              (when (< 0 u) (grow (+ (quotient u (mvector-length buffer)) 2)))
              (mvector-copy!/string buffer i s 0 (string-length s))
              (set! i (+ i (string-length s))))
    ((close)               #t)
    ((flush)               #t)
    ((position-ref)        i)
    ((position-set! index) (set! i (min (max index 0) i)))
    ((truncate)            (set! i 0) (set! buffer (make-mvector 32 0)))))

;; TODO: synchronous channels, generators
;; NOTE: these are not ports; ports are restricted to transferring bytes
;; TODO: channels that get from sequences or put to mvectors
;; TODO: generators that iterate over sequences

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TTY manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: see text/terminal-control.scm for escape codes

(define tty
  (let ()
    (define (command name . args)
      (string-trim
        (with-output-to-string
          (lambda () (apply system* (find-executable-path name) args)))))
    (define (tput arg)    (command "tput" arg))
    (define (stty . args) (apply command "stty" args))
    (method-lambda
      ;; NOTE: ec:display-size can report these, but reading the report may be inconvenient
      ((lines)      (string->number (string-trim (tput "lines"))))
      ((columns)    (string->number (string-trim (tput "cols"))))
      ;; NOTE: these don't seem necessary due to existing escape codes
      ;((clear)       (command "clear")) ; \e[2J
      ;((save)        (tput "smcup"))    ; \e[?47h
      ;((restore)     (tput "rmcup"))    ; \e[?47l
      ;((cursor-show) (tput "cnorm"))    ; \e[?25h
      ;((cursor-hide) (tput "civis"))    ; \e[?25l
      ((stty-ref)   (stty "-g"))
      ((stty-set s) (stty s))
      ((stty-raw)   (stty "raw")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (racket:eval rkt-datum)
  (define (racket-datum form)
    (define (@ i) (vector-ref form i))
    (cond ((pair? form)         (cons (racket-datum (car form))
                                      (racket-datum (cdr form))))
          ((not (vector? form)) form)
          ((eq? (@ 0) 'quote)   (@ 1))
          ((eq? (@ 0) 'vector)  (vector-map racket-datum (@ 1)))
          ((eq? (@ 0) 'keyword) (string->keyword (@ 1)))
          (else                 (error "invalid racket-datum:" form))))
  (parameterize ((current-namespace (make-base-namespace)))
    (eval (racket-datum rkt-datum))))
