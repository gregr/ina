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

  host-pid host-argument* host-environment raw-host-process/k open-pipe/k
  change-directory filesystem-change-evt filesystem-change-evt-cancel
  directory-file*/k make-symbolic-link/k make-directory/k
  delete-directory/k delete-file/k move-file/k open-input-file/k open-output-file/k
  file-type/k file-size/k file-permissions/k file-modified-seconds/k
  set-file-permissions!/k set-file-modified-seconds!/k
  gethostname open-tcp-listener/k open-tcp-connection/k open-udp-socket/k

  standard-input-port standard-output-port standard-error-port

  make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
  current-thread-group make-thread-group current-thread thread thread/suspend-to-kill
  thread-resume thread-wait thread-resume-evt thread-suspend-evt thread-dead-evt
  make-channel channel-get channel-put channel-get-evt channel-put-evt
  make-semaphore semaphore-post semaphore-wait semaphore-try-wait? semaphore-peek-evt
  sync/default handle-evt choice-evt guard-evt nack-guard-evt replace-evt
  current-time/type sleep-seconds-nanoseconds

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

  ;f32->u32 u32->f32 f64->u64 u64->f64
  ;f32->f64 f64->f32 f32->rational rational->f32 f64->rational rational->f64
  ;f32-cmp f32-floor f32-ceiling f32-truncate f32-round f32+ f32- f32* f32/
  ;f64-cmp f64-floor f64-ceiling f64-truncate f64-round f64+ f64- f64* f64/

  with-native-signal-handling)
(require
  ffi/unsafe/port ffi/unsafe/vm
  racket/flonum racket/match racket/path racket/pretty racket/os racket/tcp racket/udp racket/vector
  (prefix-in rkt: racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control transfer and interrupts ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eqv? a b)
  (or (rkt:eqv? a b)
      (if (string? a)
          (and (string? b) (string=? a b))
          (and (bytevector? a) (bytevector? b) (bytes=? a b)))))

(define (sync/default handle-default . evt*) (apply sync/timeout handle-default evt*))

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

(define (channel-get-evt ch) ch)

(define-syntax-rule (define-global-parameter name default)
  (define name
    (let ((value #f))
      (case-lambda
        (() value)
        ((x) (set! value x))))))

(define raw-current-panic-handler (make-parameter #f))
(define current-panic-handler
  (case-lambda
    (()          (raw-current-panic-handler))
    ((new thunk) (let ((old (raw-current-panic-handler)))
                   (raw-current-panic-handler
                    (lambda x*
                      (raw-current-panic-handler old (lambda () (apply new x*) (apply panic x*))))
                    thunk)))))
(define (panic . x*)
  (let ((handle (current-panic-handler))) (when handle (apply handle x*)))
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
  current-panic-handler native-signal-handler
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
;; TODO: port this to nscheme
;; TODO: update this ast representation
;(define (ast-lift-complex-values ast value->ast)
;  (let loop ((ast ast))
;    (match ast
;      (`#(quote ,value)            (if (ormap (lambda (?) (? value))
;                                              (list symbol? string? pair? vector?
;                                                    record? mvector? mbytevector? procedure?))
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
;                                   ((? mbytevector?)
;                                    (set! initialization**
;                                      (cons (map (lambda (i) (ast:call (loop mbytevector-set!)
;                                                                       (ast:ref name) (loop i)
;                                                                       (loop (mbytevector-ref value i))))
;                                                 (range (mbytevector-length value)))
;                                            initialization**))
;                                    (ast:call (loop make-mbytevector)
;                                              (loop (mbytevector-length value)) (loop 0)))
;                                   ((or (? number?) (? symbol?) (? string?) (? bytevector?))
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

;;;;;;;;;;;;
;;; Time ;;;
;;;;;;;;;;;;
(let ((vm (system-type 'vm)))
  (unless (eq? vm 'chez-scheme) (error "virtual machine is not chez-scheme" vm)))
(define current-time/type
  (let ((chez:current-time    (vm-primitive 'current-time))
        (chez:time-second     (vm-primitive 'time-second))
        (chez:time-nanosecond (vm-primitive 'time-nanosecond)))
    (lambda (type)
      (let ((type (case type
                    ((utc)                    'time-utc)
                    ((monotonic)              'time-monotonic)
                    ((process)                'time-process)
                    ((thread)                 'time-thread)
                    ((garbage-collector-cpu)  'time-collector-cpu)
                    ((garbage-collector-real) 'time-collector-real)
                    (else (panic #f "not a current-time type" type)))))
        (lambda () (let ((time (chez:current-time type)))
                     (values (chez:time-second time) (chez:time-nanosecond time))))))))
(define (sleep-seconds-nanoseconds sec nsec) (rkt:sleep (+ sec (/ nsec 1000000000))))

;;;;;;;;;;;;;;;;;;;;;
;;; IO primitives ;;;
;;;;;;;;;;;;;;;;;;;;;
(define (rkt-port-set-position!/k port new kf k)
  (with-handlers ((exn:fail:filesystem? (lambda (e) (kf 'no-position (list (exn-message e))))))
    (file-position port (or new eof))
    (k)))
(define (with-io-guard kfail thunk)
  (with-handlers ((exn:fail:filesystem:exists? (lambda (e) (kfail 'exists (list (exn-message e)))))
                  (exn:fail:filesystem:errno?  (lambda (e) (kfail (exn:fail:filesystem:errno-errno e)
                                                                  (list (exn-message e)))))
                  (exn:fail:filesystem?        (lambda (e) (kfail #f (list (exn-message e)))))
                  (exn:fail:network:errno?     (lambda (e) (kfail (exn:fail:network:errno-errno e)
                                                                  (list (exn-message e)))))
                  (exn:fail:network?           (lambda (e) (kfail #f (list (exn-message e)))))
                  ((lambda (e) (and (exn:fail? e) (not (exn:fail:contract? e))))
                   (lambda (e) (kfail #f (list (exn-message e))))))
    (thunk)))
(define-syntax-rule (io-guard kfail body ...) (with-io-guard kfail (lambda () body ...)))

(define (nonnegative-integer?! x) (unless (exact-nonnegative-integer? x)
                                    (panic #f "not a nonnegative integer" x)))
(define (buffer-range?! buf start min-count desired-count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! min-count)
  (nonnegative-integer?! desired-count)
  (let ((len (if (mbytevector? buf) (mbytevector-length buf) (bytevector-length buf))))
    (unless (<= (+ start min-count) (+ start desired-count) len)
      (panic #f "buffer range out of bounds" start min-count desired-count len))))

(define (rkt:iport partial-description port)
  (file-stream-buffer-mode port 'none)
  (define description (list* (cons 'terminal?       (terminal-port? port))
                             (cons 'file-descriptor (unsafe-port->file-descriptor port))
                             partial-description))
  (lambda (method . arg*)
    (apply
     (case method
       ((read)          (lambda (dst start min-count count kf keof k)
                          (buffer-range?! dst start min-count count)
                          (io-guard
                           kf
                           (cond
                             ((= count 0) 0)
                             ((= min-count count)
                              (let ((amount (read-bytes! (mbytevector-bv dst) port start
                                                         (+ start count))))
                                (if (eof-object? amount) (keof) (k amount))))
                             ((= min-count 0)
                              (let ((amount (read-bytes-avail!* (mbytevector-bv dst) port start
                                                                (+ start count))))
                                (if (eof-object? amount) (keof) (k amount))))
                             (else (let loop ((total 0))
                                     (let ((amount (read-bytes-avail! (mbytevector-bv dst) port
                                                                      (+ start total)
                                                                      (+ start count))))
                                       (if (eof-object? amount)
                                           (if (= total 0) (keof) (k total))
                                           (let ((total (+ total amount)))
                                             (if (< total min-count)
                                                 (loop total)
                                                 (k total)))))))))))
       ((pread)         (lambda (pos dst start count kf keof k)
                          (buffer-range?! dst start count count)
                          (io-guard
                           kf
                           (let ((pos.current (file-position* port)))
                             (if pos.current
                                 (rkt-port-set-position!/k
                                  port pos kf
                                  (lambda ()
                                    (let ((amount (read-bytes! (mbytevector-bv dst) port start
                                                               (+ start count))))
                                      (rkt-port-set-position!/k
                                       port pos.current kf
                                       (if (eof-object? amount) keof (lambda () (k amount)))))))
                                 (kf 'no-position "iport does not support pread"))))))
       ((read-byte)     (lambda (kf keof k) (io-guard kf (let ((b (read-byte port)))
                                                           (if (eof-object? b) (keof) (k b))))))
       ((set-position!) (lambda (new kf k) (rkt-port-set-position!/k port new kf k)))
       ((position)      (lambda ()         (file-position* port)))
       ((close)         (lambda (kf k)     (io-guard kf (close-input-port port) (k))))
       ((description)   (lambda ()         description))
       (else            (error "not an iport method" method)))
     arg*)))

(define (rkt:oport partial-description port)
  (file-stream-buffer-mode port 'none)
  (define description (list* (cons 'terminal?       (terminal-port? port))
                             (cons 'file-descriptor (unsafe-port->file-descriptor port))
                             partial-description))
  (lambda (method . arg*)
    (apply
     (case method
       ((write)         (lambda (src start min-count count kf k)
                          (buffer-range?! src start min-count count)
                          (let ((src (if (mbytevector? src) (mbytevector-bv src) src)))
                            (io-guard
                             kf
                             (cond
                               ((= min-count count)
                                (k (write-bytes src port start (+ start count))))
                               ((= min-count 0)
                                (k (or (write-bytes-avail* src port start (+ start count)) 0)))
                               (else (let loop ((total 0))
                                       (let* ((amount (write-bytes-avail src port (+ start total)
                                                                         (+ start count)))
                                              (total  (+ total amount)))
                                         (if (< total min-count) (loop total) (k total))))))))))
       ((pwrite)        (lambda (pos src start count kf k)
                          (buffer-range?! src start count count)
                          (let ((src (if (mbytevector? src) (mbytevector-bv src) src)))
                            (io-guard
                             kf
                             (let ((pos.current (file-position* port)))
                               (if pos.current
                                   (rkt-port-set-position!/k
                                    port pos kf
                                    (lambda () (write-bytes src port start (+ start count)) (k)))
                                   (kf 'no-position "oport does not support pwrite")))))))
       ((write-byte)    (lambda (b kf k)   (io-guard kf (write-byte b port) (k))))
       ((set-size!)     (lambda (new kf k) (io-guard kf (file-truncate port new) (k))))
       ((set-position!) (lambda (new kf k) (rkt-port-set-position!/k port new kf k)))
       ((position)      (lambda ()         (file-position* port)))
       ((close)         (lambda (kf k)     (io-guard kf (close-output-port port) (k))))
       ((description)   (lambda ()         description))
       (else            (error "not an oport method" method)))
     arg*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard IO ports ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define standard-input-port  (rkt:iport '((type . stdin))  (current-input-port)))
(define standard-output-port (rkt:oport '((type . stdout)) (current-output-port)))
(define standard-error-port  (rkt:oport '((type . stderr)) (current-error-port)))

;;;;;;;;;;;;;;;
;;; File IO ;;;
;;;;;;;;;;;;;;;
(define (change-directory        path)      (rkt:current-directory path) (values))
(define (directory-file*/k       path kf k) (io-guard kf (k (map path->string (rkt:directory-list path)))))
(define (make-symbolic-link/k to path kf k) (io-guard kf (make-file-or-directory-link to path) (k)))
(define (make-directory/k        path kf k) (io-guard kf (rkt:make-directory path) (k)))
(define (move-file/k          old new kf k) (io-guard kf (rename-file-or-directory old new #f) (k)))
(define (delete-file/k           path kf k) (io-guard kf (rkt:delete-file path) (k)))
(define (delete-directory/k      path kf k) (io-guard kf (rkt:delete-directory path) (k)))
(define (file-type/k             path kf k) (io-guard kf (let ((type (file-or-directory-type path)))
                                                           (k (case type
                                                                ((file directory link #f) type)
                                                                (else                     'unknown))))))
(define (file-size/k             path kf k) (io-guard kf (k (rkt:file-size path))))
(define (file-permissions/k      path kf k) (io-guard kf (k (file-or-directory-permissions path 'bits))))
(define (file-modified-seconds/k path kf k) (io-guard kf (k (file-or-directory-modify-seconds path))))
(define (set-file-permissions!/k path permissions kf k)
  (nonnegative-integer?! permissions)
  (io-guard kf (file-or-directory-permissions path permissions) (k)))
(define (set-file-modified-seconds!/k path seconds kf k)
  (nonnegative-integer?! seconds)
  (io-guard kf (file-or-directory-modify-seconds path seconds) (k)))
(define (open-input-file/k path kf k)
  (let ((path        (normalize-path path)))
    (io-guard kf (k (rkt:iport (list '(type . file-iport) (cons 'path path))
                               (open-input-file path))))))
(define (open-output-file/k path restriction kf k)
  (let ((path        (normalize-path path))
        (exists-flag (case restriction
                       ((create) 'error)
                       ((update) 'udpate)
                       ((#f)     'can-update)
                       (else     (panic #f "not an open-output-file restriction" restriction)))))
    (io-guard kf (k (rkt:oport (list '(type . file-oport) (cons 'path path))
                               (open-output-file path #:exists exists-flag))))))

;;;;;;;;;;;;;;;;;;
;;; Network IO ;;;
;;;;;;;;;;;;;;;;;;

(define (make-socket-ports/k in out k)
  (define (tcp-address-description p)
    (let-values (((local-host local-port remote-host remote-port) (tcp-addresses p #t)))
      (cons 'address*
            (list (cons 'local  (list (cons 'host local-host)
                                      (cons 'port local-port)))
                  (cons 'remote (list (cons 'host remote-host)
                                      (cons 'port remote-port)))))))
  (k (rkt:iport (list '(type . tcp-iport) (tcp-address-description in))  in)
     (rkt:oport (list '(type . tcp-oport) (tcp-address-description out)) out)))

(define (open-tcp-listener/k hostname port reuse? max-backlog kf k)
  (io-guard
   kf (let ((listener (tcp-listen port max-backlog reuse? hostname)))
        (k (lambda (method . arg*)
             (apply
              (case method
                ((accept/k) (lambda (kf k)
                              (io-guard kf (let-values (((in out) (tcp-accept listener)))
                                             (make-socket-ports/k in out k)))))
                ((close/k)  (lambda (kf k) (io-guard kf (tcp-close listener) (k))))
                (else       (panic #f "not a listener method" method)))
              arg*))))))

(define (open-tcp-connection/k host port local-host local-port kf k)
  (io-guard kf (let-values (((in out) (tcp-connect host port local-host local-port)))
                 (make-socket-ports/k in out k))))

(define (open-udp-socket/k family-host family-port kf k)
  (io-guard
   kf (k (let ((socket (udp-open-socket family-host family-port)))
           (define (receive-from/k dst start count kf k)
             (buffer-range?! dst start count count)
             (io-guard kf (let-values (((amount host port)
                                        (udp-receive! socket dst start (+ start count))))
                            (k amount host port))))
           (lambda (method . arg*)
             (apply
              (case method
                ((receive-from/k) receive-from/k)
                ((receive/k)
                 (lambda (dst start count kf k)
                   (receive-from/k dst start count kf (lambda (amount host port) (k amount)))))
                ((send-to/k)
                 (lambda (host port src start count kf k)
                   (buffer-range?! src start count count)
                   (let ((src (if (mbytevector? src) (mbytevector-bv src) src)))
                     (io-guard kf (udp-send-to socket host port src start (+ start count)) (k)))))
                ((send/k)
                 (lambda (          src start count kf k)
                   (buffer-range?! src start count count)
                   (let ((src (if (mbytevector? src) (mbytevector-bv src) src)))
                     (io-guard kf (udp-send    socket           src start (+ start count)) (k)))))
                ((address*/k)
                 (lambda (kf k)
                   (io-guard kf (let-values (((local-host local-port remote-host remote-port)
                                              (udp-addresses socket #t)))
                                  (k (list (cons 'local  (list (cons 'host local-host)
                                                               (cons 'port local-port)))
                                           (cons 'remote (list (cons 'host remote-host)
                                                               (cons 'port remote-port)))))))))
                ((connect/k) (lambda (host port kf k)
                               (io-guard kf (udp-connect! socket host port) (k))))
                ((bind/k)    (lambda (host port reuse? kf k)
                               (io-guard kf (udp-bind! socket host port reuse?) (k))))
                ((close/k)   (lambda (kf k) (io-guard kf (udp-close socket) (k))))
                ((set-receive-buffer-size!/k)
                 (lambda (amount kf k)
                   (io-guard kf (udp-set-receive-buffer-size! socket amount) (k))))
                ((ttl)        (lambda () (udp-ttl socket)))
                ((set-ttl!/k) (lambda (ttl kf k) (io-guard kf (udp-set-ttl! socket ttl) (k))))
                ((multicast-join!/k)
                 (lambda (addr local-host kf k)
                   (io-guard kf (udp-multicast-join-group!  socket addr local-host) (k))))
                ((multicast-leave!/k)
                 (lambda (addr local-host kf k)
                   (io-guard kf (udp-multicast-leave-group! socket addr local-host) (k))))
                ((set-multicast-ttl!/k)
                 (lambda (ttl kf k) (io-guard kf (udp-multicast-set-ttl! socket ttl) (k))))
                ((set-multicast-loopback!/k)
                 (lambda (loop? kf k) (io-guard kf (udp-multicast-set-loopback! socket loop?) (k))))
                ((set-multicast-interface!/k)
                 (lambda (host kf k) (io-guard kf (udp-multicast-set-interface! socket host) (k))))
                ((multicast-ttl)       (lambda () (udp-multicast-ttl socket)))
                ((multicast-loopback?) (lambda () (udp-multicast-loopback? socket)))
                ((multicast-interface) (lambda () (udp-multicast-interface socket)))
                (else                  (panic #f "not a udp-socket method" method)))
              arg*))))))

;;;;;;;;;;;;;;;
;;; Pipe IO ;;;
;;;;;;;;;;;;;;;
(define (open-pipe/k kf k)
  (io-guard
   kf
   ;; This produces a blocking pipe, but at the cost of running another process, and at the cost of
   ;; that process copying data from its stdin to its stdout.  Fortunately, at least the process will
   ;; be cleaned up once its stdin is closed.
   (let-values (((sp out in err) (subprocess #f #f 'stdout #f (find-executable-path "cat"))))
     (let ((in  (and in  (rkt:oport '((type . pipe-oport)) in)))
           (out (and out (rkt:iport '((type . pipe-iport)) out))))
       (k in out))))
  ;; Unfortunately, this produces a non-blocking pipe because Racket opens files with O_NONBLOCK.
  ;; This hack also comes with some extra risk because it creates and then quickly removes a
  ;; temporary directory and fifo.
  ;(io-guard
  ; kf
  ; (case-values
  ;   (let* ((out.msg   (open-output-string))
  ;          (dir       (make-temporary-directory))
  ;          (pipe-path (path->string (build-path dir "pipe"))))
  ;     (dynamic-wind
  ;      void
  ;      (lambda ()
  ;        (let ((exit-code (parameterize ((current-output-port out.msg)
  ;                                        (current-error-port  out.msg))
  ;                           (system*/exit-code (find-executable-path "mkfifo") pipe-path))))
  ;          (if (= exit-code 0)
  ;              (let ((out (rkt:iport '((type . pipe-iport)) (open-input-file pipe-path)))
  ;                    (in  (rkt:oport '((type . pipe-oport))
  ;                                      (open-output-file pipe-path #:exists 'update))))
  ;                (values in out))
  ;              (values #f exit-code (get-output-string out.msg)))))
  ;      (lambda ()
  ;        (delete-file pipe-path)
  ;        (delete-directory dir))))
  ;   ((in out)          (k in out))
  ;   ((_ exit-code msg) (kf exit-code msg))))
  ;; It would be nicer if Racket exposed the ability to create host pipes directly.  It already does
  ;; this internally to implement subprocesses.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Host system processes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define host-pid (getpid))
(define host-argument* (apply vector (path->string (find-system-path 'run-file))
                              (vector->list (current-command-line-arguments))))
(define host-environment
  (let ((host-env (current-environment-variables)))
    (map (lambda (name) (cons name (environment-variables-ref host-env name)))
         (environment-variables-names host-env))))
(current-subprocess-custodian-mode #f)
(define (raw-host-process/k in out err path arg* env kf k)
  (define (fd->rkt-port fd name mode)
    (and fd (unless (exact-nonnegative-integer? fd) (panic #f "not a file descriptor" fd name))
         (unsafe-file-descriptor->port fd name mode)))
  (io-guard
   kf
   (let-values
     (((sp out in err)
       (let ((in  (fd->rkt-port in  'in  '(read)))
             (out (fd->rkt-port out 'out '(write)))
             (err (if (or (and out err (eqv? out err)) (eq? err 'stdout))
                      'stdout
                      (fd->rkt-port err 'err '(write)))))
         (parameterize ((current-environment-variables
                         (if env
                             (apply make-environment-variables
                                    (let loop ((env env))
                                      (if (null? env)
                                          '()
                                          (cons (caar env) (cons (cdar env) (loop (cdr env)))))))
                             (current-environment-variables))))
           (apply subprocess out in err #f path arg*)))))
     (let ((in  (and in  (rkt:oport '((type . pipe-oport)) in)))
           (out (and out (rkt:iport '((type . pipe-iport)) out)))
           (err (and err (rkt:iport '((type . pipe-iport)) err))))
       (k (lambda (method)
            (case method
              ((in)        in)
              ((out)       out)
              ((err)       err)
              ((pid)       (subprocess-pid sp))
              ((wait)      (subprocess-wait sp) (subprocess-status sp))
              ((kill)      (subprocess-kill sp #t) (values))
              ((interrupt) (subprocess-kill sp #f) (values))
              (else        (panic #f "not a raw-host-process/k method" method)))))))))
