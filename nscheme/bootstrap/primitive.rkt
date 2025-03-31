#lang racket/base
(provide
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
  current-platform
  with-panic-translation with-native-signal-handling)
(require
  ffi/unsafe/port ffi/unsafe/vm
  racket/list racket/path racket/port racket/set racket/tcp racket/udp racket/vector
  (prefix-in rkt: racket/base) (prefix-in rkt: racket/pretty))

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
  (rkt:displayln "unhandled panic:")
  (rkt:pretty-write (cons 'panic x*) (current-error-port))
  (let ((msg "panic"))
    ((error-display-handler) msg (make-exn:fail msg (current-continuation-marks))))
  (exit 1))
(define (exn-context x)
  (define (pretty-srcloc sl) (and sl (srcloc->string sl)))
  (map (lambda (frame)
         (cond
           ((pair?   frame) (list (car frame) (pretty-srcloc (cdr frame))))
           ((vector? frame) (list (vector-ref frame 0)
                                  (pretty-srcloc (vector-ref frame 1))
                                  (vector-ref frame 2)))
           (else frame)))
       (continuation-mark-set->context (exn-continuation-marks x))))
(define (with-panic-translation thunk)
  (call-with-exception-handler (lambda (x) (if (exn? x)
                                               (panic #f (exn-message x) (exn-context x))
                                               (panic #f x)))
                               thunk))
(define (thread thunk) (rkt:thread (lambda () (with-panic-translation thunk))))
(define (mistake* detail*) (panic 'mistake detail*))
(define (mistake . detail*) (mistake* detail*))

(struct non-utf8-string (bv) #:prefab)
(struct non-utf8-symbol (bv) #:prefab)
(define (string? x) (or (rkt:string? x) (non-utf8-string? x)))
(define (symbol? x) (or (rkt:symbol? x) (non-utf8-symbol? x)))
(define (string->symbol x)
  (cond
    ((rkt:string?      x) (rkt:string->symbol x))
    ((non-utf8-string? x) (non-utf8-symbol (non-utf8-string-bv x)))
    (else                 (mistake 'string->symbol "not a string" x))))
(define (symbol->string x)
  (cond
    ((rkt:symbol?      x) (rkt:symbol->string x))
    ((non-utf8-symbol? x) (non-utf8-string (non-utf8-symbol-bv x)))
    (else                 (mistake 'symbol->string "not a symbol" x))))
(define (string->bytevector x)
  (cond
    ((rkt:string?      x) (string->bytes/utf-8 x))
    ((non-utf8-string? x) (non-utf8-string-bv x))
    (else                 (mistake 'string->bytevector "not a string" x))))
(define (bytevector->string x)
  (unless (bytes? x) (mistake 'bytevector->string "not a bytevector" x))
  (with-handlers ((exn:fail:contract? (lambda (e) (non-utf8-string x)))) (bytes->string/utf-8 x)))

(define (eqv? a b)
  (or (rkt:eqv? a b)
      (cond
        ((bytevector?      a) (and (bytevector?      b) (bytes=? a b)))
        ((rkt:string?      a) (and (rkt:string?      b) (string=? a b)))
        ((non-utf8-string? a) (and (non-utf8-string? b) (bytes=? (non-utf8-string-bv a)
                                                                 (non-utf8-string-bv b))))
        ((non-utf8-symbol? a) (and (non-utf8-symbol? b) (bytes=? (non-utf8-symbol-bv a)
                                                                 (non-utf8-symbol-bv b))))
        (else                 #f))))

(define (rational? x) (and (rkt:rational? x) (exact? x)))
(define (integer?  x) (rkt:exact-integer? x))

(define (bitwise-asl n k)  (rkt:arithmetic-shift n    k))
(define (bitwise-asr n k)  (rkt:arithmetic-shift n (- k)))
(define (bitwise-length n) (integer-length n))

(define (integer-floor-divmod dividend divisor)
  (unless (integer? dividend) (mistake 'integer-floor-divmod "dividend is not an integer" dividend))
  (unless (integer? divisor) (mistake 'integer-floor-divmod "divisor is not an integer" divisor))
  (let ((q (rkt:floor (/ dividend divisor))))
    (values q (- dividend (* q divisor)))))

(define-values (prop:metadata metadata? metadata-ref) (make-struct-type-property 'metadata))
(define (make-record-type . x*) (apply (make-record-type/super-type #f) x*))
(define ((make-record-type/super-type super-type)
         name field-count mutable-field-position* final? proc-spec? metadata)
  (let-values (((stype construct ? access mutate!)
                (make-struct-type
                 name super-type field-count 0 #f
                 (append (if final? (list (cons prop:sealed #t)) '())
                         (list (cons prop:metadata metadata)))
                 #f proc-spec?
                 (set-subtract (range field-count) (or mutable-field-position* '())) #f #f)))
    (values (and (not final?) (make-record-type/super-type stype)) construct ? access
            (and mutable-field-position* (not (null? mutable-field-position*))
                 (lambda (r i v) (mutate! r i v) (values))))))

(struct mbytevector (bv) #:name mbytevector-struct #:constructor-name mbytevector:new #:prefab)
(struct mvector (v) #:name mvector-struct #:constructor-name mvector:new #:prefab)

(define (make-mvector    len x)  (mvector:new   (make-vector len x)))
(define (mvector-length  mv)     (vector-length (mvector-v mv)))
(define (mvector-ref     mv i)   (vector-ref    (mvector-v mv) i))
(define (mvector-set!    mv i x) (vector-set!   (mvector-v mv) i x) (values))
(define mvector->vector
  (case-lambda
    ((mv)             (vector-copy (mvector-v mv)))
    ((mv start count) (vector-copy (mvector-v mv) start (+ start count)))))

(define (bytevector        . x*) (apply bytes x*))
(define (bytevector?       x)    (bytes?       x))
(define (bytevector-length bv)   (bytes-length bv))
(define (bytevector-ref    bv i) (bytes-ref bv i))

(define (make-mbytevector        len n)   (mbytevector:new (make-bytes len n)))
(define (mbytevector-length      mbv)     (bytevector-length (mbytevector-bv mbv)))
(define (mbytevector-ref         mbv i)   (bytevector-ref    (mbytevector-bv mbv) i))
(define (mbytevector-set!        mbv i n) (bytes-set!        (mbytevector-bv mbv) i n) (values))
(define mbytevector->bytevector
  (case-lambda
    ((mbv)             (bytes-copy (mbytevector-bv mbv)))
    ((mbv start count) (subbytes   (mbytevector-bv mbv) start (+ start count)))))

;(define (f32?      x) (single-flonum? x))
;(define (f64?      x) (double-flonum? x))
;
;(define (b32? x) (and (integer? x) (<= 0 x #xffffffff)))
;(define (b64? x) (and (integer? x) (<= 0 x #xffffffffffffffff)))
;
;(define (f32->f64      n) (assert (f32? n)) (* 1.0 n))
;(define (f64->f32      n) (assert (f64? n)) (flsingle n))
;(define (f32->rational n) (assert (f32? n)) (inexact->exact n))
;(define (f64->rational n) (assert (f64? n)) (inexact->exact n))
;(define (rational->f32 n) (assert (rational? n)) (real->single-flonum n))
;(define (rational->f64 n) (assert (rational? n)) (real->double-flonum n))
;
;(define (f32->u32 n) (assert (f32? n)) (integer-bytes->integer (real->floating-point-bytes n 4) #f))
;(define (f64->u64 n) (assert (f64? n)) (integer-bytes->integer (real->floating-point-bytes n 8) #f))
;;;; NOTE: Any NaNs produced have already been automatically quieted by these operations.
;(define (u32->f32 n) (assert (b32? n)) (floating-point-bytes->real (integer->integer-bytes n 4 #f)))
;(define (u64->f64 n) (assert (b64? n)) (floating-point-bytes->real (integer->integer-bytes n 8 #f)))
;
;(define (f32-cmp a b)
;  (assert (f32? a) (f32? b))
;  (cond ((< a b) -1)
;        ((> a b)  1)
;        (else     0)))
;(define (f64-cmp a b)
;  (assert (f64? a) (f64? b))
;  (cond ((< a b) -1)
;        ((> a b)  1)
;        (else     0)))
;
;(define (f32-floor    n) (assert (f32? n)) (flfloor    n))
;(define (f32-ceiling  n) (assert (f32? n)) (flceiling  n))
;(define (f32-truncate n) (assert (f32? n)) (fltruncate n))
;(define (f32-round    n) (assert (f32? n)) (flround    n))
;(define (f64-floor    n) (assert (f64? n)) (flfloor    n))
;(define (f64-ceiling  n) (assert (f64? n)) (flceiling  n))
;(define (f64-truncate n) (assert (f64? n)) (fltruncate n))
;(define (f64-round    n) (assert (f64? n)) (flround    n))
;
;(define (f32+ a b) (assert (f32? a) (f32? b)) (fl+ a b))
;(define (f32- a b) (assert (f32? a) (f32? b)) (fl- a b))
;(define (f32* a b) (assert (f32? a) (f32? b)) (fl* a b))
;(define (f32/ a b) (assert (f32? a) (f32? b)) (fl/ a b))
;(define (f64+ a b) (assert (f64? a) (f64? b)) (fl+ a b))
;(define (f64- a b) (assert (f64? a) (f64? b)) (fl- a b))
;(define (f64* a b) (assert (f64? a) (f64? b)) (fl* a b))
;(define (f64/ a b) (assert (f64? a) (f64? b)) (fl/ a b))

;;;;;;;;;;;;
;;; Time ;;;
;;;;;;;;;;;;
(let ((vm (system-type 'vm)))
  (unless (eq? vm 'chez-scheme) (error "virtual machine is not chez-scheme" vm)))
(define (sleep-seconds-nanoseconds sec nsec) (rkt:sleep (+ sec (/ nsec 1000000000))))
(define seconds-nanoseconds/type
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
                    (else (mistake 'current-seconds-nanoseconds "not a time type" type)))))
        (lambda () (let ((time (chez:current-time type)))
                     (values (chez:time-second time) (chez:time-nanosecond time))))))))
(define platform.time (list (cons 'time (list (cons 'sleep-seconds-nanoseconds sleep-seconds-nanoseconds)
                                              (cons 'seconds-nanoseconds/type  seconds-nanoseconds/type)))))

;;;;;;;;;;;;;;;;;;;;;
;;; IO primitives ;;;
;;;;;;;;;;;;;;;;;;;;;
(define (with-io-guard kfail thunk)
  (define (exn-frame e) (vector (exn-message e) (exn-context e)))
  (with-handlers ((exn:fail:filesystem:exists? (lambda (e) (kfail 'exists (list (exn-frame e)))))
                  (exn:fail:filesystem:errno?  (lambda (e) (kfail (exn:fail:filesystem:errno-errno e)
                                                                  (list (exn-frame e)))))
                  (exn:fail:filesystem?        (lambda (e) (kfail #f (list (exn-frame e)))))
                  (exn:fail:network:errno?     (lambda (e) (kfail (exn:fail:network:errno-errno e)
                                                                  (list (exn-frame e)))))
                  (exn:fail:network?           (lambda (e) (kfail #f (list (exn-frame e)))))
                  ((lambda (e) (and (exn:fail? e) (not (exn:fail:contract? e))))
                   (lambda (e) (kfail #f (list (exn-frame e))))))
    (thunk)))
(define-syntax-rule (io-guard kfail body ...) (with-io-guard kfail (lambda () body ...)))
(define (nonnegative-integer?! x) (unless (exact-nonnegative-integer? x)
                                    (mistake "not a nonnegative integer" x)))
(define (buffer-range?! buf start count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! count)
  (let ((len (if (mbytevector? buf) (mbytevector-length buf) (bytevector-length buf))))
    (unless (<= (+ start count) len) (mistake "buffer range out of bounds" start count len))))

(define (rkt:imemory partial-description port)
  (file-stream-buffer-mode port 'none)
  (define description (list* (cons 'terminal?       (terminal-port? port))
                             (cons 'file-descriptor (unsafe-port->file-descriptor port))
                             partial-description))
  (define pos.cached #f)
  (lambda (method . arg*)
    (apply (case method
             ((read)     (lambda (pos dst start count kf keof k)
                           (buffer-range?! dst start count)
                           (io-guard
                            kf
                            (unless (eqv? pos.cached pos) (file-position port pos))
                            (let ((amount (read-bytes-avail! (mbytevector-bv dst) port start
                                                             (+ start count))))
                              (if (eof-object? amount)
                                  (keof)
                                  (begin
                                    (set! pos.cached (+ pos amount))
                                    (k amount)))))))
             ((size)     (lambda (kf k) (io-guard kf
                                                  (file-position port eof)
                                                  (let ((size (file-position port)))
                                                    (set! pos.cached size)
                                                    (k size)))))
             ((close)    (lambda (kf k) (io-guard kf (close-input-port port) (k))))
             ((describe) (lambda () description))
             (else       (error "not an imemory method" method)))
           arg*)))
(define (rkt:omemory partial-description port)
  (file-stream-buffer-mode port 'none)
  (define description (list* (cons 'terminal?       (terminal-port? port))
                             (cons 'file-descriptor (unsafe-port->file-descriptor port))
                             partial-description))
  (define pos.cached #f)
  (lambda (method . arg*)
    (apply (case method
             ((write)    (lambda (pos src start count kf k)
                           (buffer-range?! src start count)
                           (io-guard kf
                                     (unless (eqv? pos.cached pos) (file-position port pos))
                                     (write-bytes (if (mbytevector? src) (mbytevector-bv src) src)
                                                  port start (+ start count))
                                     (set! pos.cached (+ pos count))
                                     (k))))
             ((size)     (lambda (kf k) (io-guard kf
                                                  (file-position port eof)
                                                  (let ((size (file-position port)))
                                                    (set! pos.cached size)
                                                    (k size)))))
             ((resize!)  (lambda (new kf k) (io-guard kf
                                                      (set! pos.cached #f)
                                                      (file-truncate port new)
                                                      (k))))
             ((close)    (lambda (kf k) (io-guard kf (close-output-port port) (k))))
             ((describe) (lambda () description))
             (else       (error "not an omemory method" method)))
           arg*)))

(define (rkt:iport partial-description port)
  (file-stream-buffer-mode port 'none)
  (define description (list* (cons 'terminal?       (terminal-port? port))
                             (cons 'file-descriptor (unsafe-port->file-descriptor port))
                             partial-description))
  (define buf.unread #f) (define pos.unread 0)
  (lambda (method . arg*)
    (apply (case method
             ((read)
              (lambda (dst start count kf keof k)
                (buffer-range?! dst start count)
                (if (< 0 count)
                    (let ((dst (mbytevector-bv dst)))
                      (io-guard
                       kf
                       (if buf.unread
                           (k (let* ((len    (bytes-length buf.unread))
                                     (amount (min count (- len pos.unread)))
                                     (end    (+ pos.unread amount)))
                                (bytes-copy! dst start buf.unread pos.unread end)
                                (if (< end len) (set! pos.unread end) (set! buf.unread #f))
                                (if (< amount count)
                                    (let ((more (read-bytes-avail!* dst port (+ start amount)
                                                                    (+ start count))))
                                      (if (eof-object? more) amount (+ amount more)))
                                    amount)))
                           (let ((amount (read-bytes-avail! dst port start (+ start count))))
                             (if (eof-object? amount) (keof) (k amount))))))
                    (k 0))))
             ((unread)
              (lambda (src start count kf k)
                (buffer-range?! src start count)
                (if buf.unread
                    (let ((pos (- pos.unread count)))
                      (when (< pos 0) (error "too many bytes unread" count
                                             (cons 'position pos.unread) description))
                      (set! pos.unread pos))
                    (begin
                      (set! buf.unread (subbytes (mbytevector-bv src) start (+ start count)))
                      (set! pos.unread 0)))
                (k)))
             ((close)    (lambda (kf k)     (io-guard kf (close-input-port port) (k))))
             ((describe) (lambda ()         description))
             (else       (error "not an iport method" method description)))
           arg*)))
(define (rkt:oport partial-description port)
  (file-stream-buffer-mode port 'none)
  (define description (list* (cons 'terminal?       (terminal-port? port))
                             (cons 'file-descriptor (unsafe-port->file-descriptor port))
                             partial-description))
  (lambda (method . arg*)
    (apply (case method
             ((write)    (lambda (src start count kf k)
                           (buffer-range?! src start count)
                           (let ((src (if (mbytevector? src) (mbytevector-bv src) src))
                                 (end (+ start count)))
                             (io-guard kf (write-bytes src port start end) (k)))))
             ((close)    (lambda (kf k)     (io-guard kf (close-output-port port) (k))))
             ((describe) (lambda ()         description))
             (else       (error "not an oport method" method description)))
           arg*)))

;;;;;;;;;;;;;;;;;;
;; Standard IO ;;;
;;;;;;;;;;;;;;;;;;
(define standard-input-port  (rkt:iport '((type . iport:stdin))  (rkt:current-input-port)))
(define standard-output-port (rkt:oport '((type . oport:stdout)) (rkt:current-output-port)))
(define standard-error-port  (rkt:oport '((type . oport:stderr)) (rkt:current-error-port)))
(define platform.console (list (cons 'console (list (cons 'input-port  standard-input-port)
                                                    (cons 'output-port standard-output-port)
                                                    (cons 'error-port  standard-error-port)))))

;;;;;;;;;;;;;;;
;;; File IO ;;;
;;;;;;;;;;;;;;;
(define (make-path path) (bytes->path (cond
                                        ((string? path) (string->bytevector path))
                                        ((symbol? path) (string->bytevector (symbol->string path)))
                                        (else           path))))
(define ((open-input/make-device make-device device-type) path)
  (lambda (kf k)
    (let ((path (simple-form-path (make-path path))))
      (io-guard kf (k (make-device (list (cons 'type device-type) (cons 'path (path->bytes path)))
                                   (open-input-file path)))))))
(define ((open-output/make-device make-device device-type) path mod)
  (lambda (kf k)
    (let ((path        (simple-form-path (make-path path)))
          (exists-flag (case mod
                         ((create) 'error)
                         ((update) 'update)
                         ((#f)     'can-update)
                         (else     (mistake "not an output-file modifier" mod)))))
      (io-guard kf (k (make-device (list (cons 'type device-type) (cons 'path (path->bytes path)))
                                   (open-output-file path #:exists exists-flag)))))))
(define (posix-filesystem method . arg*)
  (apply
   (case method
     ((open-imemory) (open-input/make-device  rkt:imemory 'imemory:file))
     ((open-omemory) (open-output/make-device rkt:omemory 'omemory:file))
     ((open-iport)   (open-input/make-device  rkt:iport   'iport:file))
     ((open-oport)   (open-output/make-device rkt:oport   'oport:file))
     ((current-directory)  (lambda ()        (lambda (kf k)        (io-guard kf (k (path->bytes (rkt:current-directory)))))))
     ((change-evt)         (lambda (path)    (lambda (kf k)(io-guard kf (k (filesystem-change-evt (make-path path)))))))
     ((change-directory)   (lambda (path)    (lambda (kf k)(io-guard kf (rkt:current-directory (make-path path)) (k)))))
     ((list)               (lambda (path)    (lambda (kf k)(io-guard kf (k (map path->string (rkt:directory-list (make-path path))))))))
     ((make-symbolic-link) (lambda (to path) (lambda (kf k)(io-guard kf (make-file-or-directory-link to (make-path path)) (k)))))
     ((make-directory)     (lambda (path)    (lambda (kf k)(io-guard kf (rkt:make-directory (make-path path)) (k)))))
     ((move)               (lambda (old new) (lambda (kf k)(io-guard kf (rename-file-or-directory (make-path old) (make-path new) #f) (k)))))
     ((delete-file)        (lambda (path)    (lambda (kf k)(io-guard kf (rkt:delete-file (make-path path)) (k)))))
     ((delete-directory)   (lambda (path)    (lambda (kf k)(io-guard kf (rkt:delete-directory (make-path path)) (k)))))
     ((type)               (lambda (path)    (lambda (kf k)(io-guard kf (let ((type (file-or-directory-type (make-path path))))
                                                                          (k (case type
                                                                               ((file directory link #f) type)
                                                                               (else                     'unknown))))))))
     ((size)               (lambda (path)    (lambda (kf k)(io-guard kf (k (rkt:file-size (make-path path)))))))
     ((permissions)        (lambda (path)    (lambda (kf k)(io-guard kf (k (file-or-directory-permissions (make-path path) 'bits))))))
     ((modified-seconds)   (lambda (path)    (lambda (kf k)(io-guard kf (k (file-or-directory-modify-seconds (make-path path)))))))
     ((set-permissions!)
      (lambda (path permissions)
        (lambda (kf k)
          (nonnegative-integer?! permissions)
          (io-guard kf (file-or-directory-permissions (make-path path) permissions) (k)))))
     ((set-modified-seconds!)
      (lambda (path seconds)
        (lambda (kf k)
          (nonnegative-integer?! seconds)
          (io-guard kf (file-or-directory-modify-seconds (make-path path) seconds) (k)))))
     (else (mistake "not a posix-filesystem method" method)))
   arg*))

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
  (k (rkt:iport (list '(type . iport:tcp) (tcp-address-description in))  in)
     (rkt:oport (list '(type . oport:tcp) (tcp-address-description out)) out)))
(define (posix-network method . arg*)
  (apply
   (case method
     ((tcp-listen)
      (lambda (host port reuse? max-backlog)
        (lambda (kf k)
          (io-guard kf (let ((listener (tcp-listen port max-backlog reuse? host)))
                         (k (lambda (kf k) (io-guard kf (let-values (((in out) (tcp-accept listener)))
                                                          (make-socket-ports/k in out k))))
                            (lambda (kf k) (io-guard kf (tcp-close listener) (k)))))))))
     ((tcp-connect)
      (lambda (host port local-host local-port)
        (lambda (kf k)
          (io-guard kf (let-values (((in out) (tcp-connect host port local-host local-port)))
                         (make-socket-ports/k in out k))))))
     ((udp-open)
      (lambda (family-host family-port)
        (lambda (kf k)
          (io-guard
           kf (k (let ((socket (udp-open-socket family-host family-port)))
                   (define (receive-from/k dst start count kf k)
                     (buffer-range?! dst start count)
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
                           (buffer-range?! src start count)
                           (let ((src (if (mbytevector? src) (mbytevector-bv src) src)))
                             (io-guard kf (udp-send-to socket host port src start (+ start count)) (k)))))
                        ((send/k)
                         (lambda (          src start count kf k)
                           (buffer-range?! src start count)
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
                        (else                  (mistake "not a udp-socket method" method)))
                      arg*))))))))
     (else (mistake "not a posix-network method" method)))
   arg*))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; System processes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define posix-argument* (map string->bytevector (cons (path->string (find-system-path 'run-file))
                                                      (vector->list (current-command-line-arguments)))))
(define posix-environment (let ((env (current-environment-variables)))
                            (map (lambda (name) (cons name (environment-variables-ref env name)))
                                 (environment-variables-names env))))
(current-subprocess-custodian-mode 'kill)
(define (posix-raw-process/k in out err path arg* env kf k)
  (define (fd->rkt-port fd name mode)
    (and fd (unless (exact-nonnegative-integer? fd) (mistake "not a file descriptor" fd name))
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
                         (apply make-environment-variables
                                (let loop ((env env))
                                  (if (null? env)
                                      '()
                                      (cons (caar env) (cons (cdar env) (loop (cdr env)))))))))
           (apply subprocess out in err #f (make-path path) arg*)))))
     (let ((in  (and in  (rkt:oport '((type . oport:pipe)) in)))
           (out (and out (rkt:iport '((type . iport:pipe)) out)))
           (err (and err (rkt:iport '((type . iport:pipe)) err))))
       (k (lambda (method)
            (case method
              ((in)        in)
              ((out)       out)
              ((err)       err)
              ((pid)       (subprocess-pid sp))
              ((wait)      (subprocess-wait sp) (subprocess-status sp))
              ((kill)      (subprocess-kill sp #t) (values))
              ((interrupt) (subprocess-kill sp #f) (values))
              (else        (mistake "not a posix-raw-process/k method" method)))))))))

;;;;;;;;;;;;;;;;;;;;;
;;; Posix signals ;;;
;;;;;;;;;;;;;;;;;;;;;
(define posix-signal=>handler (make-hash))
(define (posix-signal-handler signal)
  (or (hash-ref posix-signal=>handler signal #f) (lambda (sig) (panic 'posix-signal sig))))
(define (posix-set-signal-handler! signal handler)
  (cond
    ((procedure? handler) (hash-set!    posix-signal=>handler signal handler))
    ((not        handler) (hash-remove! posix-signal=>handler signal))
    (else                 (mistake 'posix-set-signal-handler! "not a procedure or #f" signal handler)))
  (values))
(define (with-native-signal-handling thunk)
  (parameterize-break
   #f
   (let ((cc (rkt:current-custodian)) (cust (make-custodian)))
     (dynamic-wind
      (lambda () (void))
      (lambda ()
        (parameterize ((rkt:current-custodian cust))
          (let ((self (current-thread)))
            (rkt:thread (lambda () (thread-wait self) (custodian-shutdown-all cust))))
          (let* ((ch   (make-channel))
                 (body (rkt:thread
                        (lambda ()
                          (with-handlers (((lambda _ #t)
                                           (lambda (x) (channel-put ch (lambda () (raise x))))))
                            (call-with-values
                             (lambda () (parameterize ((rkt:current-custodian cc)) (thunk)))
                             (lambda x* (channel-put ch (lambda () (apply values x*))))))))))
            (let loop ()
              (with-handlers ((exn:break? (lambda (x)
                                            (let ((signal (cond
                                                            ((exn:break:hang-up?   x) 1)
                                                            ((exn:break:terminate? x) 15)
                                                            (else                     2))))
                                              ((posix-signal-handler signal) signal))
                                            (loop))))
                (sync/enable-break (handle-evt ch (lambda (^return) (^return)))
                                   (handle-evt (thread-dead-evt body) void)))))))
      (lambda () (custodian-shutdown-all cust))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive evaluation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-namespace-anchor anchor.primitive)
(define evaluate-racket-form
  (let ((ns (namespace-anchor->namespace anchor.primitive)))
    (lambda (stx) (rkt:eval stx ns))))
(define (evaluate-racket-text code)
  (let ((type 'racket-text))
    (unless (bytes? code) (mistake 'primitive-evaluate "code is not a bytevector" type code))
    (call-with-input-bytes
     code
     (lambda (in)
       (let ((stx (read in)))
         (when (eof-object? stx) (mistake 'primitive-evaluate "empty code" type code))
         (unless (eof-object? (read in))
           (mistake 'primitive-evaluate "code contains more than one expression" type code))
         (evaluate-racket-form stx))))))
(define primitive-evaluate
  (case-lambda
    (()               '(racket-form racket-text))
    ((type code kf k) (case type
                        ((racket-form) (k (evaluate-racket-form code)))
                        ((racket-text) (k (evaluate-racket-text code)))
                        (else          (kf '(racket-form racket-text)))))))

;;;;;;;;;;;;;;;;
;;; Platform ;;;
;;;;;;;;;;;;;;;;
(define current-platform
  (make-parameter
   (append platform.console
           platform.time
           (list (cons 'description (list (cons 'type 'racket)
                                          (cons 'os   (system-type 'os*))
                                          (cons 'arch (system-type 'arch))))
                 (cons 'posix       (list (cons 'argument*           posix-argument*)
                                          (cons 'environment         posix-environment)
                                          (cons 'raw-process/k       posix-raw-process/k)
                                          (cons 'filesystem          posix-filesystem)
                                          (cons 'network             posix-network)
                                          (cons 'set-signal-handler! posix-set-signal-handler!)
                                          (cons 'exit                exit)))
                 (cons 'primitive-evaluate primitive-evaluate)))))
