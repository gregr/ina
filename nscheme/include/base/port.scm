(define (buffer-range?! buf start min-count desired-count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! min-count)
  (nonnegative-integer?! desired-count)
  (let ((len (if (mbytevector? buf) (mbytevector-length buf) (bytevector-length buf))))
    (unless (<= (+ start min-count) (+ start desired-count) len)
      (error "buffer range out of bounds" start min-count desired-count len))))

(define (iomemory-describe iom) (iom 'describe))
(define (port? x) (procedure? x))
(define (port-describe p) (p 'describe))

;;;;;;;;;;;;;;;;;
;;; IO Errors ;;;
;;;;;;;;;;;;;;;;;
;;; Improper use of an iomemory, port, or port-buffer operation will panic.  Proper use may still
;;; fail, indicated by two error description values:
;;; - failure tag, typically a symbol, #f, or an integer code
;;;   - #f        ; failure is not categorized
;;;   - <integer> ; e.g., an errno value
;;;   - exists
;;;   - not-open
;;;   - no-space
;;; - failed operation details
;;; Depending on the operation variant, these two values are either:
;;; - returned to a failure continuation for /k operations
;;; - raised as an io-error otherwise
(define-values (io-error:kind io-error? io-error-tag io-error-context)
  (make-exception-kind-etc error:kind 'io-error '#(tag context)))
(define (make-io-error  desc tag context) (make-exception io-error:kind (vector desc tag context)))
(define (raise-io-error tag context) (raise (make-io-error "IO error" tag context)))

;;;;;;;;;;;;;;;;;;;;
;;; Input memory ;;;
;;;;;;;;;;;;;;;;;;;;
(define (imemory-close/k im kf k) (im 'close kf k))
(define (imemory-close   im)      (imemory-close/k im raise-io-error values))
(define (imemory-size/k  im kf k) (im 'size kf k))
(define (imemory-size    im)      (imemory-size/k im raise-io-error values))
;; Returns EOF, the amount read, or a failure indication.
;; Blocks until at least (min count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (imemory-read/k im pos dst start count kf keof k) (im 'read pos dst start count kf keof k))
(define (imemory-read im pos dst start count)
  (imemory-read/k im pos dst start count raise-io-error values values))
;; Returns EOF, the byte read, or a failure indication.
(define (imemory-read-byte/k im pos kf keof k)
  (let ((dst (make-mbytevector 1 0)))
    (imemory-read/k im pos dst 0 1 1 kf keof (lambda (amount) (k (mbytevector-ref dst 0))))))
(define (imemory-read-byte im pos) (imemory-read-byte/k im pos raise-io-error values values))

;;;;;;;;;;;;;;;;;;;;;
;;; Output memory ;;;
;;;;;;;;;;;;;;;;;;;;;
(define (omemory-close/k   om kf k)      (om 'close kf k))
(define (omemory-close     om)           (omemory-close/k om raise-io-error values))
(define (omemory-size/k    om kf k)      (om 'size kf k))
(define (omemory-size      om)           (omemory-size/k om raise-io-error values))
(define (omemory-resize!/k om size kf k) (om 'resize! size kf k))
(define (omemory-resize!   om size)      (omemory-resize!/k om size raise-io-error values))
;; May return a failure indication.
;; Blocks until count bytes are written.
;; Failure may occur after a partial write.
(define (omemory-write/k om pos src start count kf k) (om 'write pos src start count kf k))
(define (omemory-write om pos src start count)
  (omemory-write/k om pos src start count raise-io-error values))
(define (omemory-write-byte/k om pos byte kf k)
  (omemory-write/k om pos (bytevector byte) 0 1 1 kf k))
(define (omemory-write-byte om pos byte) (omemory-write-byte/k om pos byte raise-io-error values))

;;;;;;;;;;;;;;;;;;;
;;; Input ports ;;;
;;;;;;;;;;;;;;;;;;;
(define (iport-close/k p kf k) (p 'close kf k))
(define (iport-close   p)      (iport-close/k p raise-io-error values))
;; Returns EOF, the amount read, or a failure indication.
;; Blocks until at least (min min-count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (iport-read/k p dst start min-count count kf keof k)
  (p 'read dst start min-count count kf keof k))
(define (iport-read p dst start min-count count)
  (iport-read/k p dst start min-count count raise-io-error values values))
;; Returns EOF, the byte read, or a failure indication.
(define (iport-read-byte/k p kf keof k)
  (let ((dst (make-mbytevector 1 0)))
    (iport-read/k p dst 0 1 1 kf keof (lambda (amount) (k (mbytevector-ref dst 0))))))
(define (iport-read-byte p) (iport-read-byte/k p raise-io-error values values))
;; Reverts the most recent read of count bytes, provided by the mbytevector src.
;; It is an error to unread different bytes from those that were originally read, but the particular
;; port implementation decides whether to enforce this.
(define (iport-unread/k p src start count kf k) (p 'unread src start count kf k))
(define (iport-unread p src start count) (iport-unread/k p src start count raise-io-error values))

;;;;;;;;;;;;;;;;;;;;
;;; Output ports ;;;
;;;;;;;;;;;;;;;;;;;;
(define (oport-close/k p kf k) (p 'close kf k))
(define (oport-close   p)      (oport-close/k p raise-io-error values))
;; Returns the amount written, or a failure indication.
;; Blocks until at least min-count bytes are written.
;; Failure may occur after a partial write.
(define (oport-write/k p src start min-count count kf k)
  (p 'write src start min-count count kf k))
(define (oport-write   p src start min-count count)
  (oport-write/k p src start min-count count raise-io-error values))
(define (oport-write-byte/k p byte kf k) (oport-write/k p (bytevector byte) 0 1 1 kf k))
(define (oport-write-byte   p byte)      (oport-write-byte/k p byte raise-io-error values))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector memory ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These bytevector memory definitions are not safe for concurrent access unless synchronization is
;;; provided by the user.
(define (imemory:mbytevector src) (imemory:bytevector src))
(define (imemory:bytevector src)
  (let ((len (if (mbytevector? src) (mbytevector-length src) (bytevector-length src))))
    (lambda (method . arg*)
      (apply
        (case method
          ((read)     (lambda (pos dst start count kf keof k)
                        (nonnegative-integer?! pos)
                        (buffer-range?! dst start count count)
                        (if (< 0 count)
                            (let* ((end (min (+ pos count) len)) (count (- end pos)))
                              (if (< 0 count)
                                  (begin (mbytevector-copy! dst start src pos count) (k count))
                                  (keof)))
                            (k 0))))
          ((size)     (lambda (kf k) (k len)))
          ((close)    (lambda (kf k) (k)))
          ((describe) (lambda () '((type . imemory:bytevector))))
          (else       (error "not an imemory:bytevector method" method)))
        arg*))))
(define (omemory:mbytevector buf)
  (mlet ((end.st (mbytevector-length buf)))
    (define (full-error kf name . x*)
      (kf 'no-space (list (vector (vector 'omemory:mbytevector (mbytevector-length buf)) name x*))))
    (lambda (method . arg*)
      (apply (case method
               ((write)    (lambda (pos src start count kf k)
                             (nonnegative-integer?! pos)
                             (buffer-range?! src start count count)
                             (let ((available (- (mbytevector-length buf) pos)))
                               (if (< available count)
                                   (full-error kf 'write pos start count)
                                   (begin (mbytevector-copy! buf pos src start count) (k))))))
               ((size)     (lambda (kf k) end.st))
               ((resize!)  (lambda (new kf k)
                             (nonnegative-integer?! new)
                             (let* ((len (mbytevector-length buf)) (end end.st))
                               (if (< len new)
                                   (full-error kf 'resize! new)
                                   (begin
                                     (when (< end new) (mbytevector-fill! buf 0 end (- new end)))
                                     (set! end.st new)
                                     (k))))))
               ((close)    (lambda (kf k) (k)))
               ((describe) (lambda () '((type . omemory:mbytevector))))
               (else       (error "not an omemory:mbytevector method" method)))
             arg*))))
(define (omemory:bytevector&current) (omemory:bytevector&current/buffer-size 64))
(define (omemory:bytevector&current/buffer-size buffer-size)
  (positive-integer?! buffer-size)
  (mlet ((buf.st (make-mbytevector buffer-size 0)) (end.st 0))
    (define (grow buf len end.copy end.min)
      (let ((new (make-mbytevector (max (+ len len) end.min) 0)))
        (mbytevector-copy! new 0 buf 0 end.copy)
        (set! buf.st new)
        new))
    (values
      (lambda (method . arg*)
        (apply (case method
                 ((write)    (lambda (pos src start count kf k)
                               (nonnegative-integer?! pos)
                               (buffer-range?! src start count count)
                               (let* ((buf buf.st)
                                      (end end.st)
                                      (len (mbytevector-length buf))
                                      (new (+ pos count))
                                      (buf (if (< len new) (grow buf len end new) buf)))
                                 (mbytevector-copy! buf pos src start count)
                                 (when (< end new) (set! end.st new))
                                 (k))))
                 ((size)     (lambda (kf k) end.st))
                 ((resize!)  (lambda (new kf k)
                               (nonnegative-integer?! new)
                               (let* ((buf buf.st) (len (mbytevector-length buf)) (end end.st))
                                 (cond ((< len new) (grow buf len end new))
                                       ((< end new) (mbytevector-fill! buf 0 end (- new end)))))
                               (set! end.st new)
                               (k)))
                 ((close)    (lambda (kf k) (k)))
                 ((describe) (lambda () '((type . omemory:bytevector))))
                 (else       (error "not an omemory:bytevector method" method)))
               arg*))
      (lambda () (mbytevector->bytevector buf.st 0 end.st)))))

;;;;;;;;;;;;;;;;;;;;
;;; Other memory ;;;
;;;;;;;;;;;;;;;;;;;;
(define null-omemory
  (lambda (method . arg*)
    (apply (case method
             ((write)    (lambda (pos src start count kf k)
                           (buffer-range?! src start count count)
                           (k)))
             ((size)     (lambda (kf k)     (k)))
             ((resize!)  (lambda (new kf k) (k)))
             ((close)    (lambda (kf k)     (k)))
             ((describe) (lambda ()         '((type . null-omemory))))
             (else       (error "not a null-omemory method" method)))
           arg*)))
(define full-omemory
  (lambda (method . arg*)
    (define (full-error kf name . x*) (kf 'no-space (list (vector 'full-omemory name x*))))
    (apply (case method
             ((write)    (lambda (pos src start count kf k)
                           (buffer-range?! src start count count)
                           (if (< 0 count) (full-error kf 'write pos start count) (k))))
             ((size)     (lambda (kf k) (k 0)))
             ((resize!)  (lambda (new kf k)
                           (nonnegative-integer?! new)
                           (if (< 0 new) (full-error kf 'resize! new) (k))))
             ((close)    (lambda (kf k) (k)))
             ((describe) (lambda () '((type . full-omemory))))
             (else       (error "not a full-omemory method" method)))
           arg*)))
(define empty-imemory
  (lambda (method . arg*)
    (apply (case method
             ((read)     (lambda (pos dst start count kf keof k)
                           (buffer-range?! dst start count count)
                           (if (< 0 count) (keof) (k 0))))
             ((size)     (lambda (kf k) (k 0)))
             ((close)    (lambda (kf k) (k)))
             ((describe) (lambda ()     '((type . empty-imemory))))
             (else       (error "not an empty-imemory method" method)))
           arg*)))
(define (imemory:constant byte)
  (lambda (method . arg*)
    (apply (case method
             ((read)     (lambda (pos dst start count kf keof k)
                           (buffer-range?! dst start count count)
                           (if (< 0 count)
                               (begin (mbytevector-fill! dst byte start count) (k count))
                               (k 0))))
             ((close)    (lambda (kf k) (k)))
             ((describe) (lambda () '((type . constant-imemory))))
             (else       (error "not a constant-imemory method" method)))
           arg*)))

(splicing-local
  ((define (make-thread-safe-memory-requester description mem)
     (let* ((ch.request (make-channel))
            (t          (thread (lambda ()
                                  (let loop ()
                                    (let* ((req         (channel-get ch.request))
                                           (ch.reply    (car req))
                                           (method&arg* (cdr req))
                                           (result      (current-panic-handler
                                                          (lambda x* (lambda () (apply panic x*)))
                                                          (lambda () (apply mem method&arg*)))))
                                      (thread (lambda () (channel-put ch.reply result)))
                                      (loop))))))
            (dead       (handle-evt (thread-dead-evt t)
                                    (lambda (_) (error "dead thread-safe-memory" description)))))
       (lambda req (let ((ch.reply (make-channel)))
                     (sync (channel-put-evt ch.request (cons ch.reply req)) dead)
                     ((sync ch.reply dead)))))))
  (define (thread-safe-imemory im)
    (let* ((description (cons '(type . thread-safe-imemory) (iomemory-describe im)))
           (request     (make-thread-safe-memory-requester description im)))
      (lambda (method . arg*)
        (apply (case method
                 ((read)     (lambda (pos dst start count kf keof k)
                               (request 'read pos dst start count
                                        (lambda (t ctx)  (lambda () (kf t ctx)))
                                        (lambda ()       keof)
                                        (lambda (amount) (lambda () (k amount))))))
                 ((close)    (lambda (kf k) (request 'close
                                                     (lambda (t ctx) (lambda () (kf t ctx)))
                                                     (lambda ()      (lambda () (k))))))
                 ((describe) (lambda () description))
                 (else       (error "not a thread-safe-imemory method" method description)))
               arg*))))
  (define (thread-safe-omemory om)
    (let* ((description (cons '(type . thread-safe-omemory) (iomemory-describe om)))
           (request     (make-thread-safe-memory-requester description om)))
      (lambda (method . arg*)
        (apply (case method
                 ((write)    (lambda (pos src start count kf k)
                               (request 'write pos src start count
                                        (lambda (t ctx) (lambda () (kf t ctx)))
                                        (lambda ()      (lambda () (k))))))
                 ((size)     (lambda (kf k) (request 'size
                                                     (lambda (t ctx) (lambda () (kf t ctx)))
                                                     (lambda ()      (lambda () (k))))))
                 ((resize!)  (lambda (new kf k) (request 'resize! new
                                                         (lambda (t ctx) (lambda () (kf t ctx)))
                                                         (lambda ()      (lambda () (k))))))
                 ((close)    (lambda (kf k) (request 'close
                                                     (lambda (t ctx) (lambda () (kf t ctx)))
                                                     (lambda ()      (lambda () (k))))))
                 ((describe) (lambda () description))
                 (else       (error "not a thread-safe-oport method" method)))
               arg*)))))

;;;;;;;;;;;;;;;;;;;;
;;; Memory ports ;;;
;;;;;;;;;;;;;;;;;;;;
(define (iport:memory im pos close?)
  (let ((description (cons '(type . iport:memory) (iomemory-describe im))))
    (mlet ((pos pos))
      (lambda (method . arg*)
        (apply (case method
                 ((read)     (lambda (dst start min-count count kf keof k)
                               (let ((i pos))
                                 (imemory-read/k im pos dst start count kf keof
                                                 (lambda (n) (set! pos (+ i n)) (k n))))))
                 ((unread)   (lambda (src start count kf k)
                               (let ((i pos))
                                 (when (< i count)
                                   (error "too many bytes unread" count (cons 'position i)
                                          description))
                                 (set! pos (- i count))
                                 (k))))
                 ((close)    (lambda (kf k) (if close? (imemory-close/k im kf k) (k))))
                 ((describe) (lambda () description))
                 (else       (error "not a iport:memory method" method description)))
               arg*)))))
(define (oport:memory om pos close?)
  (let ((description (cons '(type . oport:memory) (iomemory-describe om))))
    (mlet ((pos pos))
      (lambda (method . arg*)
        (apply (case method
                 ((write)    (lambda (src start min-count count kf k)
                               (let ((i pos))
                                 (omemory-write/k om i src start count kf
                                                  (lambda () (set! pos (+ i count)) (k count))))))
                 ((close)    (lambda (kf k) (if close? (omemory-close/k om kf k) (k))))
                 ((describe) (lambda () description))
                 (else       (error "not a oport:memory method" method description)))
               arg*)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector ports ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;; These bytevector port definitions are not safe for concurrent access unless synchronization is
;;; provided by the user.
(define (iport:mbytevector src) (iport:bytevector src))
(define (iport:bytevector src)
  (let ((len (if (mbytevector? src) (mbytevector-length src) (bytevector-length src))))
    (mlet ((pos 0))
      (define (do-read update-pos? i dst start min-count count kf keof k)
        (buffer-range?! dst start min-count count)
        (if (< 0 count)
            (let* ((end (min (+ i count) len)) (count (- end i)))
              (if (< 0 count)
                  (begin (mbytevector-copy! dst start src i count)
                         (when update-pos? (set! pos end))
                         (k count))
                  (keof)))
            (k 0)))
      (lambda (method . arg*)
        (apply
          (case method
            ((read)          (lambda (dst start min-count count kf keof k)
                               (do-read #t pos dst start min-count count kf keof k)))
            ((unread)        (lambda (src start count kf k)
                               (buffer-range?! src start count count)
                               (let ((i pos))
                                 (when (< i count) (error "too many bytes unread" count
                                                          (vector 'input-bytevector i len)))
                                 (set! pos (- i count))
                                 (k))))
            ((close)         (lambda (kf k) (k)))
            ((describe)      (lambda ()     '((type . input-bytevector))))
            (else            (error "not an input-bytevector method" method)))
          arg*)))))
(define (oport:mbytevector buf)
  (mlet ((pos.st 0))
    (define (full-error kf pos.current name . x*)
      (kf 'no-space (list (vector (vector 'output-mbytevector pos.current (mbytevector-length buf))
                                  name x*))))
    (define (do-write update-pos? pos.current src start min-count count kf k)
      (buffer-range?! src start min-count count)
      (let ((amount (min (- (mbytevector-length buf) pos.current) count)))
        (if (< amount min-count)
            (kf)
            (begin (mbytevector-copy! buf pos.current src start amount)
                   (if update-pos? (begin (set! pos.st (+ pos.current amount)) (k amount)) (k))))))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (src start min-count count kf k)
                             (do-write #t pos.st src start min-count count
                                       (lambda ()
                                         (full-error kf pos.st 'write start min-count count))
                                       k)))
          ((close)         (lambda (kf k) (k)))
          ((describe)      (lambda ()     '((type . output-mbytevector))))
          (else            (error "not an output-mbytevector method" method)))
        arg*))))
(define (oport:bytevector&current) (oport:bytevector&current/buffer-size 64))
(define (oport:bytevector&current/buffer-size buffer-size)
  (positive-integer?! buffer-size)
  (mlet ((buf.st (make-mbytevector buffer-size 0)) (pos.st 0) (end.st 0))
    (define (grow buf len end.copy end.min)
      (let ((new (make-mbytevector (max (+ len len) end.min) 0)))
        (mbytevector-copy! new 0 buf 0 end.copy)
        (set! buf.st new)
        new))
    (define (do-write/copy! update-pos? pos.0 count copy! k)
      (let* ((buf buf.st)
             (end end.st)
             (len (mbytevector-length buf))
             (pos (+ pos.0 count))
             (buf (if (< len pos) (grow buf len end pos) buf)))
        (copy! buf)
        (when (< end pos) (set! end.st pos))
        (if update-pos? (begin (set! pos.st pos) (k count)) (k))))
    (define (do-write/src update-pos? pos.0 src start min-count count k)
      (buffer-range?! src start min-count count)
      (do-write/copy! update-pos? pos.0 count
                      (lambda (buf) (mbytevector-copy! buf pos.0 src start count))
                      k))
    (values (lambda (method . arg*)
              (apply
                (case method
                  ((write)         (lambda (src start min-count count kf k)
                                     (do-write/src #t pos.st src start min-count count k)))
                  ((close)         (lambda (kf k) (k)))
                  ((describe)      (lambda ()     '((type . output-bytevector))))
                  (else            (error "not an output-bytevector method" method)))
                arg*))
            (lambda () (mbytevector->bytevector buf.st 0 end.st)))))

(define (call-with-oport:bytevector k)
  (let-values (((out current) (oport:bytevector&current))) (k out) (current)))
(define call/oport:bytevector call-with-oport:bytevector)

;;;;;;;;;;;;;;;;;;;
;;; Other ports ;;;
;;;;;;;;;;;;;;;;;;;
(define null-oport
  (lambda (method . arg*)
    (apply (case method
             ((write)         (lambda (src start min-count count kf k)
                                (buffer-range?! src start min-count count)
                                (k count)))
             ((close)         (lambda (kf k)                     (k)))
             ((describe)      (lambda () '((type . null-oport))))
             (else            (error "not a null-oport method" method)))
           arg*)))
(define full-oport
  (lambda (method . arg*)
    (define (full-error kf name . x*) (kf 'no-space (list (vector 'full-oport name x*))))
    (apply (case method
             ((write)         (lambda (src start min-count count kf k)
                                (buffer-range?! src start min-count count)
                                (if (< 0 min-count)
                                    (full-error kf 'write start min-count count)
                                    (k 0))))
             ((close)         (lambda (kf k)     (k)))
             ((describe)      (lambda () '((type . full-oport))))
             (else            (error "not a full-oport method" method)))
           arg*)))
(define empty-iport
  (lambda (method . arg*)
    (apply
      (case method
        ((read)          (lambda (dst start min-count count kf keof k)
                           (buffer-range?! dst start min-count count)
                           (if (< 0 count) (keof) (k 0))))
        ((unread)        (lambda (src start count kf k)
                           (buffer-range?! src start count count)
                           (error "too many bytes unread" count 'empty-iport)))
        ((close)         (lambda (kf k)     (k)))
        ((describe)      (lambda () '((type . empty-iport))))
        (else            (error "not an empty-iport method" method)))
      arg*)))
(define (iport:constant byte)
  (lambda (method . arg*)
    (apply
      (case method
        ((read)          (lambda (dst start min-count count kf keof k)
                           (buffer-range?! dst start min-count count)
                           (if (< 0 count)
                               (begin (mbytevector-fill! dst byte start count) (k count))
                               (k 0))))
        ((unread)        (lambda (src start count kf k) (buffer-range?! src start count count) (k)))
        ((close)         (lambda (kf k)     (k)))
        ((describe)      (lambda () '((type . constant-iport))))
        (else            (error "not a constant-iport method" method)))
      arg*)))

(splicing-local
  ((define (make-thread-safe-port-requester description port)
     (let* ((ch.request (make-channel))
            (t          (thread (lambda ()
                                  (let loop ()
                                    (let* ((req         (channel-get ch.request))
                                           (ch.reply    (car req))
                                           (method&arg* (cdr req))
                                           (result      (current-panic-handler
                                                          (lambda x* (lambda () (apply panic x*)))
                                                          (lambda () (apply port method&arg*)))))
                                      (thread (lambda () (channel-put ch.reply result)))
                                      (loop))))))
            (dead       (handle-evt (thread-dead-evt t)
                                    (lambda (_) (error "dead thread-safe-port" description)))))
       (lambda req (let ((ch.reply (make-channel)))
                     (sync (channel-put-evt ch.request (cons ch.reply req)) dead)
                     ((sync ch.reply dead)))))))
  (define (thread-safe-iport port)
    (let* ((description (list '(type . thread-safe-iport) (cons 'sub-port (port-describe port))))
           (request     (make-thread-safe-port-requester description port)))
      (lambda (method . arg*)
        (apply
          (case method
            ((read)     (lambda (dst start min-count count kf keof k)
                          (request 'read dst start min-count count
                                   (lambda (t ctx)  (lambda () (kf t ctx)))
                                   (lambda ()       keof)
                                   (lambda (amount) (lambda () (k amount))))))
            ((unread)   (lambda (src start count kf k)
                          (request 'unread src start count
                                   (lambda (t ctx) (lambda () (kf t ctx)))
                                   (lambda ()      (lambda () (k))))))
            ((close)    (lambda (kf k)
                          (request 'close
                                   (lambda (t ctx) (lambda () (kf t ctx)))
                                   (lambda ()      (lambda () (k))))))
            ((describe) (lambda () description))
            (else       (error "not a thread-safe-iport method" method)))
          arg*))))
  (define (thread-safe-oport port)
    (let* ((description (list '(type . thread-safe-oport) (cons 'sub-port (port-describe port))))
           (request     (make-thread-safe-port-requester description port)))
      (lambda (method . arg*)
        (apply
          (case method
            ((write)    (lambda (src start min-count count kf k)
                          (request 'write src start min-count count
                                   (lambda (t ctx)  (lambda () (kf t ctx)))
                                   (lambda (amount) (lambda () (k amount))))))
            ((close)    (lambda (kf k)
                          (request 'close
                                   (lambda (t ctx) (lambda () (kf t ctx)))
                                   (lambda ()      (lambda () (k))))))
            ((describe) (lambda () description))
            (else       (error "not a thread-safe-oport method" method)))
          arg*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input port-buffers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(splicing-local
  ((define default-iport-buffer-size 4096)
   (define (make-iport-buffer port buffer pos end eof? target-buffer-size)
     (mvector port buffer pos end eof? target-buffer-size))
   (define (iport-buffer-port             p)   (mvector-ref p 0))
   (define (iport-buffer-buffer           p)   (mvector-ref p 1))
   (define (iport-buffer-pos              p)   (mvector-ref p 2))
   (define (iport-buffer-end              p)   (mvector-ref p 3))
   (define (iport-buffer-eof?             p)   (mvector-ref p 4))
   (define (iport-buffer-target-size      p)   (mvector-ref p 5))
   (define (iport-buffer-set-port!        p x) (mvector-set! p 0 x))
   (define (iport-buffer-set-buffer!      p x) (mvector-set! p 1 x))
   (define (iport-buffer-set-pos!         p x) (mvector-set! p 2 x))
   (define (iport-buffer-set-end!         p x) (mvector-set! p 3 x))
   (define (iport-buffer-set-eof?!        p x) (mvector-set! p 4 x))
   (define (iport-buffer-set-target-size! p x) (mvector-set! p 5 x))
   (define (iport-buffer-buffer/refresh p)
     (let ((buf (iport-buffer-buffer p)) (size (iport-buffer-target-size p)))
       (if (< size (mbytevector-length buf))
           (let ((buf (make-mbytevector size 0)))
             (iport-buffer-set-buffer! p buf)
             buf)
           buf))))

  (define iport->iport-buffer
    (let ((go (lambda (p buffer-size)
                (make-iport-buffer p (make-mbytevector buffer-size 0) 0 0 #f buffer-size))))
      (case-lambda
        ((p)             (go p default-iport-buffer-size))
        ((p buffer-size) (positive-integer?! buffer-size) (go p buffer-size)))))

  (define iport-buffer-set-buffer-size!
    (let ((go (lambda (p size)
                (iport-buffer-set-target-size! p size)
                (let* ((buf  (iport-buffer-buffer p))
                       (pos  (iport-buffer-pos p))
                       (end  (- (iport-buffer-end p) pos))
                       (size (max size end)))
                  (unless (= size (mbytevector-length buf))
                    (let ((new (make-mbytevector size 0)))
                      (mbytevector-copy! new 0 buf pos end)
                      (iport-buffer-set-buffer! p new)
                      (iport-buffer-set-pos!    p 0)
                      (iport-buffer-set-end!    p end)))))))
      (case-lambda
        ((p)      (go p default-iport-buffer-size))
        ((p size) (positive-integer?! size) (go p size)))))

  (define (iport-buffer-close p) (iport-buffer-close/k p raise-io-error values))
  (define (iport-buffer-close/k p kf k)
    (let ((port (iport-buffer-port p)))
      (cond (port (iport-buffer-set-port!   p #f)
                  (iport-buffer-set-buffer! p #f)
                  (iport-buffer-set-pos!    p 0)
                  (iport-buffer-set-end!    p 0)
                  (iport-buffer-set-eof?!   p #f)
                  (iport-close/k port kf k))
            (else (k)))))

  (define (iport-buffer-peeked-count p) (- (iport-buffer-end p) (iport-buffer-pos p)))

  (define (iport-buffer-drop-peeked p count)
    (nonnegative-integer?! count)
    (let ((pos (+ (iport-buffer-pos p) count)) (end (iport-buffer-end p)))
      (iport-buffer-set-pos! p (if (< end pos) (begin (iport-buffer-set-eof?! p #f) end) pos))))

  ;; Reverts all peeked bytes, returning them to the underlying port via unread.
  ;; Cannot unpeek if EOF is currently in the peek buffer.
  (define (iport-buffer-unpeek p) (iport-buffer-unpeek/k p raise-io-error values))
  (define (iport-buffer-unpeek/k p kf k)
    (when (iport-buffer-eof? p) (error "cannot unpeak EOF"))
    (let ((pos (iport-buffer-pos p)) (end (iport-buffer-end p)))
      (iport-unread/k (iport-buffer-port p) (iport-buffer-buffer p) pos (- end pos) kf
                      (lambda ()
                        (iport-buffer-set-pos! p 0)
                        (iport-buffer-set-end! p 0)
                        (iport-buffer-buffer/refresh p)
                        (k)))))

  ;; Returns (values) on EOF or amount read.
  (define (iport-buffer-peek-byte   p skip)
    (iport-buffer-peek-byte/k p skip raise-io-error values values))
  (define (iport-buffer-peek-byte/k p skip kf keof k)
    (nonnegative-integer?! skip)
    (let ((buf (iport-buffer-buffer p)) (pos (iport-buffer-pos p)) (end (iport-buffer-end p)))
      (cond
        ((< (+ pos skip) end)  (k (mbytevector-ref buf (+ pos skip))))
        ((iport-buffer-eof? p) (keof))
        (else (let ((len (mbytevector-length buf)))
                (define (fill-and-peek buf.new len)
                  (let ((end (- end pos)))
                    (mbytevector-copy! buf.new 0 buf pos end)
                    (iport-buffer-set-pos! p 0)
                    (case-values (iport-read/k (iport-buffer-port p) buf.new end (- (+ skip 1) end)
                                               (- len end) values values values)
                      (()       (iport-buffer-set-end! p end) (iport-buffer-set-eof?! p #t) (keof))
                      ((amount) (let ((end (+ end amount)))
                                  (iport-buffer-set-end! p end)
                                  (if (< skip end)
                                      (k (mbytevector-ref buf.new skip))
                                      (begin (iport-buffer-set-eof?! p #t) (keof)))))
                      ((tag x)  (kf tag (cons (vector 'iport-buffer-peek-byte skip) x))))))
                (if (<= len skip)
                    (let* ((len (max (+ skip 1) (+ len len))) (new (make-mbytevector len 0)))
                      (iport-buffer-set-buffer! p new)
                      (fill-and-peek new len))
                    (fill-and-peek buf len)))))))

  ;; Returns (values) on EOF or amount read.
  (define (iport-buffer-read-byte   p) (iport-buffer-read-byte/k p raise-io-error values values))
  (define (iport-buffer-read-byte/k p kf keof k)
    (let ((dst (make-mbytevector 1 0)))
      (iport-buffer-read/k p dst 0 1 1 kf keof (lambda (amount) (k (mbytevector-ref dst 0))))))

  ;; Returns (values) on EOF or amount read.
  (define (iport-buffer-read p dst start min-count count)
    (iport-buffer-read/k p dst start min-count count raise-io-error values values))
  (define (iport-buffer-read/k p dst start min-count count kf keof k)
    (define (fail tag ctx) (kf tag (cons (vector 'iport-buffer-read start min-count count) ctx)))
    (buffer-range?! dst start min-count count)
    (let* ((pos (iport-buffer-pos p)) (end (iport-buffer-end p)) (available (- end pos)))
      (let ((amount (min count available)))
        (iport-buffer-set-pos! p (+ pos amount))
        (mbytevector-copy! dst start (iport-buffer-buffer p) pos amount))
      (cond
        ((<= count available)     (k count))
        ((iport-buffer-eof? p)    (iport-buffer-set-eof?! p #f)
                                  (if (< 0 available) (k available) (keof)))
        ((<= min-count available) available)
        (else (let ((buf (iport-buffer-buffer/refresh p)))
                (let ((start     (+ start available))
                      (count     (- count available))
                      (min-count (- min-count available))
                      (len       (mbytevector-length buf))
                      (port      (iport-buffer-port p)))
                  (if (< (+ count count) len)  ; only use buffer if count is small
                      (case-values (iport-read/k port buf 0 min-count len values values values)
                        (()       (k available))
                        ((amount) (mbytevector-copy! dst start buf 0 (min count amount))
                                  (k (if (< count amount)
                                         (begin (iport-buffer-set-pos! p count)
                                                (iport-buffer-set-end! p amount)
                                                (+ available count))
                                         (+ available amount))))
                        ((tag x)  (fail tag x)))
                      (case-values (iport-read/k
                                     port dst start min-count count values values values)
                        (()       (if (< 0 available) (k available) (keof)))
                        ((amount) (k (+ available amount)))
                        ((tag x)  (fail tag x)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output port-buffers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(splicing-local
  ((define default-oport-buffer-size 4096)
   (define (make-oport-buffer port buffer pos) (mvector port buffer pos))
   (define (oport-buffer-port        p)   (mvector-ref p 0))
   (define (oport-buffer-buffer      p)   (mvector-ref p 1))
   (define (oport-buffer-pos         p)   (mvector-ref p 2))
   (define (oport-buffer-set-port!   p x) (mvector-set! p 0 x))
   (define (oport-buffer-set-buffer! p x) (mvector-set! p 1 x))
   (define (oport-buffer-set-pos!    p x) (mvector-set! p 2 x)))

  (define oport->oport-buffer
    (let ((go (lambda (p buffer-size) (make-oport-buffer p (make-mbytevector buffer-size 0) 0))))
      (case-lambda
        ((p)             (go p default-oport-buffer-size))
        ((p buffer-size) (positive-integer?! buffer-size) (go p buffer-size)))))

  (define oport-buffer-set-buffer-size!
    (let ((go (lambda (p size)
                (let ((buf (oport-buffer-buffer p)) (pos (oport-buffer-pos p)))
                  (when (< size pos) (oport-buffer-flush p))
                  (unless (= size (mbytevector-length buf))
                    (let ((new (make-mbytevector size 0)))
                      (mbytevector-copy! new 0 buf 0 pos)
                      (oport-buffer-set-buffer! p new)))))))
      (case-lambda
        ((p)      (go p default-oport-buffer-size))
        ((p size) (nonnegative-integer?! size) (go p size)))))

  (define (oport-buffer-close p) (oport-buffer-close/k p raise-io-error values))
  (define (oport-buffer-close/k p kf k)
    (let ((port (oport-buffer-port p)))
      (if port
          (oport-buffer-flush/k p kf (lambda ()
                                       (oport-buffer-set-port! p #f)
                                       (oport-buffer-set-buffer! p #f)
                                       (oport-close/k port kf k)))
          (k))))

  (define (oport-buffer-flush p) (oport-buffer-flush/k p raise-io-error values))
  (define (oport-buffer-flush/k p kf k)
    (let ((pos (oport-buffer-pos p)))
      (case-values (oport-write/k (oport-buffer-port p) (oport-buffer-buffer p)
                                  0 pos pos values values)
        ((tag x) (kf tag (cons 'oport-buffer-flush x)))
        (_       (oport-buffer-set-pos! p 0) (k)))))

  (define (oport-buffer-write-byte p byte) (oport-buffer-write-byte/k p byte raise-io-error values))
  (define (oport-buffer-write-byte/k p byte kf k)
    (oport-buffer-write/k p (bytevector byte) 0 1 1 kf k))

  ;; Returns the "amount" written.  Block until at least min-count bytes are written.
  (define (oport-buffer-write p src start min-count count)
    (oport-buffer-write/k p src start min-count count raise-io-error values))
  (define (oport-buffer-write/k p src start min-count count kf k)
    (buffer-range?! src start min-count count)
    (let* ((buf       (oport-buffer-buffer p))
           (len       (mbytevector-length buf))
           (pos       (oport-buffer-pos p))
           (available (- len pos)))
      (define (do-write/k src start min-count count kf k)
        (oport-write/k (oport-buffer-port p) src start min-count len kf k))
      (define (drain/k min-count kf k)
        (do-write/k buf 0 min-count len kf (lambda (amount)
                                             (let ((pos (- len amount)))
                                               (mbytevector-copy! buf 0 buf amount pos)
                                               (k pos)))))
      (define (fill pos available start count)
        (let ((amount (min count available)))
          (mbytevector-copy! buf pos src start amount)
          (if (= amount available)
              (drain/k 0 kf (lambda (pos) (oport-buffer-set-pos! p pos) (k amount)))
              (begin (oport-buffer-set-pos! p (+ pos amount)) (k amount)))))
      (cond
        ((= len 0)                (do-write/k src start min-count count kf k))
        ((<= min-count available) (if (< 0 count) (fill pos available start count) (k 0)))
        (else (mbytevector-copy! buf pos src start available)
              (+ (let ((start     (+ start available))
                       (count     (- count available))
                       (min-count (- min-count available)))
                   (if (< min-count len)
                       (drain/k min-count kf (lambda (pos) (fill pos (- len pos) start count)))
                       (begin (oport-buffer-flush/k
                                p kf (lambda () (do-write/k src start min-count count kf k))))))
                 (k available)))))))
