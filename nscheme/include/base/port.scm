(define (buffer-range?! buf start min-count desired-count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! min-count)
  (nonnegative-integer?! desired-count)
  (let ((len (if (mbytevector? buf) (mbytevector-length buf) (bytevector-length buf))))
    (unless (<= (+ start min-count) (+ start desired-count) len)
      (error "buffer range out of bounds" start min-count desired-count len))))

;;; Improper use of a port operation will panic.  Proper use of a port operation may still fail,
;;; indicated by returning two values:
;;; - failure tag, typically a symbol, #f, or an integer code
;;;   - #f        ; failure is not categorized
;;;   - <integer> ; e.g., an errno value
;;;   - exists
;;;   - not-open
;;;   - no-space
;;;   - no-position
;;; - failed operation details

(define (port? x) (procedure? x))
(define (port-description s) (s 'description))

;;;;;;;;;;;;;;;;;;;
;;; Input ports ;;;
;;;;;;;;;;;;;;;;;;;
(define (iport-close/k s kf k) (s 'close kf k))
(define (iport-close   s)      (iport-close/k s raise-io-error values))
;; Returns EOF, the byte read, or a failure indication.
(define (iport-read-byte/k s kf keof k) (s 'read-byte kf keof k))
(define (iport-read-byte   s)           (iport-read-byte/k s raise-io-error values values))
;; Returns EOF, the amount read, or a failure indication.
;; Blocks until at least (min min-count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (iport-read/k s dst start min-count count kf keof k)
  (s 'read dst start min-count count kf keof k))
(define (iport-read s dst start min-count count)
  (iport-read/k s dst start min-count count raise-io-error values values))
;; Returns #f if port does not have a position.
(define (iport-position s) (s 'position))

;;; Input ports with a position
;; When pos is #f, set position to EOF.  May return a failure indication.
(define (iport-set-position!/k s pos kf k) (s 'set-position! pos kf k))
(define (iport-set-position!   s pos)      (iport-set-position!/k s pos raise-io-error values))
;; Returns EOF, the amount read, or a failure indication.
;; Blocks until at least (min count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (iport-pread/k s pos dst start count kf keof k) (s 'pread pos dst start count kf keof k))
(define (iport-pread   s pos dst start count)
  (iport-pread/k s pos dst start count raise-io-error values values))

;;;;;;;;;;;;;;;;;;;;
;;; Output ports ;;;
;;;;;;;;;;;;;;;;;;;;
(define (oport-close/k s kf k) (s 'close kf k))
(define (oport-close   s)      (oport-close/k s raise-io-error values))
;; May return a failure indication.
(define (oport-write-byte/k s byte kf k) (s 'write-byte byte kf k))
(define (oport-write-byte   s byte)      (oport-write-byte/k s byte raise-io-error values))
;; Returns the amount written, or a failure indication.
;; Blocks until at least min-count bytes are written.
;; Failure may occur after a partial write.
(define (oport-write/k s src start min-count count kf k)
  (s 'write src start min-count count kf k))
(define (oport-write   s src start min-count count)
  (oport-write/k s src start min-count count raise-io-error values))
;; Returns #f if port does not have a position.
(define (oport-position s) (s 'position))

;;; Output ports with a position
;; When pos is #f, set position to EOF.  May return a failure indication.
(define (oport-set-position!/k s pos kf k) (s 'set-position! pos kf k))
(define (oport-set-position!   s pos)      (oport-set-position!/k s pos raise-io-error values))
;; May return a failure indication.
(define (oport-set-size!/k s size kf k) (s 'set-size! size kf k))
(define (oport-set-size!   s size)      (oport-set-size!/k s size raise-io-error values))
;; May return a failure indication.
;; Blocks until count bytes are written.
;; Failure may occur after a partial write.
(define (oport-pwrite/k s pos src start count kf k) (s 'pwrite pos src start count kf k))
(define (oport-pwrite s pos src start count)
  (oport-pwrite/k s pos src start count raise-io-error values))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector ports ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;; These bytevector port definitions are not safe for concurrent access unless synchronization is
;;; provided by the user.
(define (open-input-mbytevector src) (open-input-bytevector src))
(define (open-input-bytevector src)
  (let ((len (if (mbytevector? src) (mbytevector-length src) (bytevector-length src))))
    (mlet ((pos 0))
      (define (do-read update-pos? i dst start min-count count kf keof k)
        (buffer-range?! dst start min-count count)
        (if (< 0 count)
            (let* ((end (min (+ i count) len)) (count (- end i)))
              (if (< 0 count)
                  (begin (mbytevector-copy! src i dst start count)
                         (when update-pos? (set! pos end))
                         (k count))
                  (keof)))
            (k 0)))
      (lambda (method . arg*)
        (apply
          (case method
            ((read)          (lambda (dst start min-count count kf keof k)
                               (do-read #t pos dst start min-count count kf keof k)))
            ((pread)         (lambda (pos dst start count kf keof k)
                               (do-read #f pos dst start count count kf keof k)))
            ((read-byte)     (lambda (kf keof k) (let ((i pos))
                                                   (if (< i len)
                                                       (begin (set! pos (+ i 1))
                                                              (if (mbytevector? src)
                                                                  (k (mbytevector-ref src i))
                                                                  (k (bytevector-ref src i))))
                                                       (keof)))))
            ((set-position!) (lambda (new kf k)
                               (set! pos (if new
                                             (begin (nonnegative-integer?! new)
                                                    (min len new))
                                             len))
                               (k)))
            ((position)      (lambda ()     pos))
            ((close)         (lambda (kf k) (k)))
            ((description)   (lambda ()     '((type . input-bytevector))))
            (else            (error "not an input-bytevector method" method)))
          arg*)))))

(define (open-output-mbytevector buf)
  (mlet ((pos.st 0))
    (define (full-error kf pos.current name . x*)
      (kf 'no-space (list (vector (vector 'output-mbytevector pos.current (mbytevector-length buf))
                                  name x*))))
    (define (do-write update-pos? pos.current src start min-count count kf k)
      (buffer-range?! src start min-count count)
      (let ((amount (min (- (mbytevector-length buf) pos.current) count)))
        (if (< amount min-count)
            (kf)
            (begin (mbytevector-copy! src start buf pos.current amount)
                   (if update-pos? (begin (set! pos.st (+ pos.current amount)) (k amount)) (k))))))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (src start min-count count kf k)
                             (do-write #t pos.st src start min-count count
                                       (lambda ()
                                         (full-error kf pos.st 'write start min-count count))
                                       k)))
          ((pwrite)        (lambda (pos src start count kf k)
                             (do-write #f pos src start count count
                                       (lambda () (full-error kf pos.st 'pwrite pos start count))
                                       k)))
          ((write-byte)    (lambda (byte kf k) (let ((i pos.st))
                                                 (if (< i (mbytevector-length buf))
                                                     (begin (mbytevector-set! buf i byte)
                                                            (set! pos.st (+ i 1))
                                                            (k))
                                                     (full-error kf i 'write-byte byte)))))
          ((set-size!)     (lambda (new kf k)
                             (nonnegative-integer?! new)
                             (let ((pos pos.st))
                               (if (< new pos)
                                   (set! pos.st new)
                                   (if (< (mbytevector-length buf) new)
                                       (full-error kf pos 'set-size! new)
                                       (k))))))
          ((set-position!) (lambda (new kf k)
                             (set! pos.st (if new
                                              (begin (nonnegative-integer?! new)
                                                     (min (mbytevector-length buf) new))
                                              (mbytevector-length buf)))
                             (k)))
          ((position)      (lambda ()     pos.st))
          ((close)         (lambda (kf k) (k)))
          ((description)   (lambda ()     '((type . output-mbytevector))))
          (else            (error "not an output-mbytevector method" method)))
        arg*))))

(define (output-bytevector-current s) (s 'current))
(define (open-output-bytevector) (open-output-bytevector/buffer-size 64))
(define (open-output-bytevector/buffer-size buffer-size)
  (positive-integer?! buffer-size)
  (mlet ((buf.st (make-mbytevector buffer-size 0)) (pos.st 0) (end.st 0))
    (define (grow buf len end.copy end.min)
      (let ((new (make-mbytevector (max (+ len len) end.min) 0)))
        (mbytevector-copy! buf 0 new 0 end.copy)
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
                      (lambda (buf) (mbytevector-copy! src start buf pos.0 count))
                      k))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (src start min-count count kf k)
                             (do-write/src #t pos.st src start min-count count k)))
          ((pwrite)        (lambda (pos src start count kf k)
                             (do-write/src #f pos src start count count k)))
          ((write-byte)    (lambda (b kf k)
                             (let ((i pos.st))
                               (do-write/copy! #t i 1 (lambda (buf)
                                                        (mbytevector-set! buf i b)) k))))
          ((set-size!)     (lambda (new kf k)
                             (nonnegative-integer?! new)
                             (let* ((buf buf.st)
                                    (len (mbytevector-length buf))
                                    (end end.st))
                               (cond
                                 ((<= new end) (when (< new pos.st) (set! pos.st new)))
                                 ((<= new len) (mbytevector-fill! buf 0 end (- new end)))
                                 (else         (grow buf len end new))))
                             (set! end.st new)
                             (k)))
          ((set-position!) (lambda (new kf k)
                             (set! pos.st (if new
                                              (begin (nonnegative-integer?! new)
                                                     (min end.st new))
                                              end.st))
                             (k)))
          ((position)      (lambda ()     pos.st))
          ((close)         (lambda (kf k) (k)))
          ((current)       (lambda ()     (mbytevector->bytevector buf.st 0 end.st)))
          ((description)   (lambda ()     '((type . output-bytevector))))
          (else            (error "not an output-bytevector method" method)))
        arg*))))

(define (call-with-input-bytevector bv k) (k (open-input-bytevector bv)))
(define (call-with-output-bytevector   k) (let* ((out (open-output-bytevector)))
                                            (k out)
                                            (output-bytevector-current out)))

;;;;;;;;;;;;;;;;;;;
;;; Other ports ;;;
;;;;;;;;;;;;;;;;;;;
(define null-oport
  (lambda (method . arg*)
    (apply (case method
             ((write)         (lambda (src start min-count count kf k)
                                (buffer-range?! src start min-count count)
                                (k count)))
             ((pwrite)        (lambda (pos src start count kf k) (k)))
             ((write-byte)    (lambda (b kf k)                   (k)))
             ((set-position!) (lambda (new kf k)                 (k)))
             ((set-size!)     (lambda (new kf k)                 (k)))
             ((position)      (lambda ()                         0))
             ((close)         (lambda (kf k)                     (k)))
             ((description)   (lambda () '((type . null-oport))))
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
             ((pwrite)        (lambda (pos src start count kf k)
                                (buffer-range?! src start count count)
                                (if (< 0 count)
                                    (full-error kf 'pwrite pos start count)
                                    (k 0))))
             ((write-byte)    (lambda (b kf k) (full-error kf 'write-byte b)))
             ((set-size!)     (lambda (new kf k)
                                (nonnegative-integer?! new)
                                (if (< 0 new) (full-error kf 'set-size! new) (k))))
             ((set-position!) (lambda (new kf k) (k)))
             ((position)      (lambda ()         0))
             ((close)         (lambda (kf k)     (k)))
             ((description)   (lambda () '((type . full-oport))))
             (else            (error "not a full-oport method" method)))
           arg*)))

(define empty-iport
  (lambda (method . arg*)
    (apply
      (case method
        ((read)          (lambda (dst start min-count count kf keof k)
                           (buffer-range?! dst start min-count count)
                           (if (< 0 count) (keof) (k 0))))
        ((pread)         (lambda (pos dst start count kf keof k)
                           (buffer-range?! dst start count count)
                           (if (< 0 count) (keof) (k 0))))
        ((read-byte)     (lambda (kf keof k) (keof)))
        ((set-position!) (lambda (new kf k) (k)))
        ((position)      (lambda ()         0))
        ((close)         (lambda (kf k)     (k)))
        ((description)   (lambda () '((type . empty-iport))))
        (else            (error "not an empty-iport method" method)))
      arg*)))

(define (open-constant-iport byte)
  (lambda (method . arg*)
    (apply
      (case method
        ((read)          (lambda (dst start min-count count kf keof k)
                           (buffer-range?! dst start min-count count)
                           (if (< 0 count)
                               (begin (mbytevector-fill! dst byte start count) (k count))
                               (k 0))))
        ((pread)         (lambda (pos dst start count kf keof k)
                           (buffer-range?! dst start count count)
                           (if (< 0 count)
                               (begin (mbytevector-fill! dst byte start count) (k count))
                               (k 0))))
        ((read-byte)     (lambda (kf keof k) (k byte)))
        ((set-position!) (lambda (new kf k) (k)))
        ((position)      (lambda ()         0))
        ((close)         (lambda (kf k)     (k)))
        ((description)   (lambda () '((type . constant-iport))))
        (else            (error "not a constant-iport method" method)))
      arg*)))

;;;;;;;;;;;;;;;;;
;;; IO Errors ;;;
;;;;;;;;;;;;;;;;;
;;; Improper use of a port-buffer operation will panic.  Proper use of a port-buffer operation may
;;; still fail by raising an io-error, which will include a failure tag and any additional context.
(define-values (io-error:kind io-error? io-error-tag io-error-context)
  (make-exception-kind-etc error:kind 'io-error '#(tag context)))
(define (make-io-error  desc tag context) (make-exception io-error:kind (vector desc tag context)))
(define (raise-io-error tag context) (raise (make-io-error "IO error" tag context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input port-buffers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(splicing-local
  ((define default-iport-buffer-size 4096)
   (define (make-iport-buffer port buffer pos end eof? target-buffer-size)
     (mvector port buffer pos end eof? target-buffer-size))
   (define (iport-buffer-port             p)   (mvector-ref p 0))
   (define (iport-buffer-data             p)   (mvector-ref p 1))
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
   (define (iport-buffer-data/refresh p)
     (let ((buf (iport-buffer-data p)) (size (iport-buffer-target-size p)))
       (if (< size (mbytevector-length buf))
           (let ((buf (make-mbytevector size 0)))
             (iport-buffer-set-buffer! p buf)
             buf)
           buf))))

  (define (iport-buffer->iport p)
    (if (= (iport-buffer-peeked-count p) 0)
        (iport-buffer-port p)
        (lambda (method . arg*)
          (apply
            (case method
              ((read)          (lambda (dst start min-count count kf keof k)
                                 (iport-buffer-read/k p dst start min-count count kf keof k)))
              ((pread)         (lambda (pos dst start count kf keof k)
                                 (iport-buffer-pread/k p pos dst start count kf keof k)))
              ((read-byte)     (lambda (kf keof k) (iport-buffer-read-byte/k p kf keof k)))
              ((set-position!) (lambda (pos kf k)  (iport-buffer-set-position!/k p pos kf k)))
              ((position)      (lambda ()          (iport-buffer-position p)))
              ((close)         (lambda (kf k)      (iport-buffer-close/k p kf k)))
              ((description)   (lambda ()          '((type . iport-buffer-iport))))
              (else            (error "not an iport-buffer-iport method" method)))
            arg*))))

  (define iport->iport-buffer
    (let ((go (lambda (s buffer-size)
                (make-iport-buffer s (make-mbytevector buffer-size 0) 0 0 #f buffer-size))))
      (case-lambda
        ((s)             (go s default-iport-buffer-size))
        ((s buffer-size) (positive-integer?! buffer-size) (go s buffer-size)))))

  (define iport-buffer-set-buffer-size!
    (let ((go (lambda (p size)
                (iport-buffer-set-target-size! p size)
                (let* ((buf  (iport-buffer-data p))
                       (pos  (iport-buffer-pos p))
                       (end  (- (iport-buffer-end p) pos))
                       (size (max size end)))
                  (unless (= size (mbytevector-length buf))
                    (let ((new (make-mbytevector size 0)))
                      (mbytevector-copy! buf pos new 0 end)
                      (iport-buffer-set-buffer! p new)
                      (iport-buffer-set-pos!    p 0)
                      (iport-buffer-set-end!    p end)))))))
      (case-lambda
        ((p)      (go p default-iport-buffer-size))
        ((p size) (positive-integer?! size) (go p size)))))

  (define (iport-buffer-close p) (iport-buffer-close/k p raise-io-error values))
  (define (iport-buffer-close/k p kf k)
    (let ((s (iport-buffer-port p)))
      (cond (s (iport-buffer-set-port! p #f)
               (iport-buffer-set-buffer! p #f)
               (iport-buffer-set-pos!    p 0)
               (iport-buffer-set-end!    p 0)
               (iport-buffer-set-eof?!   p #f)
               (iport-close/k s kf k))
            (else (k)))))

  (define (iport-buffer-peeked-count p) (- (iport-buffer-end p) (iport-buffer-pos p)))

  (define (iport-buffer-drop-peeked p count)
    (nonnegative-integer?! count)
    (let ((pos (+ (iport-buffer-pos p) count)) (end (iport-buffer-end p)))
      (iport-buffer-set-pos! p (if (< end pos) (begin (iport-buffer-set-eof?! p #f) end) pos))))

  (define (iport-buffer-read-peeked p count)
    (nonnegative-integer?! count)
    (let* ((start (iport-buffer-pos p)) (end (iport-buffer-end p)) (pos (+ start count)))
      (if (and (< 0 count) (= start end) (iport-buffer-eof? p))
          (begin (iport-buffer-set-eof?! p #f) (values))
          (let* ((count (if (< end pos) (begin (iport-buffer-set-eof?! p #f) (- end start)) count))
                 (dst   (make-mbytevector count 0)))
            (iport-buffer-set-pos! p (+ start count))
            (mbytevector-copy! (iport-buffer-data p) start dst 0 count)
            dst))))

  ;; Returns (values) on EOF or amount read.
  (define (iport-buffer-peek-byte   p skip)
    (iport-buffer-peek-byte/k p skip raise-io-error values values))
  (define (iport-buffer-peek-byte/k p skip kf keof k)
    (nonnegative-integer?! skip)
    (let ((buf (iport-buffer-data p)) (pos (iport-buffer-pos p)) (end (iport-buffer-end p)))
      (cond
        ((< (+ pos skip) end) (k (mbytevector-ref buf (+ pos skip))))
        ((iport-buffer-eof? p)       (keof))
        (else (let ((len (mbytevector-length buf)))
                (define (fill-and-peek buf.new len)
                  (let ((end (- end pos)))
                    (mbytevector-copy! buf pos buf.new 0 end)
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
    (let ((buf (iport-buffer-data p)) (pos (iport-buffer-pos p)) (end (iport-buffer-end p)))
      (cond
        ((< pos end)    (iport-buffer-set-pos! p (+ pos 1)) (k (mbytevector-ref buf pos)))
        ((iport-buffer-eof? p) (iport-buffer-set-eof?! p #f) (keof))
        (else (let ((buf (iport-buffer-data/refresh p)))
                (case-values (iport-read/k (iport-buffer-port p) buf 0 1 (mbytevector-length buf)
                                           values values values)
                  (()       (keof))
                  ((amount) (iport-buffer-set-pos! p 1)
                            (iport-buffer-set-end! p amount)
                            (k (mbytevector-ref buf 0)))
                  ((tag x)  (kf tag (cons 'iport-buffer-read-byte x)))))))))

  ;; Returns (values) on EOF or amount read.
  (define iport-buffer-read
    (let ((go (lambda (p dst start min-count count)
                (iport-buffer-read/k p dst start min-count count raise-io-error values values))))
      (case-lambda
        ((p dst start           count) (go p dst start count     count))
        ((p dst start min-count count) (go p dst start min-count count)))))
  (define (iport-buffer-read/k p dst start min-count count kf keof k)
    (define (fail tag ctx) (kf tag (cons (vector 'iport-buffer-read start min-count count) ctx)))
    (buffer-range?! dst start min-count count)
    (let* ((pos (iport-buffer-pos p)) (end (iport-buffer-end p)) (available (- end pos)))
      (let ((amount (min count available)))
        (iport-buffer-set-pos! p (+ pos amount))
        (mbytevector-copy! (iport-buffer-data p) pos dst start amount))
      (cond
        ((<= count available)     (k count))
        ((iport-buffer-eof? p)           (iport-buffer-set-eof?! p #f)
                                         (if (< 0 available) (k available) (keof)))
        ((<= min-count available) available)
        (else (let ((buf (iport-buffer-data/refresh p)))
                (let ((start     (+ start available))
                      (count     (- count available))
                      (min-count (- min-count available))
                      (len       (mbytevector-length buf))
                      (s         (iport-buffer-port p)))
                  (if (< (+ count count) len)  ; only use buffer if count is small
                      (case-values (iport-read/k s buf 0 min-count len values values values)
                        (()       (k available))
                        ((amount) (mbytevector-copy! buf 0 dst start (min count amount))
                                  (k (if (< count amount)
                                         (begin (iport-buffer-set-pos! p count)
                                                (iport-buffer-set-end! p amount)
                                                (+ available count))
                                         (+ available amount))))
                        ((tag x)  (fail tag x)))
                      (case-values (iport-read/k s dst start min-count count values values values)
                        (()       (if (< 0 available) (k available) (keof)))
                        ((amount) (k (+ available amount)))
                        ((tag x)  (fail tag x))))))))))

  ;; returns #f if port has no position
  (define (iport-buffer-position p)
    (let ((pos (iport-position (iport-buffer-port p))))
      (and pos (+ (- pos (iport-buffer-end p)) (iport-buffer-pos p)))))

  ;;; Input port-buffers with a position
  ;; When pos is #f, set position to EOF
  (define (iport-buffer-set-position!   p pos)
    (iport-buffer-set-position!/k p pos raise-io-error values))
  (define (iport-buffer-set-position!/k p pos kf k)
    (iport-buffer-data/refresh p)
    (case-values (iport-set-position!/k (iport-buffer-port p) pos values values)
      ((tag x) (kf tag (cons (vector 'iport-buffer-set-position! pos) x)))
      (_       (iport-buffer-set-pos! p 0) (iport-buffer-set-end! p 0) (k))))

  ;; returns (values) on EOF or amount read.
  (define (iport-buffer-pread p pos dst start count)
    (iport-buffer-pread/k p pos dst start count raise-io-error values values))
  (define (iport-buffer-pread/k p pos dst start count kf keof k)
    (iport-pread/k (iport-buffer-port p) pos dst start count kf keof k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output port-buffers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(splicing-local
  ((define default-oport-buffer-size 4096)
   (define (make-oport-buffer port buffer pos) (mvector port buffer pos))
   (define (oport-buffer-port        p)   (mvector-ref p 0))
   (define (oport-buffer-data        p)   (mvector-ref p 1))
   (define (oport-buffer-pos         p)   (mvector-ref p 2))
   (define (oport-buffer-set-port!   p x) (mvector-set! p 0 x))
   (define (oport-buffer-set-buffer! p x) (mvector-set! p 1 x))
   (define (oport-buffer-set-pos!    p x) (mvector-set! p 2 x)))

  (define (oport-buffer->oport p) (oport-buffer->oport/k p raise-io-error values))
  (define (oport-buffer->oport/k p kf k)
    (oport-buffer-flush/k p kf (lambda () (k (oport-buffer-port p)))))

  (define oport->oport-buffer
    (let ((go (lambda (s buffer-size) (make-oport-buffer s (make-mbytevector buffer-size 0) 0))))
      (case-lambda
        ((s)             (go s default-oport-buffer-size))
        ((s buffer-size) (positive-integer?! buffer-size) (go s buffer-size)))))

  (define oport-buffer-set-buffer-size!
    (let ((go (lambda (p size)
                (let ((buf (oport-buffer-data p)) (pos (oport-buffer-pos p)))
                  (when (< size pos) (oport-buffer-flush p))
                  (unless (= size (mbytevector-length buf))
                    (let ((new (make-mbytevector size 0)))
                      (mbytevector-copy! buf 0 new 0 pos)
                      (oport-buffer-set-buffer! p new)))))))
      (case-lambda
        ((p)      (go p default-oport-buffer-size))
        ((p size) (nonnegative-integer?! size) (go p size)))))

  (define (oport-buffer-close p) (oport-buffer-close/k p raise-io-error values))
  (define (oport-buffer-close/k p kf k)
    (let ((s (oport-buffer-port p)))
      (if s
          (oport-buffer-flush/k p kf (lambda ()
                                       (oport-buffer-set-port! p #f)
                                       (oport-buffer-set-buffer! p #f)
                                       (oport-close/k s kf k)))
          (k))))

  (define (oport-buffer-flush p) (oport-buffer-flush/k p raise-io-error values))
  (define (oport-buffer-flush/k p kf k)
    (let ((pos (oport-buffer-pos p)))
      (case-values (oport-write/k (oport-buffer-port p) (oport-buffer-data p)
                                  0 pos pos values values)
        ((tag x) (kf tag (cons 'oport-buffer-flush x)))
        (_       (oport-buffer-set-pos! p 0) (k)))))

  (define (oport-buffer-write-byte p byte) (oport-buffer-write-byte/k p byte raise-io-error values))
  (define (oport-buffer-write-byte/k p byte kf k)
    (let* ((buf       (oport-buffer-data p))
           (len       (mbytevector-length buf))
           (pos       (oport-buffer-pos p))
           (available (- len pos)))
      (cond
        ((= len 0)       (oport-write-byte/k (oport-buffer-port p) byte kf k))
        ((= available 0) (case-values (oport-write/k (oport-buffer-port p)
                                                     buf 0 1 len values values)
                           ((amount) (let ((pos (- len amount)))
                                       (mbytevector-copy! buf amount buf 0 pos)
                                       (mbytevector-set! buf pos byte)
                                       (oport-buffer-set-pos! p (+ pos 1))
                                       (k)))
                           ((tag x) (kf tag (cons (vector 'oport-buffer-write-byte byte) x)))))
        (else            (mbytevector-set! buf pos byte)
                         (oport-buffer-set-pos! p (+ pos 1))
                         (k)))))

  ;; Returns the "amount" written.  Block until at least min-count bytes are written.
  (define oport-buffer-write
    (let ((go (lambda (p src start min-count count)
                (oport-buffer-write/k p src start min-count count raise-io-error values))))
      (case-lambda
        ((p src)
         (let ((len (if (mbytevector? src) (mbytevector-length src) (bytevector-length src))))
           (go p src 0 len len)))
        ((p src start count)           (go p src start count     count))
        ((p src start min-count count) (go p src start min-count count)))))
  (define (oport-buffer-write/k p src start min-count count kf k)
    (buffer-range?! src start min-count count)
    (let* ((buf       (oport-buffer-data p))
           (len       (mbytevector-length buf))
           (pos       (oport-buffer-pos p))
           (available (- len pos)))
      (define (do-write/k src start min-count count kf k)
        (oport-write/k (oport-buffer-port p) src start min-count len kf k))
      (define (drain/k min-count kf k)
        (do-write/k buf 0 min-count len kf (lambda (amount)
                                             (let ((pos (- len amount)))
                                               (mbytevector-copy! buf amount buf 0 pos)
                                               (k pos)))))
      (define (fill pos available start count)
        (let ((amount (min count available)))
          (mbytevector-copy! src start buf pos amount)
          (if (= amount available)
              (drain/k 0 kf (lambda (pos) (oport-buffer-set-pos! p pos) (k amount)))
              (begin (oport-buffer-set-pos! p (+ pos amount)) (k amount)))))
      (cond
        ((= len 0)                (do-write/k src start min-count count kf k))
        ((<= min-count available) (if (< 0 count) (fill pos available start count) (k 0)))
        (else (mbytevector-copy! src start buf pos available)
              (+ (let ((start     (+ start available))
                       (count     (- count available))
                       (min-count (- min-count available)))
                   (if (< min-count len)
                       (drain/k min-count kf (lambda (pos) (fill pos (- len pos) start count)))
                       (begin (oport-buffer-flush/k
                                p kf (lambda () (do-write/k src start min-count count kf k))))))
                 (k available))))))

  ;; returns #f if port has no position
  (define (oport-buffer-position p)
    (let ((pos (oport-position (oport-buffer-port p))))
      (and pos (+ pos (oport-buffer-pos p)))))

  ;;; Output port-buffers with a position
  ;; When pos is #f, set position to EOF
  (define (oport-buffer-set-position! p pos)
    (oport-buffer-set-position!/k p pos raise-io-error values))
  (define (oport-buffer-set-position!/k p pos kf k)
    (oport-buffer-flush/k
      p kf (lambda ()
             (case-values (oport-set-position!/k (oport-buffer-port p) pos values values)
               ((tag x) (kf tag (cons (vector 'oport-buffer-set-position! pos) x)))
               (_       (oport-buffer-set-pos! p 0) (k))))))

  (define (oport-buffer-set-size! p size) (oport-buffer-set-size!/k p size raise-io-error values))
  (define (oport-buffer-set-size!/k p size kf k)
    (oport-buffer-flush/k p kf (lambda () (oport-set-size!/k (oport-buffer-port p) size kf k))))

  (define (oport-buffer-pwrite p pos src start count)
    (oport-buffer-pwrite/k p pos src start count raise-io-error values))
  (define (oport-buffer-pwrite/k p pos src start count kf k)
    (oport-buffer-flush/k
      p kf (lambda () (oport-pwrite/k (oport-buffer-port p) pos src start count kf k)))))
