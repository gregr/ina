(define (buffer-range?! buf start min-count desired-count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! min-count)
  (nonnegative-integer?! desired-count)
  (let ((len (if (mbytevector? buf) (mbytevector-length buf) (bytevector-length buf))))
    (unless (<= (+ start min-count) (+ start desired-count) len)
      (error "buffer range out of bounds" start min-count desired-count len))))

;;; Improper use of a stream operation will panic.  Proper use of a stream operation may still fail,
;;; indicated by returning two values:
;;; - failure tag, typically a symbol, #f, or an integer code
;;;   - #f        ; failure is not categorized
;;;   - <integer> ; e.g., an errno value
;;;   - exists
;;;   - not-open
;;;   - no-space
;;;   - no-position
;;; - failed operation details

;;;;;;;;;;;;;;;;;;;;;
;;; Input streams ;;;
;;;;;;;;;;;;;;;;;;;;;
(define (istream-close/k s kf k) (s 'close kf k))
(define (istream-close   s)      (istream-close/k s values values))
;; Returns EOF, the byte read, or a failure indication.
(define (istream-read-byte/k s kf keof k) (s 'read-byte kf keof k))
(define (istream-read-byte   s)           (istream-read-byte/k s values values values))
;; Returns EOF, the amount read, or a failure indication.
;; Blocks until at least (min min-count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (istream-read/k s dst start min-count count kf keof k)
  (s 'read dst start min-count count kf keof k))
(define (istream-read s dst start min-count count)
  (istream-read/k s dst start min-count count values values values))
;; Returns #f if stream does not have a position.
(define (istream-position s) (s 'position))

;;; Input streams with a position
;; When pos is #f, set position to EOF.  May return a failure indication.
(define (istream-set-position!/k s pos kf k) (s 'set-position! pos kf k))
(define (istream-set-position!   s pos)      (istream-set-position!/k s pos values values))
;; Returns EOF, the amount read, or a failure indication.
;; Blocks until at least (min count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (istream-pread/k s pos dst start count kf keof k) (s 'pread pos dst start count kf keof k))
(define (istream-pread   s pos dst start count)
  (istream-pread/k s pos dst start count values values values))

;;;;;;;;;;;;;;;;;;;;;;
;;; Output streams ;;;
;;;;;;;;;;;;;;;;;;;;;;
(define (ostream-close/k s kf k) (s 'close kf k))
(define (ostream-close   s)      (ostream-close/k s values values))
;; May return a failure indication.
(define (ostream-write-byte/k s byte kf k) (s 'write-byte byte kf k))
(define (ostream-write-byte   s byte)      (ostream-write-byte/k s byte values values))
;; Returns the amount written, or a failure indication.
;; Blocks until at least min-count bytes are written.
;; Failure may occur after a partial write.
(define (ostream-write/k s src start min-count count kf k)
  (s 'write src start min-count count kf k))
(define (ostream-write   s src start min-count count)
  (ostream-write/k s src start min-count count values values))
;; Returns #f if stream does not have a position.
(define (ostream-position   s)                           (s 'position))

;;; Output streams with a position
;; When pos is #f, set position to EOF.  May return a failure indication.
(define (ostream-set-position!/k s pos kf k) (s 'set-position! pos kf k))
(define (ostream-set-position!   s pos)      (ostream-set-position!/k s pos values values))
;; May return a failure indication.
(define (ostream-set-size!/k s size kf k) (s 'set-size! size kf k))
(define (ostream-set-size!   s size)      (ostream-set-size!/k s size values values))
;; May return a failure indication.
;; Blocks until count bytes are written.
;; Failure may occur after a partial write.
(define (ostream-pwrite/k s pos src start count kf k) (s 'pwrite pos src start count kf k))
(define (ostream-pwrite s pos src start count)
  (ostream-pwrite/k s pos src start count values values))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector streams ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These bytevector stream definitions are not safe for concurrent access unless synchronization is
;;; provided by the user.
(define (open-mbytevector-istream src) (open-bytevector-istream src))
(define (open-bytevector-istream src)
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
            (else            (error "not a bytevector-istream method" method)))
          arg*)))))

(define (open-mbytevector-ostream buf)
  (mlet ((pos.st 0))
    (define (full-error kf pos.current name . x*)
      (kf 'no-space (list (vector (vector 'mbytevector-ostream pos.current (mbytevector-length buf))
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
                                       (lambda () (full-error kf pos.st 'write start min-count count))
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
          (else            (error "not an mbytevector-ostream method" method)))
        arg*))))

(define (bytevector-ostream-current s) (s 'current))
(define (open-bytevector-ostream) (open-bytevector-ostream/buffer-size 64))
(define (open-bytevector-ostream/buffer-size buffer-size)
  (positive-integer?! buffer-size)
  (mlet ((buf.st (make-mbytevector buffer-size 0)) (pos.st 0) (end.st 0))
    (define (grow buf len end.copy end.min)
      (let ((new (make-mbytevector (max (+ len len) end.min) 0)))
        (mbytevector-copy! buf 0 new 0 end.copy)
        (set! buf.st new)
        new))
    (define (do-write/copy! update-pos? pos.0 count copy!)
      (let* ((buf buf.st)
             (end end.st)
             (len (mbytevector-length buf))
             (pos (+ pos.0 count))
             (buf (if (< len pos) (grow buf len end pos) buf)))
        (copy! buf)
        (when (< end pos) (set! end.st pos))
        (if update-pos? (begin (set! pos.st pos) count) (values))))
    (define (do-write/src update-pos? pos.0 src start min-count count)
      (buffer-range?! src start min-count count)
      (do-write/copy! update-pos? pos.0 count
                      (lambda (buf) (mbytevector-copy! src start buf pos.0 count))))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (src start min-count count kf k)
                             (do-write/src #t pos.st src start min-count count)
                             (k)))
          ((pwrite)        (lambda (pos src start count kf k)
                             (do-write/src #f pos src start count count)
                             (k)))
          ((write-byte)    (lambda (b kf k) (let ((i pos.st))
                                              (do-write/copy! #t i 1 (lambda (buf)
                                                                       (mbytevector-set! buf i b)))
                                              (k))))
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
          ((current)       (lambda ()    (mbytevector->bytevector buf.st 0 end.st)))
          (else            (error "not a bytevector-ostream method" method)))
        arg*))))

;;;;;;;;;;;;;;;;;;;;;
;;; Other streams ;;;
;;;;;;;;;;;;;;;;;;;;;
(define null-ostream
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
             (else            (error "not a null-ostream method" method)))
           arg*)))

(define full-ostream
  (lambda (method . arg*)
    (define (full-error kf name . x*) (kf 'no-space (list (vector 'full-ostream name x*))))
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
             (else            (error "not a full-ostream method" method)))
           arg*)))

(define empty-istream
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
        (else            (error "not an empty-istream method" method)))
      arg*)))

(define (open-constant-istream byte)
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
        (else            (error "not a constant-istream method" method)))
      arg*)))

;;;;;;;;;;;;;;;;;
;;; IO Errors ;;;
;;;;;;;;;;;;;;;;;
;;; Improper use of a port operation will panic.  Proper use of a port operation may still fail by
;;; raising an io-error, which will include a stream failure tag and any extra detail.
(define-values (io-error:kind io-error? io-error-tag io-error-context)
  (make-exception-kind-etc error:kind 'io-error '#(tag context)))
(define (make-io-error  desc tag context) (make-exception io-error:kind (vector desc tag context)))
(define (raise-io-error tag context) (raise (make-io-error "IO error" tag context)))

;;;;;;;;;;;;;;;;;;;
;;; Input ports ;;;
;;;;;;;;;;;;;;;;;;;
(splicing-local
  ((define default-iport-buffer-size 4096)
   (define (make-iport stream buffer pos end eof? target-buffer-size)
     (mvector stream buffer pos end eof? target-buffer-size))
   (define (iport-buffer                  p)   (mvector-ref p 1))
   (define (iport-pos                     p)   (mvector-ref p 2))
   (define (iport-end                     p)   (mvector-ref p 3))
   (define (iport-eof?                    p)   (mvector-ref p 4))
   (define (iport-target-buffer-size      p)   (mvector-ref p 5))
   (define (iport-set-stream!             p x) (mvector-set! p 0 x))
   (define (iport-set-buffer!             p x) (mvector-set! p 1 x))
   (define (iport-set-pos!                p x) (mvector-set! p 2 x))
   (define (iport-set-end!                p x) (mvector-set! p 3 x))
   (define (iport-set-eof?!               p x) (mvector-set! p 4 x))
   (define (iport-set-target-buffer-size! p x) (mvector-set! p 5 x))
   (define (iport-buffer/refresh p)
     (let ((buf (iport-buffer p)) (size (iport-target-buffer-size p)))
       (if (< size (mbytevector-length buf))
           (let ((buf (make-mbytevector size 0)))
             (iport-set-buffer! p buf)
             buf)
           buf))))

  (define (iport-stream p) (mvector-ref p 0))

  (define istream->iport
    (let ((go (lambda (s buffer-size)
                (make-iport s (make-mbytevector buffer-size 0) 0 0 #f buffer-size))))
      (case-lambda
        ((s)             (go s default-iport-buffer-size))
        ((s buffer-size) (positive-integer?! buffer-size) (go s buffer-size)))))

  (define iport-set-buffer-size!
    (let ((go (lambda (p size)
                (iport-set-target-buffer-size! p size)
                (let* ((buf  (iport-buffer p))
                       (pos  (iport-pos p))
                       (end  (- (iport-end p) pos))
                       (size (max size end)))
                  (unless (= size (mbytevector-length buf))
                    (let ((new (make-mbytevector size 0)))
                      (mbytevector-copy! buf pos new 0 end)
                      (iport-set-buffer! p new)
                      (iport-set-pos!    p 0)
                      (iport-set-end!    p end)))))))
      (case-lambda
        ((p)      (go p default-iport-buffer-size))
        ((p size) (positive-integer?! size) (go p size)))))

  (define (iport-close p)
    (let ((s (iport-stream p)))
      (when s
        (iport-set-stream! p #f)
        (iport-set-buffer! p #f)
        (iport-set-pos!    p 0)
        (iport-set-end!    p 0)
        (iport-set-eof?!   p #f)
        (istream-close s))))

  (define (iport-drop-peeked p count)
    (nonnegative-integer?! count)
    (let ((pos (+ (iport-pos p) count)) (end (iport-end p)))
      (iport-set-pos! p (if (< end pos) (begin (iport-set-eof?! p #f) end) pos))))

  (define (iport-read-peeked p count)
    (nonnegative-integer?! count)
    (let* ((start (iport-pos p)) (end (iport-end p)) (pos (+ start count)))
      (if (and (< 0 count) (= start end) (iport-eof? p))
          (begin (iport-set-eof?! p #f) (values))
          (let* ((count (if (< end pos) (begin (iport-set-eof?! p #f) (- end start)) count))
                 (dst   (make-mbytevector count 0)))
            (iport-set-pos! p (+ start count))
            (mbytevector-copy! (iport-buffer p) start dst 0 count)
            dst))))

  ;; Returns (values) on EOF or amount read.
  (define (iport-peek-byte p skip)
    (nonnegative-integer?! skip)
    (let ((buf (iport-buffer p)) (pos (iport-pos p)) (end (iport-end p)))
      (cond
        ((< (+ pos skip) end) (mbytevector-ref buf (+ pos skip)))
        ((iport-eof? p)       (values))
        (else (let ((len (mbytevector-length buf)))
                (define (fill-and-peek buf.new len)
                  (let ((end (- end pos)))
                    (mbytevector-copy! buf pos buf.new 0 end)
                    (iport-set-pos! p 0)
                    (case-values (istream-read (iport-stream p) buf.new end (- (+ skip 1) end)
                                               (- len end))
                      (()       (iport-set-end! p end) (iport-set-eof?! p #t) (values))
                      ((amount) (let ((end (+ end amount)))
                                  (iport-set-end! p end)
                                  (if (< skip end)
                                      (mbytevector-ref buf.new skip)
                                      (begin (iport-set-eof?! p #t) (values)))))
                      ((tag x)  (raise-io-error tag (cons (vector 'iport-peek-byte skip) x))))))
                (if (<= len skip)
                    (let* ((len (max (+ skip 1) (+ len len))) (new (make-mbytevector len 0)))
                      (iport-set-buffer! p new)
                      (fill-and-peek new len))
                    (fill-and-peek buf len)))))))

  ;; Returns (values) on EOF or amount read.
  (define (iport-read-byte p)
    (let ((buf (iport-buffer p)) (pos (iport-pos p)) (end (iport-end p)))
      (cond
        ((< pos end)    (iport-set-pos! p (+ pos 1)) (mbytevector-ref buf pos))
        ((iport-eof? p) (iport-set-eof?! p #f) (values))
        (else (let ((buf (iport-buffer/refresh p)))
                (case-values (istream-read (iport-stream p) buf 0 1 (mbytevector-length buf))
                  (()       (values))
                  ((amount) (iport-set-pos! p 1)
                            (iport-set-end! p amount)
                            (mbytevector-ref buf 0))
                  ((tag x)  (raise-io-error tag (cons 'iport-read-byte x)))))))))

  ;; Returns (values) on EOF or amount read.
  (define iport-read
    (let ((go (lambda (p dst start min-count count)
                (define (fail tag ctx)
                  (raise-io-error tag (cons (vector 'iport-read start min-count count) ctx)))
                (buffer-range?! dst start min-count count)
                (let* ((pos (iport-pos p)) (end (iport-end p)) (available (- end pos)))
                  (let ((amount (min count available)))
                    (iport-set-pos! p (+ pos amount))
                    (mbytevector-copy! (iport-buffer p) pos dst start amount))
                  (cond
                    ((<= count available)     count)
                    ((iport-eof? p)           (iport-set-eof?! p #f)
                                              (if (< 0 available) available (values)))
                    ((<= min-count available) available)
                    (else (let ((buf (iport-buffer/refresh p)))
                            (let ((start     (+ start available))
                                  (count     (- count available))
                                  (min-count (- min-count available))
                                  (len       (mbytevector-length buf))
                                  (s         (iport-stream p)))
                              (if (< (+ count count) len)  ; only use buffer if count is small
                                  (case-values (istream-read s buf 0 min-count len)
                                    (()       available)
                                    ((amount) (mbytevector-copy! buf 0 dst start (min count amount))
                                              (if (< count amount)
                                                  (begin (iport-set-pos! p count)
                                                         (iport-set-end! p amount)
                                                         (+ available count))
                                                  (+ available amount)))
                                    ((tag x)  (fail tag x)))
                                  (case-values (istream-read s dst start min-count count)
                                    (()       (if (< 0 available) available (values)))
                                    ((amount) (+ available amount))
                                    ((tag x)  (fail tag x))))))))))))
      (case-lambda
        ((p dst start count)           (go p dst start count     count))
        ((p dst start min-count count) (go p dst start min-count count)))))

  ;; returns #f if port has no position
  (define (iport-position p)
    (let ((pos (istream-position (iport-stream p))))
      (and pos (+ (- pos (iport-end p)) (iport-pos p)))))

  ;;; Input ports with a position
  ;; when pos is #f, set position to EOF
  (define (iport-set-position! p pos)
    (iport-buffer/refresh p)
    (case-values (istream-set-position! (iport-stream p) pos)
      ((tag x) (raise-io-error tag (cons (vector 'iport-set-position! pos) x)))
      (_       (iport-set-pos! p 0) (iport-set-end! p 0))))

  ;; returns (values) on EOF or amount read.
  (define (iport-pread p pos dst start count)
    (case-values (istream-pread (iport-stream p) pos dst start count)
      (()       (values))
      ((amount) amount)
      ((tag x)  (raise-io-error tag (cons (vector 'iport-pread pos start count) x))))))

;;;;;;;;;;;;;;;;;;;;
;;; Output ports ;;;
;;;;;;;;;;;;;;;;;;;;
(splicing-local
  ((define default-oport-buffer-size 4096)
   (define (make-oport stream buffer pos) (mvector stream buffer pos))
   (define (oport-buffer      p)   (mvector-ref p 1))
   (define (oport-pos         p)   (mvector-ref p 2))
   (define (oport-set-stream! p x) (mvector-set! p 0 x))
   (define (oport-set-buffer! p x) (mvector-set! p 1 x))
   (define (oport-set-pos!    p x) (mvector-set! p 2 x)))

  (define (oport-stream p) (mvector-ref p 0))

  (define ostream->oport
    (let ((go (lambda (s buffer-size) (make-oport s (make-mbytevector buffer-size 0) 0))))
      (case-lambda
        ((s)             (go s default-oport-buffer-size))
        ((s buffer-size) (positive-integer?! buffer-size) (go s buffer-size)))))

  (define oport-set-buffer-size!
    (let ((go (lambda (p size)
                (let ((buf (oport-buffer p)) (pos (oport-pos p)))
                  (when (< size pos) (oport-flush p))
                  (unless (= size (mbytevector-length buf))
                    (let ((new (make-mbytevector size 0)))
                      (mbytevector-copy! buf 0 new 0 pos)
                      (oport-set-buffer! p new)))))))
      (case-lambda
        ((p)      (go p default-oport-buffer-size))
        ((p size) (nonnegative-integer?! size) (go p size)))))

  (define (oport-close p)
    (let ((s (oport-stream p)))
      (when s
        (oport-flush p)
        (oport-set-stream! p #f)
        (oport-set-buffer! p #f)
        (ostream-close s))))

  (define (oport-flush p)
    (let ((pos (oport-pos p)))
      (case-values (ostream-write (oport-stream p) (oport-buffer p) 0 pos pos)
        ((tag x) (raise-io-error tag (cons 'oport-flush x)))
        (_       (oport-set-pos! p 0)))))

  (define (oport-write-byte p byte)
    (let* ((buf       (oport-buffer p))
           (len       (mbytevector-length buf))
           (pos       (oport-pos p))
           (available (- len pos)))
      (cond
        ((= len 0)       (case-values (ostream-write-byte (oport-stream p) byte)
                           ((tag x) (raise-io-error tag (cons (vector 'oport-write-byte byte) x)))
                           (_       (values))))
        ((= available 0) (case-values (ostream-write (oport-stream p) buf 0 1 len)
                           ((amount) (let ((pos (- len amount)))
                                       (mbytevector-copy! buf amount buf 0 pos)
                                       (mbytevector-set! buf pos byte)
                                       (oport-set-pos! p (+ pos 1))))
                           ((tag x) (raise-io-error tag (cons (vector 'oport-write-byte byte) x)))))
        (else            (mbytevector-set! buf pos byte)
                         (oport-set-pos! p (+ pos 1))))))

  ;; Returns the "amount" written.  Block until at least min-count bytes are written.
  (define oport-write
    (let ((go (lambda (p src start min-count count)
                (buffer-range?! src start min-count count)
                (let* ((buf       (oport-buffer p))
                       (len       (mbytevector-length buf))
                       (pos       (oport-pos p))
                       (available (- len pos)))
                  (define (do-write src start min-count count)
                    (case-values (ostream-write (oport-stream p) src start min-count len)
                      ((amount) amount)
                      ((tag x)  (raise-io-error
                                  tag (cons (vector 'oport-write start min-count count) x)))))
                  (define (drain min-count)
                    (let* ((amount (do-write buf 0 min-count len))
                           (pos    (- len amount)))
                      (mbytevector-copy! buf amount buf 0 pos)
                      pos))
                  (define (fill pos available start count)
                    (let ((amount (min count available)))
                      (mbytevector-copy! src start buf pos amount)
                      (oport-set-pos! p (if (= amount available) (drain 0) (+ pos amount)))
                      amount))
                  (cond
                    ((= len 0)                (do-write src start min-count count))
                    ((<= min-count available) (if (< 0 count) (fill pos available start count) 0))
                    (else (mbytevector-copy! src start buf pos available)
                          (+ (let ((start     (+ start available))
                                   (count     (- count available))
                                   (min-count (- min-count available)))
                               (if (< min-count len)
                                   (let ((pos (drain min-count)))
                                     (fill pos (- len pos) start count))
                                   (begin (oport-flush p)
                                          (do-write src start min-count count))))
                             available)))))))
      (case-lambda
        ((p src)
         (let ((len (if (mbytevector? src) (mbytevector-length src) (bytevector-length src))))
           (go p src 0 len len)))
        ((p src start count)           (go p src start count     count))
        ((p src start min-count count) (go p src start min-count count)))))

  ;; returns #f if port has no position
  (define (oport-position p)
    (let ((pos (ostream-position (oport-stream p))))
      (and pos (+ pos (oport-pos p)))))

  ;;; Output ports with a position
  ;; when pos is #f, set position to EOF
  (define (oport-set-position! p pos)
    (oport-flush p)
    (case-values (ostream-set-position! (oport-stream p) pos)
      ((tag x) (raise-io-error tag (cons (vector 'oport-set-position! pos) x)))
      (_       (oport-set-pos! p 0))))

  (define (oport-set-size! p size)
    (oport-flush p)
    (case-values (ostream-set-size! (oport-stream p) size)
      ((tag x) (raise-io-error tag (cons (vector 'oport-set-size! size) x)))
      (_       (values))))

  (define (oport-pwrite p pos src start count)
    (oport-flush p)
    (case-values (ostream-pwrite (oport-stream p) pos src start count)
      ((tag x)  (raise-io-error tag (cons (vector 'oport-pwrite pos start count) x)))
      (_        (values)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector ports ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (open-input-mbytevector  src) (istream->iport (open-mbytevector-istream src)))
(define (open-input-bytevector   src) (istream->iport (open-bytevector-istream  src)))
(define (open-output-mbytevector dst) (ostream->oport (open-mbytevector-ostream dst)))
(define (open-output-bytevector)      (ostream->oport (open-bytevector-ostream)))
(define (output-bytevector-current p) (oport-flush p) (bytevector-ostream-current (oport-stream p)))

(define (call-with-input-bytevector   bv  k) (k (open-input-bytevector bv)))
(define (call-with-output-mbytevector mbv k) (let ((p (open-output-mbytevector mbv)))
                                               (let-values ((x* (k p)))
                                                 (oport-flush p)
                                                 (apply values x*))))
(define (call-with-output-bytevector      k) (let* ((out (open-output-bytevector))
                                                    (s   (oport-stream out)))
                                               (k out)
                                               (oport-flush out)
                                               (bytevector-ostream-current s)))

;;;;;;;;;;;;;;;;;;;
;;; Other ports ;;;
;;;;;;;;;;;;;;;;;;;
(define (open-null-output-port)         (ostream->oport null-ostream  0))
(define (open-full-output-port)         (ostream->oport full-ostream  0))
(define (open-empty-input-port)         (istream->iport empty-istream 1))
(define (open-constant-input-port byte) (istream->iport (open-constant-istream byte)))
