(define (buffer-range?! buf start min-count desired-count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! min-count)
  (nonnegative-integer?! desired-count)
  (let ((len (if (mbytevector? buf) (mbytevector-length buf) (bytevector-length buf))))
    (unless (<= (+ start min-count) (+ start desired-count) len)
      (error "buffer range out of bounds" start min-count desired-count len))))

;;;;;;;;;;;;;;;;;;;;;
;;; Input streams ;;;
;;;;;;;;;;;;;;;;;;;;;

;;; All input streams

;; Returns one of the following:
;; - #f
;; - eof
;; - closed
;; - (error . ,description)
(define (istream-status    s)                           (s 'status))
(define (istream-close     s)                           (s 'close))
;; These both return (values) on EOF or amount read.  Check status if less is read than expected.
(define (istream-read-byte s)                           (s 'read-byte))
(define (istream-read      s dst start min-count count) (s 'read dst start min-count count))
;; returns #f if stream does not have a position
(define (istream-position  s)                           (s 'position))

;;; Input streams with a position

;; When pos is #f, set position to EOF
(define (istream-set-position! s pos)                 (s 'set-position! pos))
;; Returns (values) on EOF or amount read.  Check status if less is read than expected.
(define (istream-pread         s pos dst start count) (s 'pread pos dst start count))

;;;;;;;;;;;;;;;;;;;;;;
;;; Output streams ;;;
;;;;;;;;;;;;;;;;;;;;;;

;;; All output streams

;; Returns one of the following:
;; - #f
;; - full
;; - closed
;; - (error . ,description)
(define (ostream-status     s)                           (s 'status))
(define (ostream-close      s)                           (s 'close))
;; These both return the amount written.  Check status if less is written than expected.
(define (ostream-write-byte s byte)                      (s 'write-byte byte))
(define (ostream-write      s src start min-count count) (s 'write src start min-count count))
;; Returns #f if stream does not have a position
(define (ostream-position   s)                           (s 'position))

;;; Output streams with a position

;; When pos is #f, set position to EOF
(define (ostream-set-position! s pos)                 (s 'set-position! pos))
(define (ostream-set-size!     s size)                (s 'set-size! size))
;; Returns the amount written.  Check status if less is written than expected.
(define (ostream-pwrite        s pos src start count) (s 'pwrite pos src start count))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector streams ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These bytevector stream definitions are not safe for concurrent access unless synchronization is
;;; provided by the user.

(define (open-mbytevector-istream src) (open-bytevector-istream src))
(define (open-bytevector-istream src)
  (let ((len (if (mbytevector? src) (mbytevector-length src) (bytevector-length src))))
    (mlet ((pos 0))
      (lambda (method . arg*)
        (apply
          (case method
            ((read)          (lambda (dst start min-count count)
                               (buffer-range?! dst start min-count count)
                               (if (< 0 count)
                                   (let* ((i     pos)
                                          (end   (min (+ i count) len))
                                          (count (- end i)))
                                     (if (< 0 count)
                                         (begin (set! pos end)
                                                (mbytevector-copy! src i dst start count)
                                                count)
                                         (values)))
                                   0)))
            ((pread)         (lambda (pos dst start count)
                               (buffer-range?! dst start count count)
                               (if (< 0 count)
                                   (let* ((i     pos)
                                          (end   (min (+ i count) len))
                                          (count (- end i)))
                                     (if (< 0 count)
                                         (begin (mbytevector-copy! src i dst start count) count)
                                         (values)))
                                   0)))
            ((read-byte)     (lambda ()
                               (let ((i pos))
                                 (if (< i len)
                                     (begin (set! pos (+ i 1))
                                            (if (mbytevector? src)
                                                (mbytevector-ref src i)
                                                (bytevector-ref src i)))
                                     (values)))))
            ((position)      (lambda ()    pos))
            ((set-position!) (lambda (new) (if new
                                               (begin (nonnegative-integer?! new)
                                                      (set! pos (min len new)))
                                               (set! pos len))))
            ((close)         (lambda ()    (values)))
            ((status)        (lambda ()    (and (= pos len) 'eof)))
            (else            (error "not a bytevector-istream method" method)))
          arg*)))))

(define (open-mbytevector-ostream buf)
  (mlet ((pos 0))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (src start min-count count)
                             (buffer-range?! src start min-count count)
                             (let* ((pos.current pos)
                                    (count (min (- (mbytevector-length buf) pos.current) count)))
                               (mbytevector-copy! src start buf pos.current count)
                               (set! pos (+ pos.current count))
                               count)))
          ((pwrite)        (lambda (pos src start count)
                             (buffer-range?! src start count count)
                             (let* ((pos.current pos)
                                    (count (min (- (mbytevector-length buf) pos.current) count)))
                               (mbytevector-copy! src start buf pos.current count)
                               count)))
          ((write-byte)    (lambda (byte)
                             (let ((len (mbytevector-length buf)) (i pos))
                               (if (< i len)
                                   (begin (mbytevector-set! buf i byte)
                                          (set! pos (+ i 1))
                                          1)
                                   0))))
          ((set-size!)     (lambda (new) (values)))
          ((position)      (lambda ()    pos))
          ((set-position!) (lambda (new) (if new
                                             (begin (nonnegative-integer?! new)
                                                    (set! pos (min (mbytevector-length buf) new)))
                                             (set! pos (mbytevector-length buf)))))
          ((close)         (lambda ()    (values)))
          ((status)        (lambda ()    (and (= pos (mbytevector-length buf)) 'full)))
          (else            (error "not an mbytevector-ostream method" method)))
        arg*))))

(define (open-bytevector-ostream)
  (mlet ((buf.st (make-mbytevector 64 0)) (pos.st 0) (end.st 0))
    (define (grow buf len end.copy end.min)
      (let ((new (make-mbytevector (max (+ len len) end.min) 0)))
        (mbytevector-copy! buf 0 new 0 end.copy)
        (set! buf.st new)
        new))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (src start min-count count)
                             (buffer-range?! src start min-count count)
                             (let* ((buf   buf.st)
                                    (pos.0 pos.st)
                                    (end   end.st)
                                    (len   (mbytevector-length buf))
                                    (pos   (+ pos.0 count))
                                    (buf   (if (< len pos) (grow buf len end pos) buf)))
                               (mbytevector-copy! src start buf pos.0 count)
                               (set! pos.st pos)
                               (when (< end pos) (set! end.st pos))
                               count)))
          ((pwrite)        (lambda (pos src start count)
                             (buffer-range?! src start count count)
                             (let* ((buf   buf.st)
                                    (pos.0 pos)
                                    (end   end.st)
                                    (len   (mbytevector-length buf))
                                    (pos   (+ pos.0 count))
                                    (buf   (if (< len pos) (grow buf len end pos) buf)))
                               (mbytevector-copy! src start buf pos.0 count)
                               (when (< end pos) (set! end.st pos))
                               count)))
          ((write-byte)    (lambda (byte)
                             (let* ((buf   buf.st)
                                    (pos.0 pos.st)
                                    (end   end.st)
                                    (len   (mbytevector-length buf))
                                    (pos   (+ pos.0 1))
                                    (buf   (if (< len pos) (grow buf len end pos) buf)))
                               (mbytevector-set! buf pos.0 byte)
                               (set! pos.st pos)
                               (when (< end pos) (set! end.st pos))
                               1)))
          ((set-size!)     (lambda (new)
                             (nonnegative-integer?! new)
                             (let* ((buf buf.st) (len (mbytevector-length buf)) (end end.st))
                               (cond
                                 ((<= new end) (when (< new pos.st) (set! pos.st new)))
                                 ((<= new len) (mbytevector-fill! buf 0 end (- new end)))
                                 (else         (grow buf len end new))))
                             (set! end.st new)))
          ((position)      (lambda ()    pos.st))
          ((set-position!) (lambda (new) (set! pos.st (if new
                                                          (begin (nonnegative-integer?! new)
                                                                 (min end.st new))
                                                          end.st))))
          ((close)         (lambda ()    (values)))
          ((status)        (lambda ()    #f))
          ((current)       (lambda ()    (mbytevector->bytevector buf.st 0 end.st)))
          (else            (error "not a bytevector-ostream method" method)))
        arg*))))

(define (bytevector-ostream-current s) (s 'current))

;;;;;;;;;;;;;;;;;;;;;
;;; Other streams ;;;
;;;;;;;;;;;;;;;;;;;;;

(define null-ostream
  (lambda (method . arg*)
    (apply (case method
             ((write)         (lambda (src start min-count count)
                                (buffer-range?! src start min-count count)
                                count))
             ((pwrite)        (lambda (pos src start count)
                                (buffer-range?! src start count count)
                                count))
             ((write-byte)    (lambda (b)   1))
             ((set-position!) (lambda (new) (values)))
             ((set-size!)     (lambda (new) (values)))
             ((position)      (lambda ()    0))
             ((close)         (lambda ()    (values)))
             ((status)        (lambda ()    #f))
             (else            (error "not a null-ostream method" method)))
           arg*)))

(define full-ostream
  (lambda (method . arg*)
    (apply (case method
             ((write)         (lambda (src start min-count count) 0))
             ((pwrite)        (lambda (pos src start count)       0))
             ((write-byte)    (lambda (b)   0))
             ((set-position!) (lambda (new) (values)))
             ((set-size!)     (lambda (new) (values)))
             ((position)      (lambda ()    0))
             ((close)         (lambda ()    (values)))
             ((status)        (lambda ()    'full))
             (else            (error "not a full-ostream method" method)))
           arg*)))

(define empty-istream
  (lambda (method . arg*)
    (apply
      (case method
        ((read)          (lambda (dst start min-count count)
                           (buffer-range?! dst start min-count count)
                           (if (< 0 count) (values) 0)))
        ((pread)         (lambda (pos dst start count)
                           (buffer-range?! dst start count count)
                           (if (< 0 count) (values) 0)))
        ((read-byte)     (lambda ()    (values)))
        ((set-position!) (lambda (new) (values)))
        ((position)      (lambda ()    0))
        ((close)         (lambda ()    (values)))
        ((status)        (lambda ()    'eof))
        (else            (error "not an empty-istream method" method)))
      arg*)))

(define (open-constant-istream byte)
  (lambda (method . arg*)
    (apply
      (case method
        ((read)          (lambda (dst start min-count count)
                           (buffer-range?! dst start min-count count)
                           (if (< 0 count)
                               (begin (mbytevector-fill! dst byte start count) count)
                               0)))
        ((pread)         (lambda (pos dst start count)
                           (buffer-range?! dst start count count)
                           (if (< 0 count)
                               (begin (mbytevector-fill! dst byte start count) count)
                               0)))
        ((read-byte)     (lambda ()    byte))
        ((set-position!) (lambda (new) (values)))
        ((position)      (lambda ()    0))
        ((close)         (lambda ()    (values)))
        ((status)        (lambda ()    #f))
        (else            (error "not a constant-istream method" method)))
      arg*)))
