(define (buffer-range?! buf start min-count desired-count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! min-count)
  (nonnegative-integer?! desired-count)
  (let ((len (if (mbytevector? buf) (mbytevector-length buf) (bytevector-length buf))))
    (unless (<= (+ start min-count) (+ start desired-count) len)
      (error "buffer range out of bounds" start min-count desired-count len))))

;;; Improper use of a stream operation will panic.
;;; Proper use of a stream operation may still fail by raising an io-error.

(define-values (io-error:kind io-error? io-error-operation)
  (make-exception-kind-etc error:kind 'io-error '#(operation)))
(define (make-io-error desc op) (make-exception io-error:kind (vector desc op)))
(define (raise-io-error . x*) (raise (apply make-io-error x*)))
(define (raise-full-ostream-io-error op) (raise-io-error "not enough space in ostream" op))

;;;;;;;;;;;;;;;;;;;;;
;;; Input streams ;;;
;;;;;;;;;;;;;;;;;;;;;

;;; All input streams

(define (istream-close     s)                           (s 'close))
;; These both return (values) on EOF or amount read.
(define (istream-read-byte s)                           (s 'read-byte))
(define (istream-read      s dst start min-count count) (s 'read dst start min-count count))
;; returns #f if stream does not have a position
(define (istream-position  s)                           (s 'position))

;;; Input streams with a position

;; When pos is #f, set position to EOF
(define (istream-set-position! s pos)                 (s 'set-position! pos))
;; Returns (values) on EOF or amount read.
(define (istream-pread         s pos dst start count) (s 'pread pos dst start count))

;;;;;;;;;;;;;;;;;;;;;;
;;; Output streams ;;;
;;;;;;;;;;;;;;;;;;;;;;

;;; All output streams

(define (ostream-close      s)                           (s 'close))
(define (ostream-write-byte s byte)                      (s 'write-byte byte))
;; Returns the "amount" written, where: (<= min-count amount count)
(define (ostream-write      s src start min-count count) (s 'write src start min-count count))
;; Returns #f if stream does not have a position
(define (ostream-position   s)                           (s 'position))

;;; Output streams with a position

;; When pos is #f, set position to EOF
(define (ostream-set-position! s pos)                 (s 'set-position! pos))
(define (ostream-set-size!     s size)                (s 'set-size! size))
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
                                         (begin (mbytevector-copy! src i dst start count)
                                                count)
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
            (else            (error "not a bytevector-istream method" method)))
          arg*)))))

(define (open-mbytevector-ostream buf)
  (mlet ((pos.st 0))
    (define (full-error pos.current . x*)
      (raise-full-ostream-io-error
        (list (list 'mbytevector-ostream pos.current (mbytevector-length buf)) x*)))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (src start min-count count)
                             (buffer-range?! src start min-count count)
                             (let* ((pos.current pos.st)
                                    (amount (min (- (mbytevector-length buf) pos.current) count)))
                               (mbytevector-copy! src start buf pos.current amount)
                               (set! pos.st (+ pos.current amount))
                               (when (< amount min-count)
                                 (full-error pos.current 'write start min-count count))
                               count)))
          ((pwrite)        (lambda (pos src start count)
                             (buffer-range?! src start count count)
                             (let* ((pos.current pos)
                                    (amount (min (- (mbytevector-length buf) pos.current) count)))
                               (mbytevector-copy! src start buf pos.current amount)
                               (when (< amount count)
                                 (full-error pos.st 'pwrite pos start count)))))
          ((write-byte)    (lambda (byte) (let ((i pos.st))
                                            (unless (< i (mbytevector-length buf))
                                              (full-error i 'write-write byte))
                                            (mbytevector-set! buf i byte)
                                            (set! pos.st (+ i 1)))))
          ((set-size!)     (lambda (new)
                             (nonnegative-integer?! new)
                             (let ((pos pos.st))
                               (if (< new pos)
                                   (set! pos.st new)
                                   (when (< (mbytevector-length buf) new)
                                     (full-error pos 'set-size! new))))))
          ((position)      (lambda ()    pos.st))
          ((set-position!) (lambda (new) (if new
                                             (begin
                                               (nonnegative-integer?! new)
                                               (set! pos.st (min (mbytevector-length buf) new)))
                                             (set! pos.st (mbytevector-length buf)))))
          ((close)         (lambda ()    (values)))
          (else            (error "not an mbytevector-ostream method" method)))
        arg*))))

(define open-bytevector-ostream
  (let ((go (lambda (buffer-size)
              (mlet ((buf.st (make-mbytevector buffer-size 0)) (pos.st 0) (end.st 0))
                (define (grow buf len end.copy end.min)
                  (let ((new (make-mbytevector (max (+ len len) end.min) 0)))
                    (mbytevector-copy! buf 0 new 0 end.copy)
                    (set! buf.st new)
                    new))
                (lambda (method . arg*)
                  (apply
                    (case method
                      ((write)      (lambda (src start min-count count)
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
                      ((pwrite)     (lambda (pos src start count)
                                      (buffer-range?! src start count count)
                                      (let* ((buf   buf.st)
                                             (pos.0 pos)
                                             (end   end.st)
                                             (len   (mbytevector-length buf))
                                             (pos   (+ pos.0 count))
                                             (buf   (if (< len pos) (grow buf len end pos) buf)))
                                        (mbytevector-copy! src start buf pos.0 count)
                                        (when (< end pos) (set! end.st pos)))))
                      ((write-byte) (lambda (byte)
                                      (let* ((buf   buf.st)
                                             (pos.0 pos.st)
                                             (end   end.st)
                                             (len   (mbytevector-length buf))
                                             (pos   (+ pos.0 1))
                                             (buf   (if (< len pos) (grow buf len end pos) buf)))
                                        (mbytevector-set! buf pos.0 byte)
                                        (set! pos.st pos)
                                        (when (< end pos) (set! end.st pos)))))
                      ((set-size!)  (lambda (new)
                                      (nonnegative-integer?! new)
                                      (let* ((buf buf.st)
                                             (len (mbytevector-length buf))
                                             (end end.st))
                                        (cond
                                          ((<= new end) (when (< new pos.st) (set! pos.st new)))
                                          ((<= new len) (mbytevector-fill! buf 0 end (- new end)))
                                          (else         (grow buf len end new))))
                                      (set! end.st new)))
                      ((position)      (lambda ()    pos.st))
                      ((set-position!) (lambda (new) (set! pos.st
                                                       (if new
                                                           (begin (nonnegative-integer?! new)
                                                                  (min end.st new))
                                                           end.st))))
                      ((close)         (lambda ()    (values)))
                      ((current)       (lambda ()    (mbytevector->bytevector buf.st 0 end.st)))
                      (else            (error "not a bytevector-ostream method" method)))
                    arg*))))))
    (case-lambda
      (()            (go 64))
      ((buffer-size) (positive-integer?! buffer-size) (go buffer-size)))))

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
             ((pwrite)        (lambda (pos src start count) (values)))
             ((write-byte)    (lambda (b)                   (values)))
             ((set-position!) (lambda (new)                 (values)))
             ((set-size!)     (lambda (new)                 (values)))
             ((position)      (lambda ()                    0))
             ((close)         (lambda ()                    (values)))
             (else            (error "not a null-ostream method" method)))
           arg*)))

(define full-ostream
  (lambda (method . arg*)
    (define (full-error . x*) (raise-full-ostream-io-error (list 'full-ostream x*)))
    (apply (case method
             ((write)         (lambda (src start min-count count)
                                (buffer-range?! src start min-count count)
                                (when (< 0 min-count) (full-error 'write start min-count count))
                                0))
             ((pwrite)        (lambda (pos src start count)
                                (buffer-range?! src start count count)
                                (when (< 0 count) (full-error 'pwrite pos start count))))
             ((write-byte)    (lambda (b)   (full-error 'write-byte b)))
             ((set-size!)     (lambda (new)
                                (nonnegative-integer?! new)
                                (when (< 0 new) (full-error 'set-size! new))))
             ((set-position!) (lambda (new) (values)))
             ((position)      (lambda ()    0))
             ((close)         (lambda ()    (values)))
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
        (else            (error "not a constant-istream method" method)))
      arg*)))
