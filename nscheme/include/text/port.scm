;;; Improper use of a port operation will panic.
;;; Proper use of a port operation may still fail by raising an io-error.

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
                                      (begin (iport-set-eof?! p #t) (values))))))))
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
                            (mbytevector-ref buf 0))))))))

  ;; Returns (values) on EOF or amount read.
  (define iport-read
    (let ((go (lambda (p dst start min-count count)
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
                                    (() available)
                                    ((amount)
                                     (mbytevector-copy! buf 0 dst start (min count amount))
                                     (if (< count amount)
                                         (begin (iport-set-pos! p count)
                                                (iport-set-end! p amount)
                                                (+ available count))
                                         (+ available amount))))
                                  (case-values (istream-read s dst start min-count count)
                                    (()       (if (< 0 available) available (values)))
                                    ((amount) (+ available amount))))))))))))
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
    (istream-set-position! (iport-stream p) pos)
    (iport-set-pos! p 0)
    (iport-set-end! p 0))

  ;; returns (values) on EOF or amount read.
  (define (iport-pread p pos dst start count)
    (istream-pread (iport-stream p) pos dst start count)))

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
                  (let ((size (max size pos)))
                    (unless (= size (mbytevector-length buf))
                      (let ((new (make-mbytevector size 0)))
                        (mbytevector-copy! buf 0 new 0 pos)
                        (oport-set-buffer! p new))))))))
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
      (ostream-write (oport-stream p) (oport-buffer p) 0 pos pos)
      (oport-set-pos! p 0)))

  (define (oport-write-byte p byte)
    (let* ((buf       (oport-buffer p))
           (len       (mbytevector-length buf))
           (pos       (oport-pos p))
           (available (- len pos)))
      (cond
        ((= len 0)       (ostream-write-byte (oport-stream p) byte))
        ((= available 0) (let* ((amount (ostream-write (oport-stream p) buf 0 1 len))
                                (pos    (- len amount)))
                           (mbytevector-copy! buf amount buf 0 pos)
                           (mbytevector-set! buf pos byte)
                           (oport-set-pos! p (+ pos 1))))
        (else            (mbytevector-set! buf pos byte)
                         (oport-set-pos! p (+ pos 1))))))

  ;; Returns the "amount" written, where: (<= min-count amount count)
  (define oport-write
    (let ((go (lambda (p src start min-count count)
                (buffer-range?! src start min-count count)
                (let* ((buf       (oport-buffer p))
                       (len       (mbytevector-length buf))
                       (pos       (oport-pos p))
                       (available (- len pos)))
                  (define (drain min-count)
                    (let* ((amount (ostream-write (oport-stream p) buf 0 min-count len))
                           (pos    (- len amount)))
                      (mbytevector-copy! buf amount buf 0 pos)
                      pos))
                  (define (fill pos available start count)
                    (let ((amount (min count available)))
                      (mbytevector-copy! src start buf pos amount)
                      (oport-set-pos! p (if (= amount available) (drain 0) (+ pos amount)))
                      amount))
                  (cond
                    ((= len 0) (ostream-write (oport-stream p) src start min-count count))
                    ((<= min-count available) (if (< 0 count) (fill pos available start count) 0))
                    (else (mbytevector-copy! src start buf pos available)
                          (+ (let ((start     (+ start available))
                                   (count     (- count available))
                                   (min-count (- min-count available)))
                               (if (< min-count len)
                                   (let ((pos (drain min-count)))
                                     (fill pos (- len pos) start count))
                                   (begin (oport-flush p)
                                          (ostream-write (oport-stream p) src start min-count
                                                         count))))
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
    (ostream-set-position! (oport-stream p) pos)
    (oport-set-pos! p 0))

  (define (oport-set-size! p size)
    (oport-flush p)
    (ostream-set-size! (oport-stream p) size))

  (define (oport-pwrite p pos src start count)
    (oport-flush p)
    (ostream-pwrite (oport-stream p) pos src start count)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector ports ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (open-input-mbytevector  src) (istream->iport (open-mbytevector-istream src)))
(define (open-input-bytevector   src) (istream->iport (open-bytevector-istream  src)))
(define (open-output-mbytevector dst) (ostream->oport (open-mbytevector-ostream dst)))
(define (open-output-bytevector . x*) (ostream->oport (apply open-bytevector-ostream x*)))
(define (output-bytevector-current p) (oport-flush p) (bytevector-ostream-current (oport-stream p)))

(define (call-with-input-bytevector   bv  k) (k (open-input-bytevector bv)))
(define (call-with-output-mbytevector mbv k) (let ((p (open-output-mbytevector mbv)))
                                               (let-values ((x* (k p)))
                                                 (oport-close p)
                                                 (apply values x*))))
(define (call-with-output-bytevector      k) (let ((out (open-output-bytevector)))
                                               (k out)
                                               (let ((result (output-bytevector-current out)))
                                                 (oport-close out)
                                                 result)))

;;;;;;;;;;;;;;;;;;;
;;; Other ports ;;;
;;;;;;;;;;;;;;;;;;;

(define (open-null-output-port)         (ostream->oport null-ostream  0))
(define (open-full-output-port)         (ostream->oport full-ostream  0))
(define (open-empty-input-port)         (istream->iport empty-istream 1))
(define (open-constant-input-port byte) (istream->iport (open-constant-istream byte)))
