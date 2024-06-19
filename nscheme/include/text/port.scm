(define (buffer-range?! buf start count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! count)
  (let ((len (if (mbytevector? buf) (mbytevector-length buf) (bytevector-length buf))))
    (unless (<= (+ start count) len)
      (error "buffer range out of bounds" start count len))))

;;;;;;;;;;;;;
;;; Ports ;;;
;;;;;;;;;;;;;

;;; All ports
(define (port-close    p) (p 'close))
;; returns #f if port has no position
(define (port-position p) (p 'position))

;;; All ports with a position
;; when pos is #f, set position to EOF
(define (port-set-position! p pos) (p 'set-position! pos))

;;; Input ports
(define (port-drop p count)                      (p 'drop count))
;; wait?: #f | partial | full
;; returns (values) on EOF
(define (port-peek p skip wait? dst start count) (p 'peek skip wait? dst start count))
;; wait?: #f | partial | full
;; returns (values) on EOF
(define (port-read p      wait? dst start count) (p 'read wait? dst start count))

;;; Input ports with a position
(define (port-pread p pos dst start count) (p 'pread pos dst start count))

;;; Output ports
(define (port-flush p)                       (p 'flush))
;; wait?: #f | partial | full | full/flush
(define (port-write p wait? src start count) (p 'write wait? src start count))

;;; Output ports with a position
(define (port-pwrite    p pos src start count) (p 'pwrite pos src start count))
(define (port-set-size! p size)                (p 'set-size! size))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector ports ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (open-input-bytevector src)
  (let ((len (if (mbytevector? src) (mbytevector-length src) (bytevector-length src))))
    (mlet ((pos 0))
      (lambda (method . arg*)
        (apply
          (case method
            ((peek)  (lambda (skip wait? dst start count)
                       (buffer-range?! dst start count)
                       (if (< 0 count)
                           (let* ((i     (+ pos skip))
                                  (end   (min (+ i count) len))
                                  (count (- end i)))
                             (if (< 0 count)
                                 (begin (mbytevector-copy! src i dst start count)
                                        count)
                                 (values)))
                           0)))
            ((read)  (lambda (wait? dst start count)
                       (buffer-range?! dst start count)
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
            ((pread) (lambda (pos dst start count)
                       (buffer-range?! dst start count)
                       (if (< 0 count)
                           (let* ((i     pos)
                                  (end   (min (+ i count) len))
                                  (count (- end i)))
                             (if (< 0 count)
                                 (begin (mbytevector-copy! src i dst start count) count)
                                 (values)))
                           0)))
            ((drop)          (lambda (count)
                               (nonnegative-integer?! count)
                               (set! pos (min len (+ pos count)))))
            ((position)      (lambda ()    pos))
            ((set-position!) (lambda (new) (if new
                                               (begin (nonnegative-integer?! new)
                                                      (set! pos (min len new)))
                                               (set! pos len))))
            ((close)         (lambda ()    (values)))
            (else            (error "not an input-bytevector method" method)))
          arg*)))))

(define (open-output-mbytevector buf)
  (mlet ((pos 0))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (wait? src start count)
                             (buffer-range?! src start count)
                             (let* ((pos.current pos)
                                    (count (min (- (mbytevector-length buf) pos.current) count)))
                               (mbytevector-copy! src start buf pos.current count)
                               (set! pos (+ pos.current count))
                               count)))
          ((pwrite)        (lambda (pos src start count)
                             (buffer-range?! src start count)
                             (let* ((pos.current pos)
                                    (count (min (- (mbytevector-length buf) pos.current) count)))
                               (mbytevector-copy! src start buf pos.current count)
                               count)))
          ((flush)         (lambda ()    (values)))
          ((set-size!)     (lambda (new) (values)))
          ((position)      (lambda ()    pos))
          ((set-position!) (lambda (new) (if new
                                             (begin (nonnegative-integer?! new)
                                                    (set! pos (min (mbytevector-length buf) new)))
                                             (set! pos (mbytevector-length buf)))))
          ((close)         (lambda ()    (values)))
          (else            (error "not an output-mbytevector method" method)))
        arg*))))

(define (open-output-bytevector)
  (define (make-state pos size buf) (vector pos size buf))
  (define (state-pos  st)           (vector-ref st 0))
  (define (state-size st)           (vector-ref st 1))
  (define (state-buf  st)           (vector-ref st 2))
  (mlet ((state (make-state 0 0 (make-mbytevector 16 0))))
    (define (state-grow st size.min)
      (let* ((current (state-buf st)) (len (mbytevector-length current)))
        (if (< len size.min)
            (let ((new (make-mbytevector (max (+ len len) size.min) 0)))
              (mbytevector-copy! current 0 new 0 (state-size st))
              (make-state (state-pos st) (max (state-size st) size.min) new))
            (let ((size (state-size st)))
              (if (< size size.min)
                  (make-state (state-pos st) size.min current)
                  st)))))
    (lambda (method . arg*)
      (apply
        (case method
          ((write)         (lambda (wait? src start count)
                             (buffer-range?! src start count)
                             (let* ((st  state)
                                    (pos (state-pos st))
                                    (end (+ pos count))
                                    (st  (state-grow st end))
                                    (buf (state-buf st)))
                               (mbytevector-copy! src start buf pos count)
                               (set! state (make-state end (state-size st) buf)))
                             count))
          ((pwrite)        (lambda (pos src start count)
                             (buffer-range?! src start count)
                             (let* ((st     state)
                                    (pos    (state-pos st))
                                    (st.new (state-grow st (+ pos count)))
                                    (buf    (state-buf st.new)))
                               (mbytevector-copy! src start buf pos count)
                               (unless (eq? st st.new) (set! state st)))
                             count))
          ((flush)         (lambda () (values)))
          ((set-size!)     (lambda (new)
                             (let* ((st state) (st.new (state-grow st new)))
                               (if (eq? st st.new)
                                   (when (< new (state-pos st))
                                     (set! state (make-state new (state-size st) (state-buf st))))
                                   (set! state st.new)))))
          ((position)      (lambda () (state-pos state)))
          ((set-position!) (lambda (new)
                             (let* ((st state) (size (state-size st)))
                               (let ((new (if new
                                              (begin (nonnegative-integer?! new)
                                                     (min size new))
                                              size)))
                                 (unless (= (state-pos st) new)
                                   (set! state (make-state new size (state-buf st))))))))
          ((close)         (lambda () (values)))
          ((current)       (lambda () (let ((st state)) (mbytevector->bytevector (state-buf st) 0
                                                                                 (state-size st)))))
          (else            (error "not an output-bytevector method" method)))
        arg*))))

(define (output-bytevector-current p) (p 'current))

(define (call-with-input-bytevector   bv  k) (k (open-input-bytevector bv)))
(define (call-with-output-mbytevector mbv k) (k (open-output-mbytevector mbv)))
(define (call-with-output-bytevector      k) (let ((out (open-output-bytevector)))
                                               (k out)
                                               (output-bytevector-current out)))

;;;;;;;;;;;;;;;;;;;
;;; Other ports ;;;
;;;;;;;;;;;;;;;;;;;

(define null-output-port
  (lambda (method . arg*)
    (apply (case method
             ((write)         (lambda (wait? src start count)
                                (buffer-range?! src start count)
                                count))
             ((pwrite)        (lambda (pos src start count)
                                (buffer-range?! src start count)
                                count))
             ((flush)         (lambda ()    (values)))
             ((set-size!)     (lambda (new) (values)))
             ((position)      (lambda ()    0))
             ((set-position!) (lambda (new) (values)))
             ((close)         (lambda ()    (values)))
             (else            (error "not a null-output-port method" method)))
           arg*)))

(define empty-input-port
  (lambda (method . arg*)
    (apply
      (case method
        ((peek)          (lambda (skip wait? dst start count)
                           (buffer-range?! dst start count)
                           (if (< 0 count) (values) 0)))
        ((read)          (lambda (wait? dst start count)
                           (buffer-range?! dst start count)
                           (if (< 0 count) (values) 0)))
        ((pread)         (lambda (pos dst start count)
                           (buffer-range?! dst start count)
                           (if (< 0 count) (values) 0)))
        ((drop)          (lambda (count) (values)))
        ((position)      (lambda ()      0))
        ((set-position!) (lambda (new)   (values)))
        ((close)         (lambda ()      (values)))
        (else            (error "not an empty-input-port method" method)))
      arg*)))

(define (open-constant-input-port byte)
  (lambda (method . arg*)
    (apply
      (case method
        ((peek)  (lambda (skip wait? dst start count)
                   (buffer-range?! dst start count)
                   (if (< 0 count)
                       (begin (mbytevector-fill! dst byte start count) count)
                       0)))
        ((read)  (lambda (wait? dst start count)
                   (buffer-range?! dst start count)
                   (if (< 0 count)
                       (begin (mbytevector-fill! dst byte start count) count)
                       0)))
        ((pread) (lambda (pos dst start count)
                   (buffer-range?! dst start count)
                   (if (< 0 count)
                       (begin (mbytevector-fill! dst byte start count) count)
                       0)))
        ((drop)          (lambda (count) (values)))
        ((position)      (lambda ()      0))
        ((set-position!) (lambda (new)   (values)))
        ((close)         (lambda ()      (values)))
        (else            (error "not a constant-input-port method" method)))
      arg*)))
