(define (buffer-range?! buf start count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! count)
  (unless (<= (+ start count) (mbytevector-length buf))
    (error "buffer range out of bounds" start count (mbytevector-length buf))))

;;;;;;;;;;;;;
;;; Ports ;;;
;;;;;;;;;;;;;

;;; All ports
(define (port-close        p)         (p 'close))
(define (port-buffer?      p)         (p 'buffer?))
(define (port-set-buffer?! p buffer?) (p 'set-buffer?! buffer?))
;; returns #f if port has no position
(define (port-position     p)         (p 'position))

;;; All ports with a position
;; when pos is #f, set position to EOF
(define (port-set-position! p pos) (p 'set-position! pos))

;;; Input ports
(define (port-drop  p count)                      (p 'drop count))
;; returns (values) on EOF
(define (port-peek* p skip full? dst start count) (p 'peek* skip full? dst start count))
;; returns (values) on EOF
(define (port-read* p      full? dst start count) (p 'read* full? dst start count))

;;; Output ports
(define (port-flush  p)                       (p 'flush))
(define (port-write* p full? src start count) (p 'write* full? src start count))

;;; Output ports with a position
(define (port-set-size! p size) (p 'set-size! size))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bytevector ports ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (open-input-bytevector bv)
  (mlet ((pos 0))
    (lambda (method . arg*)
      (apply
        (case method
          ((peek*) (lambda (skip full? dst start count)
                     (buffer-range?! dst start count)
                     (if (< 0 count)
                         (let* ((i     (+ pos skip))
                                (end   (min (+ i count) (bytevector-length bv)))
                                (count (- end i)))
                           (if (< 0 count)
                               (begin (mbytevector-copy! bv i dst start count)
                                      count)
                               (values)))
                         0)))
          ((read*) (lambda (full? dst start count)
                     (buffer-range?! dst start count)
                     (if (< 0 count)
                         (let* ((i     pos)
                                (end   (min (+ i count) (bytevector-length bv)))
                                (count (- end i)))
                           (if (< 0 count)
                               (begin (mbytevector-copy! bv i dst start count)
                                      (set! pos end)
                                      count)
                               (values)))
                         0)))
          ((drop)          (lambda (count)
                             (nonnegative-integer?! count)
                             (set! pos (min (bytevector-length bv) (+ pos count)))))
          ((position)      (lambda ()    pos))
          ((set-position!) (lambda (new) (let ((size (bytevector-length bv)))
                                           (if new
                                               (begin (nonnegative-integer?! new)
                                                      (set! pos (min size new)))
                                               (set! pos size)))))
          ((buffer?)       (lambda ()    #t))
          ((set-buffer?!)  (lambda (b?)  (values)))
          ((close)         (lambda ()    (values)))
          (else            (error "not an input-bytevector method" method)))
        arg*))))

(define (open-output-bytevector)
  (mlet ((pos 0) (size 0) (buf (make-mbytevector 16 0)))
    (define (grow! size.min)
      (let* ((current buf) (len (mbytevector-length current)))
        (when (< len size.min)
          (let ((new (make-mbytevector (max (+ len len) size.min) 0)))
            (mbytevector-copy! current 0 new 0 size)
            (set! buf new)))))
    (lambda (method . arg*)
      (apply
        (case method
          ((write*)        (lambda (full? src start count)
                             (buffer-range?! src start count)
                             (let ((size.min (+ pos count)))
                               (when (< size size.min)
                                 (grow! size.min)
                                 (set! size size.min))
                               (mbytevector-copy! src start buf pos count))
                             count))
          ((flush)         (lambda ()    (values)))
          ((set-size!)     (lambda (new)
                             (grow! new)
                             (set! size new)
                             (set! pos (min pos new))))
          ((position)      (lambda ()    pos))
          ((set-position!) (lambda (new) (if new
                                             (begin (nonnegative-integer?! new)
                                                    (set! pos (min size new)))
                                             (set! pos size))))
          ((buffer?)       (lambda ()    #t))
          ((set-buffer?!)  (lambda (b?)  (values)))
          ((close)         (lambda ()    (values)))
          ((current)       (lambda ()    (let ((current (make-mbytevector size 0)))
                                           (mbytevector-copy! buf 0 current 0 size)
                                           (mbytevector->bytevector current))))
          (else            (error "not an output-bytevector method" method)))
        arg*))))

(define (output-bytevector-current p) (p 'current))

(define (call-with-input-bytevector bv k) (k (open-input-bytevector bv)))
(define (call-with-output-bytevector   k) (let ((out (open-output-bytevector)))
                                            (k out)
                                            (output-bytevector-current out)))

;;;;;;;;;;;;;;;;;;;
;;; Other ports ;;;
;;;;;;;;;;;;;;;;;;;

(define null-output-port
  (lambda (method . arg*)
    (apply (case method
             ((write*)        (lambda (full? src start count)
                                (buffer-range?! src start count)
                                count))
             ((flush)         (lambda ()    (values)))
             ((set-size!)     (lambda (new) (values)))
             ((position)      (lambda ()    0))
             ((set-position!) (lambda (new) (values)))
             ((buffer?)       (lambda ()    #t))
             ((set-buffer?!)  (lambda (b?)  (values)))
             ((close)         (lambda ()    (values)))
             (else            (error "not a null-output-port method" method)))
           arg*)))

(define empty-input-port
  (lambda (method . arg*)
    (apply
      (case method
        ((peek*)         (lambda (skip full? dst start count)
                           (buffer-range?! dst start count)
                           (if (< 0 count) (values) 0)))
        ((read*)         (lambda (full? dst start count)
                           (buffer-range?! dst start count)
                           (if (< 0 count) (values) 0)))
        ((drop)          (lambda (count) (values)))
        ((position)      (lambda ()      0))
        ((set-position!) (lambda (new)   (values)))
        ((buffer?)       (lambda ()      #t))
        ((set-buffer?!)  (lambda (b?)    (values)))
        ((close)         (lambda ()      (values)))
        (else            (error "not an empty-input-port method" method)))
      arg*)))

(define (open-constant-input-port byte)
  (lambda (method . arg*)
    (apply
      (case method
        ((peek*) (lambda (skip full? dst start count)
                   (buffer-range?! dst start count)
                   (if (< 0 count)
                       (begin (mbytevector-fill! dst byte start count) count)
                       0)))
        ((read*) (lambda (full? dst start count)
                   (buffer-range?! dst start count)
                   (if (< 0 count)
                       (begin (mbytevector-fill! dst byte start count) count)
                       0)))
        ((drop)          (lambda (count) (values)))
        ((position)      (lambda ()      0))
        ((set-position!) (lambda (new)   (values)))
        ((buffer?)       (lambda ()      #t))
        ((set-buffer?!)  (lambda (b?)    (values)))
        ((close)         (lambda ()      (values)))
        (else            (error "not a constant-input-port method" method)))
      arg*)))
