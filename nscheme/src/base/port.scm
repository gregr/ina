(define (buffer-range?! buf start count)
  (nonnegative-integer?! start)
  (nonnegative-integer?! count)
  (let ((len (if (mbytes? buf) (mbytes-length buf) (bytes-length buf))))
    (unless (<= (+ start count) len)
      (mistake "buffer range out of bounds" 'start start 'count count 'buffer-size len))))

(define (iomemory-describe iom) (iom 'describe))
(define (port? x) (procedure? x))
(define (port-describe p) (p 'describe))

;;;;;;;;;;;;;;;;;;;;
;;; Input memory ;;;
;;;;;;;;;;;;;;;;;;;;
(define (imemory-close/k im kf k) (im 'close kf k))
(define (imemory-close   im)      (imemory-close/k im raise-io-error values))
(define (imemory-size/k  im kf k) (im 'size kf k))
(define (imemory-size    im)      (imemory-size/k im raise-io-error values))
;; Returns EOF, the amount read, or a failure indication.
;; Blocks until at least (min count (max 1 available-bytes)) bytes are read.
;; Failure may occur after a partial read.
(define (imemory-read/k im pos dst start count kf keof k) (im 'read pos dst start count kf keof k))
(define (imemory-read im pos dst start count)
  (imemory-read/k im pos dst start count raise-io-error values values))
;; Returns EOF, the byte read, or a failure indication.
(define (imemory-read-byte/k im pos kf keof k)
  (let ((dst (make-mbytes 1 0)))
    (imemory-read/k im pos dst 0 1 kf keof (lambda (amount) (k (mbytes-ref dst 0))))))
(define (imemory-read-byte im pos) (imemory-read-byte/k im pos raise-io-error values values))
;; Returns EOF, the bytes read, or a failure indication.
;; Blocks until at least (min count (max 1 available-bytes)) bytes are read.
;; Failure may occur after a partial read.
(define (imemory-read-bytes/k im pos count kf keof k)
  (let ((dst (make-mbytes count 0)))
    (imemory-read/k im pos dst 0 count kf keof (lambda (amount) (k (mbytes->bytes dst 0 amount))))))
(define (imemory-read-bytes im pos count)
  (imemory-read-bytes/k im pos count raise-io-error values values))
;; Returns the amount read, or a failure indication.
;; Blocks until at least (min count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (imemory-read*/k im pos dst start count kf k)
  (imemory-read/k im pos dst start count kf (lambda () (k 0))
                  (lambda (amount)
                    (let loop ((total amount))
                      (if (< total count)
                          (imemory-read/k im (+ pos total) dst (+ start total) (- count total) kf
                                          (lambda ()       (k total))
                                          (lambda (amount) (loop (+ total amount))))
                          (k total))))))
(define (imemory-read* im pos dst start count)
  (imemory-read*/k im pos dst start count raise-io-error values))
;; Returns the amount read, or a failure indication.
;; Blocks until at least (min count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (imemory-read*-bytes/k im pos count kf k)
  (let ((dst (make-mbytes count 0)))
    (imemory-read*/k im pos dst 0 count kf (lambda () (k #""))
                     (lambda (amount) (k (mbytes->bytes dst 0 amount))))))
(define (imemory-read*-bytes im pos count)
  (imemory-read*-bytes/k im pos count raise-io-error values))

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
;; May return a failure indication.
;; Blocks until the byte is written.
(define (omemory-write-byte/k om pos byte kf k)
  (omemory-write/k om pos (bytes byte) 0 1 1 kf k))
(define (omemory-write-byte om pos byte) (omemory-write-byte/k om pos byte raise-io-error values))
;; May return a failure indication.
;; Blocks until the entire bytes is written.
;; Failure may occur after a partial write.
(define (omemory-write-bytes/k om pos src kf k)
  (let ((count (if (mbytes? src) (mbytes-length src) (bytes-length src))))
    (omemory-write/k om pos src 0 count kf k)))
(define (omemory-write-bytes om pos src)
  (omemory-write-bytes/k om pos src raise-io-error values))

;;;;;;;;;;;;;;;;;;;
;;; Input ports ;;;
;;;;;;;;;;;;;;;;;;;
(define (iport-close/k p kf k) (p 'close kf k))
(define (iport-close   p)      (iport-close/k p raise-io-error values))
;; Returns EOF, the amount read, or a failure indication.
;; Blocks until at least (min count (max 1 available-bytes)) bytes are read.
;; Failure may occur after a partial read.
(define (iport-read/k p dst start count kf keof k) (p 'read dst start count kf keof k))
(define (iport-read p dst start count)
  (iport-read/k p dst start count raise-io-error values values))
;; Returns EOF, the byte read, or a failure indication.
(define (iport-read-byte/k p kf keof k)
  (let ((dst (make-mbytes 1 0)))
    (iport-read/k p dst 0 1 kf keof (lambda (amount) (k (mbytes-ref dst 0))))))
(define (iport-read-byte p) (iport-read-byte/k p raise-io-error values values))
;; Returns EOF, the bytes read, or a failure indication.
;; Blocks until at least (min count (max 1 available-bytes)) bytes are read.
;; Failure may occur after a partial read.
(define (iport-read-bytes/k p count kf keof k)
  (let ((dst (make-mbytes count 0)))
    (iport-read/k p dst 0 count kf keof (lambda (amount) (k (mbytes->bytes dst 0 amount))))))
(define (iport-read-bytes p count)
  (iport-read-bytes/k p count raise-io-error values values))
;; Returns the amount read, or a failure indication.
;; Blocks until at least (min count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (iport-read*/k p dst start count kf k)
  (iport-read/k p dst start count kf (lambda () (k 0))
                (lambda (amount)
                  (let loop ((total amount))
                    (if (< total count)
                        (iport-read/k p dst (+ start total) (- count total) kf
                                      (lambda ()       (k total))
                                      (lambda (amount) (loop (+ total amount))))
                        (k total))))))
(define (iport-read* p dst start count)
  (iport-read*/k p dst start count raise-io-error values))
;; Returns the amount read, or a failure indication.
;; Blocks until at least (min count remaining-bytes) bytes are read.
;; Failure may occur after a partial read.
(define (iport-read*-bytes/k p count kf k)
  (let ((dst (make-mbytes count 0)))
    (iport-read*/k p dst 0 count kf (lambda () (k #""))
                   (lambda (amount) (k (mbytes->bytes dst 0 amount))))))
(define (iport-read*-bytes p count)
  (iport-read*-bytes/k p count raise-io-error values))
;; Reverts the most recent read(s) of count bytes, provided by the mbytes src.
;; It is an error to unread different bytes from those that were originally read.
;; It is an error to unread more bytes than have been read since the last unread.
;; Each port implementation decides whether to enforce these constraints.
(define (iport-unread/k p src start count kf k) (p 'unread src start count kf k))
(define (iport-unread p src start count) (iport-unread/k p src start count raise-io-error values))

;;;;;;;;;;;;;;;;;;;;
;;; Output ports ;;;
;;;;;;;;;;;;;;;;;;;;
(define (oport-close/k p kf k) (p 'close kf k))
(define (oport-close   p)      (oport-close/k p raise-io-error values))
;; May return a failure indication.
;; Blocks until count bytes are written.
;; Failure may occur after a partial write.
(define (oport-write/k p src start count kf k) (p 'write src start count kf k))
(define (oport-write   p src start count) (oport-write/k p src start count raise-io-error values))
;; May return a failure indication.
;; Blocks until the byte is written.
(define (oport-write-byte/k p byte kf k) (oport-write/k p (bytes byte) 0 1 kf k))
(define (oport-write-byte   p byte)      (oport-write-byte/k p byte raise-io-error values))
;; May return a failure indication.
;; Blocks until the entire bytes is written.
;; Failure may occur after a partial write.
(define (oport-write-bytes/k p src kf k)
  (let ((count (if (mbytes? src) (mbytes-length src) (bytes-length src))))
    (oport-write/k p src 0 count kf k)))
(define (oport-write-bytes p src) (oport-write-bytes/k p src raise-io-error values))

;;;;;;;;;;;;;;;;;;;;
;;; Bytes memory ;;;
;;;;;;;;;;;;;;;;;;;;
;;; These bytes memory definitions are not safe for concurrent access unless synchronization is
;;; provided by the user.
(define (imemory:mbytes src) (imemory:bytes src))
(define (imemory:bytes src)
  (let ((len (if (mbytes? src) (mbytes-length src) (bytes-length src))))
    (lambda (method . arg*)
      (apply
        (case method
          ((read)     (lambda (pos dst start count kf keof k)
                        (nonnegative-integer?! pos)
                        (buffer-range?! dst start count)
                        (if (< 0 count)
                            (let* ((end (min (+ pos count) len)) (count (- end pos)))
                              (if (< 0 count)
                                  (begin (mbytes-copy! dst start src pos count) (k count))
                                  (keof)))
                            (k 0))))
          ((size)     (lambda (kf k) (k len)))
          ((close)    (lambda (kf k) (k)))
          ((describe) (lambda () '((type . imemory:bytes))))
          (else       (mistake 'imemory:bytes "not a method" method)))
        arg*))))
(define (omemory:mbytes buf)
  (mlet ((end.st (mbytes-length buf)))
    (define (full-error kf name . x*)
      (kf 'no-space (list (vector (vector 'omemory:mbytes (mbytes-length buf)) name x*))))
    (lambda (method . arg*)
      (apply (case method
               ((write)    (lambda (pos src start count kf k)
                             (nonnegative-integer?! pos)
                             (buffer-range?! src start count)
                             (let ((available (- (mbytes-length buf) pos)))
                               (if (< available count)
                                   (full-error kf 'write pos start count)
                                   (begin (mbytes-copy! buf pos src start count) (k))))))
               ((size)     (lambda (kf k) end.st))
               ((resize!)  (lambda (new kf k)
                             (nonnegative-integer?! new)
                             (let* ((len (mbytes-length buf)) (end end.st))
                               (if (< len new)
                                   (full-error kf 'resize! new)
                                   (begin (when (< end new) (mbytes-fill! buf 0 end (- new end)))
                                          (set! end.st new)
                                          (k))))))
               ((close)    (lambda (kf k) (k)))
               ((describe) (lambda () '((type . omemory:mbytes))))
               (else       (mistake 'omemory:mbytes "not a method" method)))
             arg*))))
(define (omemory:bytes&current) (omemory:bytes&current/buffer-size 64))
(define (omemory:bytes&current/buffer-size buffer-size)
  (positive-integer?! buffer-size)
  (mlet ((buf.st (make-mbytes buffer-size 0)) (end.st 0))
    (define (grow buf len end.copy end.min)
      (let ((new (make-mbytes (max (+ len len) end.min) 0)))
        (mbytes-copy! new 0 buf 0 end.copy)
        (set! buf.st new)
        new))
    (values
      (lambda (method . arg*)
        (apply (case method
                 ((write)    (lambda (pos src start count kf k)
                               (nonnegative-integer?! pos)
                               (buffer-range?! src start count)
                               (let* ((buf buf.st)
                                      (end end.st)
                                      (len (mbytes-length buf))
                                      (new (+ pos count))
                                      (buf (if (< len new) (grow buf len end new) buf)))
                                 (mbytes-copy! buf pos src start count)
                                 (when (< end new) (set! end.st new))
                                 (k))))
                 ((size)     (lambda (kf k) end.st))
                 ((resize!)  (lambda (new kf k)
                               (nonnegative-integer?! new)
                               (let* ((buf buf.st) (len (mbytes-length buf)) (end end.st))
                                 (cond ((< len new) (grow buf len end new))
                                       ((< end new) (mbytes-fill! buf 0 end (- new end)))))
                               (set! end.st new)
                               (k)))
                 ((close)    (lambda (kf k) (k)))
                 ((describe) (lambda () '((type . omemory:bytes))))
                 (else       (mistake 'omemory:bytes "not a method" method)))
               arg*))
      (lambda () (mbytes->bytes buf.st 0 end.st)))))

;;;;;;;;;;;;;;;;;;;;
;;; Other memory ;;;
;;;;;;;;;;;;;;;;;;;;
(define null-omemory
  (lambda (method . arg*)
    (apply (case method
             ((write)    (lambda (pos src start count kf k) (buffer-range?! src start count) (k)))
             ((size)     (lambda (kf k)                     (k)))
             ((resize!)  (lambda (new kf k)                 (k)))
             ((close)    (lambda (kf k)                     (k)))
             ((describe) (lambda ()                         '((type . null-omemory))))
             (else       (mistake 'null-omemory "not a method" method)))
           arg*)))
(define full-omemory
  (lambda (method . arg*)
    (define (full-error kf name . x*) (kf 'no-space (list (vector 'full-omemory name x*))))
    (apply (case method
             ((write)    (lambda (pos src start count kf k)
                           (buffer-range?! src start count)
                           (if (< 0 count) (full-error kf 'write pos start count) (k))))
             ((size)     (lambda (kf k) (k 0)))
             ((resize!)  (lambda (new kf k)
                           (nonnegative-integer?! new)
                           (if (< 0 new) (full-error kf 'resize! new) (k))))
             ((close)    (lambda (kf k) (k)))
             ((describe) (lambda () '((type . full-omemory))))
             (else       (mistake 'full-omemory "not a method" method)))
           arg*)))
(define empty-imemory
  (lambda (method . arg*)
    (apply (case method
             ((read)     (lambda (pos dst start count kf keof k)
                           (buffer-range?! dst start count)
                           (if (< 0 count) (keof) (k 0))))
             ((size)     (lambda (kf k) (k 0)))
             ((close)    (lambda (kf k) (k)))
             ((describe) (lambda ()     '((type . empty-imemory))))
             (else       (mistake 'empty-imemory "not a method" method)))
           arg*)))
(define (imemory:constant byte)
  (lambda (method . arg*)
    (apply (case method
             ((read)     (lambda (pos dst start count kf keof k)
                           (buffer-range?! dst start count)
                           (if (< 0 count)
                               (begin (mbytes-fill! dst byte start count) (k count))
                               (k 0))))
             ((close)    (lambda (kf k) (k)))
             ((describe) (lambda () '((type . constant-imemory))))
             (else       (mistake 'imemory:constant "not a method" method)))
           arg*)))

;;;;;;;;;;;;;;;;;;;;
;;; Memory ports ;;;
;;;;;;;;;;;;;;;;;;;;
(define (iport:memory im pos close?)
  (let ((description (cons '(type . iport:memory) (iomemory-describe im))))
    (mlet ((pos pos))
      (lambda (method . arg*)
        (apply (case method
                 ((read)     (lambda (dst start count kf keof k)
                               (let ((i pos))
                                 (imemory-read/k im pos dst start count kf keof
                                                 (lambda (n) (set! pos (+ i n)) (k n))))))
                 ((unread)   (lambda (src start count kf k)
                               (let ((i pos))
                                 (when (< i count)
                                   (mistake "too many bytes unread" count (cons 'position i)
                                            description))
                                 (set! pos (- i count))
                                 (k))))
                 ((close)    (lambda (kf k) (if close? (imemory-close/k im kf k) (k))))
                 ((describe) (lambda () description))
                 (else       (mistake 'iport:memory "not a method" method 'description description)))
               arg*)))))
(define (oport:memory om pos close?)
  (let ((description (cons '(type . oport:memory) (iomemory-describe om))))
    (mlet ((pos pos))
      (lambda (method . arg*)
        (apply (case method
                 ((write)    (lambda (src start count kf k)
                               (let ((i pos))
                                 (omemory-write/k om i src start count kf
                                                  (lambda () (set! pos (+ i count)) (k))))))
                 ((close)    (lambda (kf k) (if close? (omemory-close/k om kf k) (k))))
                 ((describe) (lambda () description))
                 (else       (mistake 'oport:memory "not a method" method 'description description)))
               arg*)))))

;;;;;;;;;;;;;;;;;;;
;;; Bytes ports ;;;
;;;;;;;;;;;;;;;;;;;
;;; These bytes port definitions are not safe for concurrent access unless synchronization is
;;; provided by the user.
(define (iport:mbytes src) (iport:memory (imemory:mbytes src) 0 #f))
(define (iport:bytes  src) (iport:memory (imemory:bytes  src) 0 #f))
(define (oport:mbytes buf) (oport:memory (omemory:mbytes buf) 0 #f))
(define (oport:bytes&current) (oport:bytes&current/buffer-size 64))
(define (oport:bytes&current/buffer-size buffer-size)
  (let-values (((om current) (omemory:bytes&current/buffer-size buffer-size)))
    (values (oport:memory om 0 #f) current)))
(define (call-with-oport:bytes k)
  (let-values (((out current) (oport:bytes&current))) (k out) (current)))
(define call/oport:bytes call-with-oport:bytes)
(define (call-with-batched-oport p k)
  (let*-values (((out current) (oport:bytes&current)) (x* (k out)))
    (let ((b* (current))) (oport-write p b* 0 (bytes-length b*)))
    (apply values x*)))
(define call/batched-oport call-with-batched-oport)

;;;;;;;;;;;;;;;;;;;
;;; Other ports ;;;
;;;;;;;;;;;;;;;;;;;
(define null-oport            (oport:memory null-omemory            0 #f))
(define full-oport            (oport:memory full-omemory            0 #f))
(define empty-iport           (iport:memory empty-imemory           0 #f))
(define (iport:constant byte) (iport:memory (imemory:constant byte) 0 #f))

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
                                    (lambda (_) (mistake "dead thread-safe-port" description)))))
       (lambda req (let ((ch.reply (make-channel)))
                     (sync (channel-put-evt ch.request (cons ch.reply req)) dead)
                     ((sync ch.reply dead)))))))
  (define (thread-safe-iport port)
    (let* ((description (cons '(type . thread-safe-iport) (port-describe port)))
           (request     (make-thread-safe-port-requester description port)))
      (lambda (method . arg*)
        (apply (case method
                 ((read)     (lambda (dst start count kf keof k)
                               (request 'read dst start count
                                        (lambda (t ctx)  (lambda () (kf t ctx)))
                                        (lambda ()       keof)
                                        (lambda (amount) (lambda () (k amount))))))
                 ((unread)   (lambda (src start count kf k)
                               (request 'unread src start count
                                        (lambda (t ctx) (lambda () (kf t ctx)))
                                        (lambda ()      k))))
                 ((close)    (lambda (kf k)
                               (request 'close
                                        (lambda (t ctx) (lambda () (kf t ctx)))
                                        (lambda ()      k))))
                 ((describe) (lambda () description))
                 (else       (mistake 'thread-safe-iport "not a method" method 'description description)))
               arg*))))
  (define (thread-safe-oport port)
    (let* ((description (cons '(type . thread-safe-oport) (port-describe port)))
           (request     (make-thread-safe-port-requester description port)))
      (lambda (method . arg*)
        (apply (case method
                 ((write)    (lambda (src start count kf k)
                               (request 'write src start count
                                        (lambda (t ctx)  (lambda () (kf t ctx)))
                                        (lambda ()       k))))
                 ((close)    (lambda (kf k)
                               (request 'close
                                        (lambda (t ctx) (lambda () (kf t ctx)))
                                        (lambda ()      k))))
                 ((describe) (lambda () description))
                 (else       (mistake 'thread-safe-oport "not a method" method 'description description)))
               arg*)))))

(define ((buffered-oport&flush/k/buffer-size buffer-size) port)
  (let ((description (cons '(type . buffered-oport) (port-describe port)))
        (buf         (make-mbytes buffer-size 0)))
    (mlet ((pos 0))
      (values
        (lambda (method . arg*)
          (apply (case method
                   ((write)    (lambda (src start count kf k)
                                 (buffer-range?! src start count)
                                 (let* ((len       (mbytes-length buf))
                                        (i         pos)
                                        (available (- len i))
                                        (amount    (min count available)))
                                   (mbytes-copy! buf i src start (min count available))
                                   (set! pos (+ i amount))
                                   (if (< count available)
                                       (k)
                                       (oport-write/k
                                         port buf 0 len kf
                                         (lambda ()
                                           (set! pos 0)
                                           (let ((start (+ start available))
                                                 (count (- count available)))
                                             (if (< count len)
                                                 (begin (mbytes-copy! buf 0 src start count)
                                                        (set! pos count)
                                                        (k))
                                                 (oport-write/k port src start count kf k)))))))))
                   ((close)    (lambda (kf k) (oport-write/k port buf 0 pos kf k)))
                   ((describe) (lambda () description))
                   (else       (mistake 'buffered-oport "not a method" method 'description description)))
                 arg*))
        (lambda (kf k) (oport-write/k port buf 0 pos kf (lambda () (set! pos 0) (k))))))))
(define ((buffered-oport&flush/buffer-size buffer-size) port)
  (let-values (((p flush/k) ((buffered-oport&flush/k/buffer-size buffer-size) port)))
    (values p (lambda () (flush/k raise-io-error values)))))
(define ((buffered-oport/buffer-size buffer-size) port)
  (let-values (((p flush/k) ((buffered-oport&flush/k/buffer-size buffer-size) port))) p))
(splicing-let ((typical-buffer-size 4096))
  (define buffered-oport&flush/k (buffered-oport&flush/k/buffer-size typical-buffer-size))
  (define buffered-oport&flush   (buffered-oport&flush/buffer-size   typical-buffer-size))
  (define buffered-oport         (buffered-oport/buffer-size         typical-buffer-size)))
(define (call-with-buffered-oport p k) (let ((out (buffered-oport p))) (let-values ((x* (k out)))
                                                                         (oport-close out)
                                                                         (apply values x*))))
(define call/buffered-oport call-with-buffered-oport)

(define ((iport->bytes/buffer-size buffer-size) port)
  (mlet ((buffer (make-mbytes buffer-size 0)))
    (let loop ((start 0))
      (let* ((len       (mbytes-length buffer))
             (available (- len start))
             (amount    (iport-read* port buffer start available)))
        (if (< amount available)
            (mbytes->bytes buffer 0 (+ start amount))
            (let ((new (make-mbytes (+ len len) 0)))
              (mbytes-copy! new 0 buffer 0 len)
              (set! buffer new)
              (loop len)))))))
(define iport->bytes (iport->bytes/buffer-size 4096))

(define ((iport-transfer-all/k/buffer-size buffer-size) in out kf k)
  (let ((buffer (make-mbytes buffer-size 0)))
    (let loop ()
      (iport-read/k
        in buffer 0 buffer-size kf k
        (lambda (amount) (oport-write/k out buffer 0 amount kf loop))))))
(define iport-transfer-all/k (iport-transfer-all/k/buffer-size 4096))
(define (iport-transfer-all in out) (iport-transfer-all/k in out raise-io-error values))
