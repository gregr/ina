#lang racket/base
(provide make-mvector mvector? mvector->vector
         mvector-length mvector-ref mvector-set! mvector-cas!
         string->vector vector->string cons*
         bitwise-arithmetic-shift << >> & \| ^
         filesystem console tcp stdio
         string:port:input string:port:output null:port:output
         call-with-input-string call-with-output-string
         method-lambda method-choose method-unknown method-except method-only
         racket-eval)

(require racket/file racket/tcp racket/string racket/struct racket/vector)

(struct mvector (v)
        #:methods gen:custom-write
        ((define write-proc
           (make-constructor-style-printer
             (lambda (mv) 'mvector)
             (lambda (mv) (vector->list (mvector-v mv)))))))

(define (make-mvector k x)          (mvector (make-vector k x)))
(define (mvector->vector mv)        (vector-copy   (mvector-v mv)))
(define (mvector-length mv)         (vector-length (mvector-v mv)))
(define (mvector-ref mv i)          (vector-ref    (mvector-v mv) i))
(define (mvector-set! mv i x)       (vector-set!   (mvector-v mv) i x))
(define (mvector-cas! mv i old new) (vector-cas!   (mvector-v mv) i old new))

(define (string->vector s)
  (list->vector (bytes->list (string->bytes/utf-8 s))))
(define (vector->string v)
  (bytes->string/utf-8 (list->bytes (vector->list v))))
(define cons* list*)

(define (bitwise-arithmetic-shift a b) (arithmetic-shift a b))
(define (<< i s) (bitwise-arithmetic-shift i s))
(define (>> i s) (bitwise-arithmetic-shift i (- s)))
(define (& a b)  (bitwise-and a b))
(define (\| a b) (bitwise-ior a b))
(define (^ a b)  (bitwise-xor a b))

(define-syntax assert
  (syntax-rules ()
    ((_ condition ...)
     (begin (unless condition
              (error "assertion failed:" 'condition '(condition ...))) ...))))

(define (lambda/handle-fail handle inner)
  (define (fail x datum) (handle (vector 'fail datum (exn-message x))))
  (lambda args
    (with-handlers*
      ((exn:fail:filesystem:errno?  (lambda (x) (fail x `(filesystem ,(exn:fail:filesystem:errno-errno x)))))
       (exn:fail:filesystem:exists? (lambda (x) (fail x '(filesystem exists))))
       (exn:fail:filesystem?        (lambda (x) (fail x '(filesystem #f))))
       (exn:fail:network:errno?     (lambda (x) (fail x `(network ,(exn:fail:network:errno-errno x)))))
       (exn:fail:network?           (lambda (x) (fail x '(network #f))))
       (exn:fail?                   (lambda (x) (fail x '(#f #f)))))
      (apply inner args))))

(define (method-unknown name . args) (error "unknown method:" name args))
(define (method-except m names)
  (lambda (name . args)
    (apply (if (member name names) method-unknown m) name args)))
(define (method-only m names)
  (lambda (name . args)
    (apply (if (member name names) m method-unknown) name args)))

(define-syntax method-choose
  (syntax-rules (else)
    ((_ ((name ...) body ...) ... (else else-body ...))
     (lambda (method-name . args)
       (apply (case method-name
                ((name ...) body ...) ...
                (else       else-body ...))
              method-name args)))
    ((_ body ...) (method-choose body ... (else method-unknown)))))

(define-syntax method-lambda
  (syntax-rules (else)
    ((_ ((name . param) body ...) ... (else else-body ...))
     (method-choose ((name) (lambda (_ . param) body ...)) ... (else else-body ...)))
    ((_ body ...) (method-lambda body ... (else method-unknown)))))

(define (mvector-copy!/ref mv start src src-start src-end ref)
  (let loop ((i src-start))
    (cond ((<= src-end i) (- i src-start))
          (else (mvector-set! mv (+ start (- i src-start)) (ref src i))
                (loop (+ i 1))))))
(define (mvector-copy! mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end mvector-ref))
(define (mvector-copy!/bytes mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end bytes-ref))
(define (mvector-copy!/vector mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end vector-ref))
(define (mvector-copy!/string mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end
                     (lambda (s i) (char->integer (string-ref s i)))))

(define (call-with-input-string s k) (k (string:port:input s)))
;; TODO: consider wrapping out to limit capability given to k
(define (call-with-output-string  k) (let ((out (string:port:output)))
                                       (k out)
                                       (out 'string)))

;; TODO: model file descriptors and their operations directly?

(define (bytestream:port port)
  (method-lambda
    ((buffer-mode-ref)       (file-stream-buffer-mode port))
    ((buffer-mode-set! mode) (file-stream-buffer-mode port mode))))

(define (bytestream:port:input super port)
  (define (eof->false d) (if (eof-object? d) #f d))
  (method-lambda
    ((close)     (close-input-port port))
    ((get)       (eof->false (read-byte port)))
    ((peek skip) (eof->false (peek-byte port skip)))
    ((get*! mv start len)
     (define bs (eof->false (read-bytes len port)))
     (if bs (mvector-copy!/bytes mv start bs 0 (bytes-length bs)) 0))
    ((peek*! mv start skip until)
     (define bs (eof->false (peek-bytes (- until skip) skip port)))
     (if bs (mvector-copy!/bytes mv start bs 0 (bytes-length bs)) 0))
    ((forget amount) (define bs (eof->false (read-bytes amount port)))
                     (if bs (bytes-length bs) 0))
    (else        super)))

(define (bytestream:port:output super port)
  (method-lambda
    ((close)  (close-output-port port))
    ((put b)  (write-byte b port))
    ((put* s) (write-string s port))
    ((flush)  (flush-output port))
    (else     super)))

(define (file:port port)
  (define super (bytestream:port port))
  (method-lambda
    ((position-ref)        (file-position* port))
    ((position-set! index) (file-position* port index))
    (else                  super)))

(define (file:port:input  port) (bytestream:port:input (file:port port) port))
(define (file:port:output port)
  (define super (file:port port))
  (bytestream:port:output
    (method-lambda
      ((truncate size) (file-truncate port size))
      (else            super))
    port))

(define (filesystem root)
  ;; NOTE: this is not a secure abstraction of a subtree of the file system.
  ;; There is no protection against using ".." to access parent directories.
  (define (path/root path)
    (append root (if (string? path)
                   ;; #:trim? works around weird string-split default behavior
                   (string-split path "/" #:trim? #f)
                   path)))
  (define (resolve path) (string-join (path/root path) "/"))
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((subsystem path) (filesystem (path/root path)))

      ((open-input        path                ) (file:port:input  (open-input-file  (resolve path))))
      ((open-output       path (exists 'error)) (file:port:output (open-output-file (resolve path) #:exists exists)))
      ((open-input-output path (exists 'error)) (define-values (in out)
                                                  (open-input-output-file (resolve path) #:exists exists))
                                                (list (file:port:input in) (file:port:output in)))

      ((directory              path) (map path->string         (directory-list (resolve path))))
      ((file-exists?           path) (file-exists?                             (resolve path)))
      ((directory-exists?      path) (directory-exists?                        (resolve path)))
      ((link-exists?           path) (link-exists?                             (resolve path)))
      ((make-directory         path) (make-directory                           (resolve path)))
      ((make-directory*        path) (make-directory*                          (resolve path)))
      ((make-parent-directory* path) (make-parent-directory*                   (resolve path)))
      ((make-link           to path) (make-file-or-directory-link (resolve to) (resolve path)))
      ((delete-file            path) (delete-file                              (resolve path)))
      ((delete-directory       path) (delete-directory                         (resolve path)))
      ((delete-directory/files path) (delete-directory/files                   (resolve path)))

      ((rename old new (exists-ok? #f)) (rename-file-or-directory (resolve old) (resolve new) exists-ok?))
      ((copy   old new (exists-ok? #f)) (copy-file                (resolve old) (resolve new) exists-ok?))
      ((copy-directory/files src dest (keep-modify-time? #f) (keep-links? #f))
       (copy-directory/files (resolve src) (resolve dest)
                             #:keep-modify-seconds? keep-modify-time?
                             #:preserve-links? keep-links?))

      ((size             path)         (file-size                        (resolve path)))
      ((permissions-ref  path)         (file-or-directory-permissions    (resolve path) 'bits))
      ((permissions-set! path mode)    (assert (integer? mode))
                                       (file-or-directory-permissions    (resolve path) mode))
      ((modify-time-ref  path)         (file-or-directory-modify-seconds (resolve path)))
      ((modify-time-set! path seconds) (file-or-directory-modify-seconds (resolve path) seconds))

      ((wait path) (define evt (filesystem-change-evt (resolve path)))
                   (sync evt)
                   (filesystem-change-evt-cancel evt)))))

(define (console in out err)
  (lambda/handle-fail (lambda (x) x)
                      (method-lambda ((in) in) ((out) out) ((error) err))))

(define (tcp:port in out)
  (define (abandoner port)
    (define super (bytestream:port port))
    (lambda/handle-fail
      (lambda (x) x)
      (method-lambda
        ((abandon) (tcp-abandon-port port))
        (else super))))
  (define a.in  (abandoner in))
  (define a.out (abandoner out))
  (define super
    (lambda/handle-fail
      (lambda (x) x)
      (method-lambda
        ((addresses)  (define-values
                        (local-host local-port remote-host remote-port)
                        (tcp-addresses in #t))
                      (list local-host local-port remote-host remote-port))
        ((in  . args) (if (null? args) (bytestream:port:input  a.in  in)
                        (apply a.in  args)))
        ((out . args) (if (null? args) (bytestream:port:output a.out out)
                        (apply a.out args))))))
  (bytestream:port:output (bytestream:port:input super in) out))

(define (tcp:listener listen)
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((close)  (tcp-close listen))
      ((ready?) (tcp-accept-ready? listen))
      ((accept) (define-values (in out) (tcp-accept listen))
                (tcp:port in out)))))

(define tcp
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((listen hostname port (max-wait 4) (reuse? #f))
       (tcp:listener (tcp-listen port max-wait reuse? hostname)))
      ((connect host port (localhost #f) (localport #f))
       (define-values (in out) (tcp-connect host port localhost localport))
       (tcp:port in out)))))

;; TODO: what does a pre-connected udp port object look like?
;; TODO: what about a not-connected udp port?: udp-bind! udp-connect! udp-send-to
;(define (udp:port port) ...)  ;; connected udp ports: udp-send, udp-receive

(define (stdio:port:input  port) (bytestream:port:input  (bytestream:port port) port))
(define (stdio:port:output port) (bytestream:port:output (bytestream:port port) port))

(define stdio (console (stdio:port:input  (current-input-port))
                       (stdio:port:output (current-output-port))
                       (stdio:port:output (current-error-port))))

(define (string:port:input s)
  (define v (string->vector s))
  (define i 0)
  (define (ref i) (and (< i (vector-length v)) (vector-ref v i)))
  (define (ref* mv start i skip until)
    (mvector-copy!/vector mv start v (+ i skip)
                          (min (vector-length v) (+ i until))))
  (method-lambda
    ((get)                        (define b (ref i))
                                  (when b (set! i (+ i 1)))
                                  b)
    ((peek skip)                  (ref (+ i skip)))
    ((get*! mv start len)         (define amount (ref* mv start i 0 len))
                                  (set! i (+ i amount))
                                  amount)
    ((peek*! mv start skip until) (ref* mv start i skip until))
    ((forget amount) (define next-i (min (vector-length v) (+ i amount)))
                     (define actual (- next-i i))
                     (set! i next-i) actual)
    ((position-ref)        i)
    ((position-set! index) (set! i (min (max index 0) (vector-length v))))))

(define (string:port:output)
  (define buffer (make-mvector 32 0))
  (define i 0)
  (define (grow mult)
    (define current buffer)
    (set! buffer (make-mvector (* mult (mvector-length buffer)) 0))
    (mvector-copy! buffer 0 current 0 (mvector-length current)))
  (method-lambda
    ((string) (define out (make-mvector i 0))
              (mvector-copy! out 0 buffer 0 i)
              (vector->string (mvector->vector out)))
    ((put b)  (when (= i (mvector-length buffer)) (grow 2))
              (mvector-set! buffer i b)
              (set! i (+ i 1)))
    ((put* s) (define u (- (string-length s) (- (mvector-length buffer) i)))
              (when (< 0 u) (grow (+ (quotient u (mvector-length buffer)) 2)))
              (mvector-copy!/string buffer i s 0 (string-length s))
              (set! i (+ i (string-length s))))
    ((close)               #t)
    ((flush)               #t)
    ((position-ref)        i)
    ((position-set! index) (set! i (min (max index 0) i)))
    ((truncate)            (set! i 0) (set! buffer (make-mvector 32 0)))))

(define null:port:output
  (method-lambda
    ((put  _) #t)
    ((put* _) #t)
    ((close)  #t)
    ((flush)  #t)))

;; TODO: synchronous channels, generators
;; NOTE: these are not ports; ports are restricted to transferring bytes
;; TODO: channels that get from sequences or put to mvectors
;; TODO: generators that iterate over sequences

(define (racket-eval rkt-datum)
  (define (racket-datum form)
    (define (@ i) (vector-ref form i))
    (cond ((pair? form)         (cons (racket-datum (car form))
                                      (racket-datum (cdr form))))
          ((not (vector? form)) form)
          ((eq? (@ 0) 'quote)   (@ 1))
          ((eq? (@ 0) 'vector)  (vector-map racket-datum (@ 1)))
          ((eq? (@ 0) 'keyword) (string->keyword (@ 1)))
          (else                 (error "invalid racket-datum:" form))))
  (parameterize ((current-namespace (make-base-namespace)))
    (eval (racket-datum rkt-datum))))
