#lang racket/base
(provide make-mvector mvector? mvector->vector
         mvector-length mvector-ref mvector-set! mvector-cas!
         string->vector vector->string
         filesystem console tcp stdio
         port:string:input
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

(define (port:bytestream port)
  (method-lambda
    ((buffer-mode-ref)       (file-stream-buffer-mode port))
    ((buffer-mode-set! mode) (file-stream-buffer-mode port mode))))

(define (port:bytestream:input super port)
  (define (eof->false d) (if (eof-object? d) #f d))
  (method-lambda
    ((close)     (close-input-port port))
    ((get)       (eof->false (read-byte port)))
    ((peek skip) (eof->false (peek-byte port skip)))
    (else        super)))

(define (port:bytestream:output super port)
  (method-lambda
    ((close) (close-output-port port))
    ((put b) (write-byte b port))
    ((flush) (flush-output port))
    (else    super)))

(define (port:file port)
  (define super (port:bytestream port))
  (method-lambda
    ((position-ref)        (file-position* port))
    ((position-set! index) (file-position* port index))
    (else                  super)))

(define (port:file:input  port) (port:bytestream:input (port:file port) port))
(define (port:file:output port)
  (define super (port:bytestream:output (port:file port) port))
  (method-lambda
    ((truncate size) (file-truncate port size))
    (else            super)))

(define (filesystem root)
  (define (path/root path)
    ;; NOTE: string-split has a weird default, requiring us to use #:trim?.
    (let loop ((path (if (string? path) (string-split path "/" #:trim? #f)
                       path)))
      (cond ((null? path)             root)
            ((equal? ".." (car path)) (loop (cdr path)))
            (else                     (append root path)))))
  (define (resolve path) (string-join (path/root path) "/"))
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((filesystem path) (filesystem (path/root path)))

      ((open-input path)
       (port:file:input (open-input-file (resolve path))))
      ((open-output path exists)
       (port:file:output (open-output-file (resolve path) #:exists exists)))
      ((open-input-output path exists)
       (define-values (in out)
         (open-input-output-file (resolve path) #:exists exists))
       (list (port:file:input in) (port:file:output in)))

      ((directory path) (map path->string (directory-list (resolve path))))

      ((file-exists?      path) (file-exists?      (resolve path)))
      ((directory-exists? path) (directory-exists? (resolve path)))
      ((link-exists?      path) (link-exists?      (resolve path)))

      ((make-directory         path) (make-directory         (resolve path)))
      ((make-directory*        path) (make-directory*        (resolve path)))
      ((make-parent-directory* path) (make-parent-directory* (resolve path)))
      ((make-link to path) (make-file-or-directory-link (resolve to)
                                                        (resolve path)))

      ((delete-file            path) (delete-file            (resolve path)))
      ((delete-directory       path) (delete-directory       (resolve path)))
      ((delete-directory/files path) (delete-directory/files (resolve path)))
      ((rename old new exists-ok?) (rename-file-or-directory
                                     (resolve old) (resolve new) exists-ok?))
      ((copy   old new exists-ok?) (copy-file
                                     (resolve old) (resolve new) exists-ok?))
      ((copy-directory/files src dest keep-modify-time? keep-links?)
       (copy-directory/files (resolve src) (resolve dest)
                             #:keep-modify-seconds? keep-modify-time?
                             #:preserve-links? keep-links?))

      ((size             path)      (file-size (resolve path)))
      ((permissions-ref  path)      (file-or-directory-permissions
                                      (resolve path) 'bits))
      ((permissions-set! path mode) (assert (integer? mode))
                                    (file-or-directory-permissions
                                      (resolve path) mode))
      ((modify-time-ref  path)
       (file-or-directory-modify-seconds (resolve path)))
      ((modify-time-set! path seconds)
       (file-or-directory-modify-seconds (resolve path) seconds))

      ((wait path)
       (define evt (filesystem-change-evt (resolve path)))
       (sync evt)
       (filesystem-change-evt-cancel evt)))))

(define (console in out err)
  (lambda/handle-fail (lambda (x) x)
                      (method-lambda ((in) in) ((out) out) ((error) err))))

(define (port:tcp port)
  (define super (port:bytestream port))
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((addresses) (define-values
                     (local-host local-port remote-host remote-port)
                     (tcp-addresses port #t))
                   (list local-host local-port remote-host remote-port))
      ((abandon)   (tcp-abandon-port port))
      (else        super))))

(define (port:tcp:input  port) (port:bytestream:input  (port:tcp port) port))
(define (port:tcp:output port) (port:bytestream:output (port:tcp port) port))

(define (tcp:listener listen)
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((close)  (tcp-close listen))
      ((ready?) (tcp-accept-ready? listen))
      ((accept) (define-values (in out) (tcp-accept listen))
                (list (port:tcp:input in) (port:tcp:output out))))))

(define tcp
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((listen hostname port max-wait reuse?)
       (tcp:listener (tcp-listen port max-wait reuse? hostname)))
      ((connect host port localhost localport)
       (define-values (in out) (tcp-connect host port localhost localport))
       (list (port:tcp:input in) (port:tcp:output out))))))

;; TODO: what does a pre-connected udp port object look like?
;(define (port:udp port) ...)  ;; connected udp ports: udp-send, udp-receive

;; TODO: maybe these shouldn't be fully-fledged file ports?
;; Might want to hide: truncate, position
(define stdio (console (port:file:input  (current-input-port))
                       (port:file:output (current-output-port))
                       (port:file:output (current-error-port))))

(define (port:string:input s)
  (define v (string->vector s))
  (define i 0)
  (define (ref i) (and (< i (vector-length v)) (vector-ref v i)))
  (method-lambda
    ((get)       (define b (ref i))
                 (when b (set! i (+ i 1)))
                 b)
    ((peek skip) (ref (+ i skip)))))
;; TODO: port:string:output
;; TODO: vector, mvector ports.
;; TODO: synchronous channel ports.
;; TODO: generator ports.

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
