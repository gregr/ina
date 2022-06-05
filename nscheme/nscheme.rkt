#lang racket/base
(provide vector->svector svector->vector svector? svector-length svector-ref
         mvector->vector make-mvector mvector? mvector-length mvector-ref mvector-set!
         utf8->string string->utf8 bytevector? bytevector-length bytevector-ref
         mbytevector->bytevector make-mbytevector mbytevector? mbytevector-length mbytevector-ref mbytevector-set!
         bitwise-arithmetic-shift << >> & \| ^
         case-clause-param case-clause-body
         code-source-info code-captured-variables code-case-clauses
         procedure-metadata:closure? procedure-metadata:closure-code procedure-metadata:closure-captured-values
         procedure-metadata:primitive? procedure-metadata:primitive-name
         procedure-metadata:io? procedure-metadata:io-name procedure-metadata:io-descriptor
         procedure-metadata
         procedure-primitive! procedure-io! procedure-closure!
         string->vector vector->string cons*
         stdio filesystem tcp udp tty
         console string:port:input string:port:output null:port:output
         call-with-input-string call-with-output-string
         port-close port-flush port-put port-put*
         port-forget port-get port-peek port-get*! port-peek*!
         port-truncate port-position port-position-set!
         port-buffer-mode port-buffer-mode-set!
         method-lambda method-choose method-unknown method-except method-only
         racket:eval)
(require racket/file racket/list racket/match racket/port racket/string racket/struct
         racket/system racket/tcp racket/udp racket/vector)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Data primitives ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; - small constant: eof null boolean
;; - number:
;;   - exact rational
;;     - integer
;;       - fixnum
;;       - bignum
;;     - ratnum
;;   - inexact
;;     - flonum (various precisions)
;;   - machine [unsigned] integer (various precisions)
;; - symbol, string, bytevector, vector, pair
;; - svector (special, distinct immutable vector type)
;; - mvector, mbytevector (mutable)
;; - procedure:
;;   - primitive
;;   - io controller
;;   - closure

(struct mbytevector (bv))
(struct mvector     (v))
(struct svector     (v) #:prefab)

(define (vector->svector v) (svector v))

(define (svector->vector sv)  (svector-v sv))
(define (svector-length sv i) (vector-length (svector->vector sv)))
(define (svector-ref    sv i) (vector-ref    (svector->vector sv) i))

(define (make-mvector          len x)  (mvector       (make-vector len x)))
(define (mvector-length        mv)     (vector-length (mvector-v mv)))
(define (mvector-ref           mv i)   (vector-ref    (mvector-v mv) i))
(define (mvector-set!          mv i x) (vector-set!   (mvector-v mv) i x))
(define (mvector->vector       mv)     (vector-copy   (mvector-v mv)))
(define (unsafe-mvector-freeze mv)     (mvector-v mv))

(define (utf8->string bv) (bytes->string/utf-8 bv))
(define (string->utf8 s)  (string->bytes/utf-8 s))

(define (bytevector?       bv)   (bytes?       bv))
(define (bytevector-length bv)   (bytes-length bv))
(define (bytevector-ref    bv i) (bytes-ref    bv i))

(define (make-mbytevector          len b)   (mbytevector       (make-bytes len b)))
(define (mbytevector-length        mbv i)   (bytevector-length (mbytevector-bv mbv)))
(define (mbytevector-ref           mbv i)   (bytevector-ref    (mbytevector-bv mbv) i))
(define (mbytevector-set!          mbv i b) (bytes-set!        (mbytevector-bv mbv) i b))
(define (mbytevector->bytevector   mbv)     (bytes-copy (mbytevector-bv mbv)))
(define (unsafe-mbytevector-freeze mbv)     (mbytevector-bv mbv))

;; TODO: define primitive fixnum/macnum operations?
(define (bitwise-arithmetic-shift a b) (arithmetic-shift a b))
(define (<< i s) (bitwise-arithmetic-shift i s))
(define (>> i s) (bitwise-arithmetic-shift i (- s)))
(define (& a b)  (bitwise-and a b))
(define (\| a b) (bitwise-ior a b))
(define (^ a b)  (bitwise-xor a b))

(define (make-case-clause param body) (vector param body))
(define (case-clause-param cc)        (vector-ref cc 0))
(define (case-clause-body  cc)        (vector-ref cc 1))

(define (make-code sinfo cvars case-clauses) (vector sinfo cvars case-clauses))
(define (code-source-info        c)          (vector-ref c 0))
(define (code-captured-variables c)          (vector-ref c 1))
(define (code-case-clauses       c)          (vector-ref c 2))

(define (procedure-metadata:closure code cvalues)       (vector 'closure code cvalues))
(define (procedure-metadata:closure?                pm) (eq? (vector-ref pm 0) 'closure))
(define (procedure-metadata:closure-code            pm) (vector-ref pm 1))
(define (procedure-metadata:closure-captured-values pm) (vector-ref pm 2))
;; A primitive is an operation that is implemented directly in the platform
;; layer.  These operations will typically be portable, but it is possible to
;; define platform-specific operations.  To remain portable when snapshotting
;; part of a system, code that makes use of a platform-specific operation
;; should factor out any values that refer to the operation, so they can be
;; marked as shared values.  When the snapshot is built, shared values will be
;; omitted, and a host which loads the snapshot is responsible for supplying
;; substitutes for these values.  This is analogous to dynamic linking with a
;; shared library in mainstream operating systems.  A substitute would be
;; implemented in a way that is both consistent with the original, and
;; compatible with the new host, solving the portability issue.
(define (procedure-metadata:primitive name)    (vector 'primitive name))
(define (procedure-metadata:primitive?     pm) (eq? (vector-ref pm 0) 'primitive))
(define (procedure-metadata:primitive-name pm) (vector-ref pm 1))
;; An io controller is a transient, host-specific input/output capability.  In
;; some cases, particularly when the capability maps to a virtual io device, a
;; system can optionally persist the corresponding device state and package it
;; with a program snapshot.  But more often, when a host loads the snapshot,
;; this capability will be attached to a new io device of the host's choosing.
(define (procedure-metadata:io name desc)     (vector 'io name desc))
(define (procedure-metadata:io?           pm) (eq? (vector-ref pm 0) 'primitive))
(define (procedure-metadata:io-name       pm) (vector-ref pm 1))
(define (procedure-metadata:io-descriptor pm) (vector-ref pm 2))

;; Procedure metadata is stored as a thunk.  This laziness simplifies the
;; generated Racket code for attaching metadata to a procedure, allowing the
;; attachment to occur immediately after construction of the procedure itself.
;; This immediate attachment would not always be possible without the laziness,
;; particularly when a procedure is bound in a recursive context, such as a
;; letrec, while capturing other values bound in the same letrec.  The problem
;; is these captured values are a component of the metadata, and may not have
;; been initialized by the time the procedure is constructed.  By wrapping the
;; metadata in a thunk, we no longer need to worry about initialization order.
(define procedure=>metadata (make-weak-hash))
(define (procedure-metadata p) ((hash-ref procedure=>metadata p (lambda () (error "procedure has no metadata" p)))))

;; These operations are Racket-specific.  The above procedure-metadata:X
;; constructors should also only appear in Racket code.  However,
;; procedure-metadata and the procedure-metadata:X accessors are actual nscheme
;; operations, and are not Racket-specific.
(define (procedure-metadata-set! p pmeta)      (hash-set! procedure=>metadata p pmeta))
(define (procedure-primitive!    p name)       (procedure-metadata-set! p (lambda () (procedure-metadata:primitive name))))
(define (procedure-io!           p name desc)  (procedure-metadata-set! p (lambda () (procedure-metadata:io        name desc))))
(define (procedure-closure!      p code cvals) (procedure-metadata-set! p (lambda () (procedure-metadata:closure   code cvals))))

(define-syntax-rule (declare-primitives! name ...)
  (for-each procedure-primitive! (list name ...) '(name ...)))

(declare-primitives!
  eq? eqv? eof-object? null? procedure? pair? cons car cdr
  string->symbol symbol->string symbol? string? vector vector? vector-length vector-ref
  vector->svector svector->vector svector? svector-length svector-ref
  mvector->vector make-mvector mvector? mvector-length mvector-ref mvector-set!
  utf8->string string->utf8 bytevector? bytevector-length bytevector-ref
  mbytevector->bytevector make-mbytevector mbytevector? mbytevector-length mbytevector-ref mbytevector-set!
  number? exact? integer? inexact? = <= < + - * / quotient remainder truncate integer-length
  bitwise-arithmetic-shift << >> & \| ^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snapshot saving and loading ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ast:quote v)                      `#(quote ,v))
(define (ast:ref name)                     `#(ref ,name))
(define (ast:if ast.c ast.t ast.f)         `#(if ,ast.c ,ast.t ,ast.f))
(define (ast:begin ast.effect* ast.result) `#(begin ,ast.effect* ,ast.result))
(define (ast:call ast.proc . ast.args)     `#(call #f ,ast.proc ,ast.args))
(define (ast:case-lambda sinfo clause*)    `#(case-lambda ,sinfo ,clause*))
(define (ast:letrec bpair* body)           `#(letrec ,bpair* ,body))
(define (ast:lambda sinfo param ast.body)  (ast:case-lambda sinfo (list (make-case-clause param ast.body))))
(define (ast:let bpair* body)              (apply ast:call (ast:lambda #f (map car bpair*) body) (map cdr bpair*)))
(define (ast:list ast*)                    (apply ast:call (ast:lambda #f 'xs (ast:ref 'xs)) ast*))

(define (ast:binding-pair lhs rhs)   (cons lhs rhs))
(define (ast:binding-pair-lhs bpair) (car bpair))
(define (ast:binding-pair-rhs bpair) (cdr bpair))

(define (ast-lift-complex-values ast value->name)
  (let loop ((ast ast))
    (match ast
      (`#(quote ,value)                         (if (or pair? vector? svector? mvector? mbytevector? procedure?)
                                                  (ast:ref (value->name value))
                                                  ast))
      (`#(ref ,name)                            ast)
      (`#(if ,condition ,body.true ,body.false) (ast:if (loop condition) (loop body.true) (loop body.false)))
      (`#(begin ,effect* ,result)               (ast:begin (map loop effect*) (loop result)))
      (`#(call ,sinfo ,proc ,arg*)              (ast:call sinfo (loop proc) (map loop arg*)))
      (`#(case-lambda ,sinfo ,case-clause*)     (ast:case-lambda sinfo
                                                                 (map (lambda (cc) (make-case-clause
                                                                                     (case-clause-param cc)
                                                                                     (loop (case-clause-body cc))))
                                                                      case-clause*)))
      (`#(letrec ,bpair* ,body)                 (ast:letrec (map (lambda (bp) (ast:binding-pair
                                                                                (ast:binding-pair-lhs bp)
                                                                                (loop (ast:binding-pair-rhs bp))))
                                                                 bpair*)
                                                            (loop body))))))

(struct snapshot (primitive* io* value* initialization* root))

(define (snapshot-ast ss ast:primitive ast:io external-binding-pairs)
  (ast:let external-binding-pairs
           (ast:letrec (append (map (lambda (name pname)
                                      (ast:binding-pair name (ast:primitive pname)))
                                    (map car (snapshot-primitive* ss))
                                    (map cdr (snapshot-primitive* ss)))
                               (map (lambda (name pname&desc)
                                      (ast:binding-pair name (ast:io (car pname&desc) (cdr pname&desc))))
                                    (map car (snapshot-io* ss))
                                    (map cdr (snapshot-io* ss)))
                               (snapshot-value* ss))
                       (ast:begin (snapshot-initialization* ss) (snapshot-root ss)))))

(define (make-snapshot value.root id->name external-value=>name)
  (let ((value=>name      (make-hasheq))
        (primitive*       '())
        (io*              '())
        (procedure*       '())
        (other*           '())
        (initialization** '()))
    (define (ast:prim   p)                    (ast:ref (loop p)))
    (define (args-name)      (id->name (- -1 (hash-count value=>name))))
    (define (gen-name value) (let ((name (id->name (hash-count value=>name))))
                               (hash-set! value=>name value name)
                               name))
    (define-syntax-rule (push! stack name value)
      (set! stack (cons (cons name value) stack)))
    (define (ast:ref/loop v) (ast:ref (loop v)))
    (define (loop value)
      (or (hash-ref value=>name          value #f)
          (hash-ref external-value=>name value #f)
          (match value
            ((or '() #f #t (? fixnum?) (? eof-object?)) (ast:quote value))
            (_ (let ((name (gen-name value)))
                 (match value
                   ((? procedure?)
                    (match (procedure-metadata value)
                      (`#(primitive ,pname) (push! primitive* name pname))
                      (`#(io ,pname ,desc)  (push! io*        name (cons pname desc)))
                      (`#(closure ,code ,cvalues)
                        (let* ((sinfo (code-source-info code))
                               (name.code
                                 (or (hash-ref value=>name code #f)
                                     (let ((name  (gen-name code)))
                                       (push! procedure* name
                                              (ast:lambda
                                                sinfo (code-captured-variables code)
                                                (ast:case-lambda
                                                  sinfo (map (lambda (cc)
                                                               (make-case-clause (case-clause-param cc)
                                                                                 (ast-lift-complex-values
                                                                                   (case-clause-body cc) loop)))
                                                             (code-case-clauses code)))))
                                       name)))
                               (name.args (args-name)))
                          (push! procedure* name (ast:lambda sinfo name.args
                                                             (ast:call (ast:prim apply)
                                                                       (apply ast:call (ast:ref name.code)
                                                                              (map ast:ref/loop cvalues))
                                                                       (ast:ref name.args))))))))
                   ((? mvector?)
                    (push! other* name (ast:call (ast:prim make-mvector)
                                                 (ast:ref/loop (mvector-length value)) (ast:ref/loop 0)))
                    (set! initialization**
                      (cons (map (lambda (i)
                                   (ast:call (ast:prim mbytevector-set!)
                                             (ast:ref name) (ast:ref/loop i)
                                             (ast:ref/loop (mbytevector-ref value i))))
                                 (range (mbytevector-length value)))
                            initialization**)))
                   (_ (let ((ast (match value
                                   ((cons v.a v.d) (ast:call (ast:prim cons)
                                                             (ast:ref/loop v.a) (ast:ref/loop v.d)))
                                   ((? vector?)    (apply ast:call (ast:prim vector)
                                                          (map ast:ref/loop (vector->list value))))
                                   ((? svector?)   (ast:call (ast:prim vector->svector)
                                                             (ast:ref/loop (svector->vector value))))
                                   ((? mbytevector?)
                                    (set! initialization**
                                      (cons (map (lambda (i)
                                                   (ast:call (ast:prim mbytevector-set!)
                                                             (ast:ref name) (ast:ref/loop i)
                                                             (ast:ref/loop (mbytevector-ref value i))))
                                                 (range (mbytevector-length value)))
                                            initialization**))
                                    (ast:call (ast:prim make-mbytevector)
                                              (ast:ref/loop (mbytevector-length value)) (ast:ref/loop 0)))
                                   ((or (? number?) (? symbol?) (? string?) (? bytevector?))
                                    (ast:quote value)))))
                        (push! other* name ast))))
                 name)))))
    (let ((ast.root (ast:ref/loop value.root)))
      (snapshot (reverse primitive*)
                (reverse io*)
                (append (reverse procedure*) (reverse other*))
                (foldl append '() initialization**)
                ast.root))))


;;; TODO: reorganize the remainder of this file.

;; TODO: we don't want these operations.  Use bytevectors instead.
(define (string->vector s)
  (list->vector (bytes->list (string->bytes/utf-8 s))))
(define (vector->string v)
  (bytes->string/utf-8 (list->bytes (vector->list v))))


(define cons* list*)


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
;; TODO: this operation needs to be removed
(define (mvector-copy!/string mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end
                     (lambda (s i) (char->integer (string-ref s i)))))

(define (call-with-input-string s k) (k (string:port:input s)))
;; TODO: consider wrapping out to limit capability given to k
(define (call-with-output-string  k) (let ((out (string:port:output)))
                                       (k out)
                                       (out 'string)))

;; TODO: synchronizable events for ports

(define (port-close    p)   (p 'close))
(define (port-flush    p)   (p 'flush))
(define (port-put      p b) (p 'put  b))
;; TODO: this should take a (byte)vector as input
(define (port-put*     p s) (p 'put* s))
;; TODO: define port-put-string separately for convenience

(define (port-forget p amount) (p 'forget amount))
(define (port-get    p)        (p 'get))
(define (port-peek   p skip)   (p 'peek skip))
;; TODO: ideally these would be a general implementation in terms of port-get/peek.
;; Revisit these when testing compiler optimizations.
;; TODO: also, define non-! versions that return a new (byte)vector for convenience
(define (port-get*!  p mv start len)        (p 'get*!  mv start len))
(define (port-peek*! p mv start skip until) (p 'peek*! mv start skip until))

(define (port-truncate         p)   (p 'truncate))
(define (port-position         p)   (p 'position-ref))
(define (port-position-set!    p i) (p 'position-set! i))
(define (port-buffer-mode      p)   (p 'buffer-mode-ref))
(define (port-buffer-mode-set! p m) (p 'buffer-mode-set! m))

;; TODO: model file descriptors and their operations directly?

;; TODO: for simplicity, have get* directly return a string rather than fill a buffer?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic bytestream IO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ;; TODO: this should take a (byte)vector as input
    ((put* s) (write-string s port))
    ((flush)  (flush-output port))
    (else     super)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File IO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network communication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tcp
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((listen hostname port (max-wait 4) (reuse? #f))
       (tcp:listener (tcp-listen port max-wait reuse? hostname)))
      ((connect host port (localhost #f) (localport #f))
       (define-values (in out) (tcp-connect host port localhost localport))
       (tcp:port in out)))))

(define (tcp:listener listen)
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((close)  (tcp-close listen))
      ;; TODO: use a synchronizable event instead, based on tcp-accept-evt
      ((ready?) (tcp-accept-ready? listen))
      ((accept) (define-values (in out) (tcp-accept listen))
                (tcp:port in out)))))

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
                        (host.local port.local host.remote port.remote)
                        (tcp-addresses in #t))
                      (list host.local port.local host.remote port.remote))
        ((in  . args) (if (null? args) (bytestream:port:input  a.in  in)
                        (apply a.in  args)))
        ((out . args) (if (null? args) (bytestream:port:output a.out out)
                        (apply a.out args))))))
  (bytestream:port:output (bytestream:port:input super in) out))

;; TODO: synchronizable events for udp sending, receiving, and readiness
(define udp
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((open host.family port.family (port.local #f) (reuse? #f))
       (define p (udp:port (udp-open-socket host.family port.family)))
       (when port.local (p 'bind #f port.local reuse?))
       p)
      ((listen host.local port.local (reuse? #f))
       (define p (udp:port (udp-open-socket host.local port.local)))
       (p 'bind host.local port.local reuse?)
       p)
      ((connect host port (port.local #f) (reuse? #f))
       (define p (udp:port (udp-open-socket host port)))
       (when port.local (p 'bind #f port.local reuse?))
       (p 'connect host port)
       p))))

(define (udp:port socket)
  (lambda/handle-fail
    (lambda (x) x)
    (method-lambda
      ((close)                      (udp-close    socket))
      ((bind host port (reuse? #f)) (udp-bind!    socket host port reuse?))
      ((connect host port)          (udp-connect! socket host port))
      ((disconnect)                 (udp-connect! socket #f   #f))
      ((addresses)                  (define-values
                                      (host.local port.local host.remote port.remote)
                                      (udp-addresses socket #t))
                                    (list host.local port.local host.remote port.remote))

      ;; TODO: this should take a (byte)vector as input
      ((put* s)        (udp-send socket (string->bytes/utf-8 s)))
      ((aim host port) (lambda/handle-fail
                         (lambda (x) x)
                         (method-lambda
                           ;; TODO: synchronizable events
                           ;; TODO: this should take a (byte)vector as input
                           ((put* s) (udp-send-to socket host port (string->bytes/utf-8 s))))))

      ;; TODO: omit remote host and port when connected?
      ((get*! mv start len)
       (define        bs                 (make-bytes len 0))
       (define-values (amount host port) (udp-receive! socket bs 0 len))
       (mvector-copy!/bytes mv start bs 0 amount)
       (cons amount (cons host port)))

      ((buffer-size-set! amount) (udp-set-receive-buffer-size! socket amount))
      ((ttl-ref)                 (udp-ttl                      socket))
      ((ttl-set! ttl)            (udp-set-ttl!                 socket ttl))

      ((multicast-join  addr host.local)    (udp-multicast-join-group!    socket addr host.local))
      ((multicast-leave addr host.local)    (udp-multicast-leave-group!   socket addr host.local))
      ((multicast-iface-ref)                (udp-multicast-interface      socket))
      ((multicast-iface-set! host.local)    (udp-multicast-set-interface! socket      host.local))
      ((multicast-loopback?-ref)            (udp-multicast-loopback?      socket))
      ((multicast-loopback?-set! loopback?) (udp-multicast-set-loopback!  socket loopback?))
      ((multicast-ttl-ref)                  (udp-multicast-ttl            socket))
      ((multicast-ttl-set! ttl)             (udp-multicast-set-ttl!       socket ttl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard IO and consoles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (console in out err)
  (lambda/handle-fail (lambda (x) x)
                      (method-lambda ((in) in) ((out) out) ((error) err))))

(define (stdio:port:input  port) (bytestream:port:input  (bytestream:port port) port))
(define (stdio:port:output port) (bytestream:port:output (bytestream:port port) port))

(define stdio (console (stdio:port:input  (current-input-port))
                       (stdio:port:output (current-output-port))
                       (stdio:port:output (current-error-port))))

(define null:port:output
  (method-lambda
    ((put  _) #t)
    ((put* _) #t)
    ((close)  #t)
    ((flush)  #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String IO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: these should be built on (byte)vector:port:input/output

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
    ;; TODO: this should take a (byte)vector as input
    ;; TODO: do not use these string operations
    ((put* s) (define u (- (string-length s) (- (mvector-length buffer) i)))
              (when (< 0 u) (grow (+ (quotient u (mvector-length buffer)) 2)))
              (mvector-copy!/string buffer i s 0 (string-length s))
              (set! i (+ i (string-length s))))
    ((close)               #t)
    ((flush)               #t)
    ((position-ref)        i)
    ((position-set! index) (set! i (min (max index 0) i)))
    ((truncate)            (set! i 0) (set! buffer (make-mvector 32 0)))))

;; TODO: synchronous channels, generators
;; NOTE: these are not ports; ports are restricted to transferring bytes
;; TODO: channels that get from sequences or put to mvectors
;; TODO: generators that iterate over sequences

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TTY manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: see tty.scm for escape codes

(define tty
  (let ()
    (define (command name . args)
      (string-trim
        (with-output-to-string
          (lambda () (apply system* (find-executable-path name) args)))))
    (define (tput arg)    (command "tput" arg))
    (define (stty . args) (apply command "stty" args))
    (method-lambda
      ;; NOTE: ec:display-size can report these, but reading the report may be inconvenient
      ((lines)      (string->number (string-trim (tput "lines"))))
      ((columns)    (string->number (string-trim (tput "cols"))))
      ;; NOTE: these don't seem necessary due to existing escape codes
      ;((clear)       (command "clear")) ; \e[2J
      ;((save)        (tput "smcup"))    ; \e[?47h
      ;((restore)     (tput "rmcup"))    ; \e[?47l
      ;((cursor-show) (tput "cnorm"))    ; \e[?25h
      ;((cursor-hide) (tput "civis"))    ; \e[?25l
      ((stty-ref)   (stty "-g"))
      ((stty-set s) (stty s))
      ((stty-raw)   (stty "raw")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (racket:eval rkt-datum)
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
