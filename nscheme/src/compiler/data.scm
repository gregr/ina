(define (mloc w b d i s) (vector 'mloc w b d i s))
(define (mloc? x) (and (vector? x) (= (vector-length x) 6) (eqv? (vector-ref x 0) 'mloc)))
(define (mloc-width x) (vector-ref x 1))
(define (mloc-base  x) (vector-ref x 2))
(define (mloc-disp  x) (vector-ref x 3))
(define (mloc-index x) (vector-ref x 4))
(define (mloc-shift x) (vector-ref x 5))

(define (make-relocation) (mvector #f))
(define (relocation? x) (and (mvector? x) (= (mvector-length x) 1)))
(define (relocation-ref x) (mvector-ref x 0))
(define (relocation-set! x offset) (mvector-set! x 0 offset))

(splicing-let ((2^63 #x8000000000000000) (2^64 #x10000000000000000))
  (define s64-min (- 2^63))
  (define s64-max (- 2^63 1))
  (define u64-max (- 2^64 1))
  (define (s64 x) (let ((x (integer-floor-mod x 2^64))) (if (< x 2^63) x (- x 2^64))))
  (define (u64 x) (integer-floor-mod x 2^64)))

(splicing-local
  ((define rtd.uvar  (make-rtd 'uvar 4 #t 'uvar))
   (define make-uvar (rtd-constructor rtd.uvar))
   (define uvar-ref  (rtd-accessor    rtd.uvar))
   (define uvar-set! (rtd-mutator     rtd.uvar)))
  (define uvar? (rtd-predicate rtd.uvar))
  (define (uvar name) (make-uvar name #f #f #f))
  (define (uvar-name   x) (uvar-ref x 0))
  (define (uvar-uid    x) (uvar-ref x 1))
  (define (uvar-source x) (uvar-ref x 2))
  (define (uvar-note   x) (uvar-ref x 3))
  (define (set-uvar-uid!    x uid)  (uvar-set! x 1 uid))
  (define (set-uvar-source! x note) (uvar-set! x 2 note))
  (define (set-uvar-note!   x note) (uvar-set! x 3 note))
  (define (uvar->symbol x)
    (let ((name (uvar-name x)) (uid (uvar-uid x)))
      (string->symbol (string-append (if name (symbol->string name) "_") "."
                                     (if uid (number->string uid) "_"))))))

;; TODO: arity is not specific enough
(splicing-local
  ((define rtd.primop (make-rtd 'primop 2 #f 'primop))  ; TODO: behavioral properties and open code
   (define primop-ref (rtd-accessor rtd.primop)))
  (define primop  (rtd-constructor rtd.primop))
  (define primop? (rtd-predicate rtd.primop))
  (define (primop-name  x) (primop-ref x 0))
  (define (primop-arity x) (primop-ref x 1)))

;; TODO: move target-specific definitions to a target-specific file that we can selectively include

;; NOTE: these can be implemented more efficiently as tag-checking at a lower level, but doing so
;; assumes that the target representation matches the host representation.  Also, it's not clear
;; that we should make assumptions about the target representation here in the first place.  We
;; likely want to be representation-agnostic until later phases of the compiler.  However, we have
;; to decide how to normalize quoted data early in the compiler.  Therefore we should probably
;; parameterize quote normalization by the representation choice.  This is not a big deal because
;; we already need to parameterize quote normalization by whether to preserve the literal runtime
;; values that were quoted, or to reconstruct them.
;; TODO: is it really important to normalize quotes early?
;; - to expose procedures that should be converted to lambdas, yes
;; - alternatively, could we just lift ALL quotes?
;;   - except maybe the most obvious immediates (null and boolean)?
;;   - and leave further simplification to a later phase that knows the representation
(define (fixnum? x) (and (integer? x) (<= #x-1000000000000000 x #x0fffffffffffffff)))
(define (immediate? x)
  (define (string-immediate? x) (<= (bytes-length x) 7))
  (or (null? x) (boolean? x) (fixnum? x)
      (and (symbol? x) (string-immediate? (symbol->string x)))
      (and (string? x) (string-immediate? x))
      (and (vector? x) (= (vector-length x) 0))))

(define procedure->primop
  (let ((proc=>primop
          (map (lambda (n=>p) (cons (cdr n=>p) (primop (car n=>p) #f)))
               ;; TODO: also build primops for procedures we might not have access to here
               (aquote
                 panic apply values call/values make-record-type describe
                 eqv? null? boolean? procedure? symbol? rational? integer?
                 pair? vector? mvector? bytes? mbytes?
                 cons car cdr vector vector-length vector-ref
                 make-mvector mvector->vector mvector-length mvector-ref mvector-set!
                 bytes bytes-length bytes-ref bytes->symbol symbol->bytes
                 make-mbytes mbytes->bytes mbytes-length mbytes-ref mbytes-set!
                 bitwise-asl bitwise-asr bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length
                 integer-floor-divmod numerator denominator = <= >= < > + - * /))))
    (lambda (proc) (let ((pp (assv proc proc=>primop))) (and pp (cdr pp))))))
