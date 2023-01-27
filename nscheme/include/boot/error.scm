(splicing-local
  ((define rtd.error (vector 2)))
  (define (make-error kind details)
    (let ((e (make-record rtd.error 0)))
      (record-set! e 0 kind)
      (record-set! e 1 details)
      e))
  (define (error? x) (and (record? x) (eq? (record-type-descriptor x) rtd.error))))

(define (error?!       x) (has-type?! error? 'error x))
(define (error-kind    e) (error?! e) (record-ref e 0))
(define (error-details e) (error?! e) (record-ref e 1))


(define (make-error:syntax description . stx*)
  (make-error 'syntax (list (cons 'description description)
                            (cons 'location*   stx*))))

(define (make-error:system problem operation . operand*)
  (make-error 'system (list (cons 'problem   problem)
                            (cons 'operation operation)
                            (cons 'operand*  operand*))))

(define (make-error:arithmetic problem operation . operand*)
  (make-error 'arithmetic (list (cons 'problem   problem)
                                (cons 'operation operation)
                                (cons 'operand*  operand*))))

;; TODO: programming mistakes should panic rather than raise:
;; - assertion violations
;; - type violations
;; - procedure arity violations
;; - accessing an out-of-bounds index
;; - accessing an uninitialized variable
;; - dividing an exact integer by zero
;; Errors that are not programming mistakes should raise.

(define (make-error:assertion stx)
  (make-error 'assertion (list (cons 'location* (list stx)))))

;; The compiler will generate code for implicit error-checking by inlining these constructors.
;; This implicit error-checking will take place inside primitive operator definitions, sites that
;; call unknown procedures, arity-checking preludes of non-primitive procedure definitions, and
;; sites that reference variables whose initialization status is unknown.
(define (make-error:type expected datum)
  (make-error 'type (list (cons 'expected expected)
                          (cons 'given    datum))))

(define (make-error:procedure-arity proc given)
  (make-error 'procedure-arity (list (cons 'object proc)
                                     (cons 'given  given))))

(define (make-error:index-bounds object index)
  (make-error 'index-bounds (list (cons 'object object)
                                  (cons 'given  index))))

(define (make-error:uninitialized stx)
  (make-error 'uninitialized (list (cons 'location* (list stx)))))


(define (raise-type-error expected given)
  (raise (make-error:type expected given)))

(define (has-type?! type? expected given)
  (unless (type? given) (raise-type-error expected given)))

(define (raise-syntax-error description . stx*)
  (raise (apply make-error:syntax description stx*)))

;; TODO: or just: (define (error . details) (raise (make-error #f details)))
(define (error message . irritants)
  (raise (make-error #f (cons message irritants))))

(define (raise c)
  (if (error? c)
      (apply panic 'error (error-kind c) (error-details c))
      (panic 'unknown-condition c)))

;;; TODO: clean up these older notes:

;error vs. assertion-violation

;(assert <expr>) ; assert is syntax, not a procedure

; condition: warning vs. serious vs. message + irritants + who
; serious: error vs. violation
; violation: assertion, non-continuable, implementation-restriction, lexical (i.e., read), syntax (i.e., parse), undefined (i.e., unbound variables)

; better distinctions:
; - call the root base class "exception"
; - error vs. warning vs. interrupt/async/break/alarm/resource-budget-exceeded vs. etc.
;   - maybe warnings should not be considered exceptions; only serious conditions are exceptions?
; - error: failure (e.g., io device failure) vs. mistake/violation (assertion, type, syntax, read/lexical, unbound/undefined, implementation-restriction)
;;  - #s(error kind subkind message irritants)
;;  - failure:
;;    - network
;;    - filesystem
;;    - read
;;    - write
;;    ;- memory? likely a bad idea to allow handling this
;;  - violation:
;;    - assertion  stx
;;    - type   given expected "integer"  article: ("a" vs. "an")
;;    - arity  given expected
;;    - index  object index
;;    - arithmetic  operator operands problem
;;      - problem:
;;        - divide-by-zero
;;        - overflow
;;        - underflow
;;    - lexical
;;    - syntax   expected stx*
;;    - syntax:unbound  stx*
