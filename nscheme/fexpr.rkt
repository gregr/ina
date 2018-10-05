#lang racket/base
(provide eval env:base s->ns ns->s plift)
(require racket/bool racket/control racket/vector)

(define (s->ns d)
  (cond ((symbol? d) (symbol->string d))
        ((pair? d)   (cons (s->ns (car d)) (s->ns (cdr d))))
        ((vector? d) (vector-map s->ns d))
        (else        d)))
(define (ns->s d)
  (cond ((string? d) (string->symbol d))
        ((pair? d)   (cons (ns->s (car d)) (ns->s (cdr d))))
        ((vector? d) (vector-map ns->s d))
        (else        d)))
(define (plift racket-proc) (lambda (a) (apply racket-proc a)))
(define (procedure=? m n)   (eq? m n))
(struct mvector (v) #:transparent)
(define (make-mvector k d)          (mvector (make-vector k d)))
(define (mvector=? m n)             (eq? m n))
(define (mvector-length mv)         (vector-length (mvector-v mv)))
(define (mvector-ref mv i)          (vector-ref (mvector-v mv) i))
(define (mvector-set! mv i new)     (vector-set! (mvector-v mv) i new) #t)
;; TODO: update Racket to use this.
;(define (mvector-cas! mv i old new) (vector-cas! (mvector-v mv) i old new))
(define (mvector->vector mv)        (vector-copy (mvector-v mv)))
(define (string->vector s) (list->vector (map char->integer (string->list s))))
(define (vector->string v) (list->string (map integer->char (vector->list v))))

;; TODO:
;; alist-ref, alist-access/list-at
;; properties: apply, define
;; define a more general make-eval in parsing/applicative nscheme
(define (env-ref-prop env id property default)
  (let ((rib (assoc id env)))
    (if (not rib) (error '"unbound identifier:" id)
      (let ((kv (assoc property (cdr rib))))
        (if kv (cdr kv) default)))))

;; TODO: include definition list process here.  Define define/begin separately.
(define (@lambda env param body)
  (define names
    (let loop ((p param) (ns '()))
      (cond ((pair? p) (loop (cdr p) (loop (car p) ns)))
            ((string? p)
             (when (member p ns) (error '"duplicate parameter:" p param))
             (cons p ns))
            ((null? p) ns)
            ((vector? p) (loop (vector->list p) ns))
            (else (error '"invalid parameter:" p param)))))
  (define (tree->b* param arg)
    (let loop ((p param) (a arg))
      (cond ((and (pair? p) (pair? a)) (append (loop (car p) (car a))
                                               (loop (cdr p) (cdr a))))
            ((string? p)               (list (cons p a)))
            ((and (null? p) (null? a)) '())
            ((and (vector? p) (vector? a))
             (loop (vector->list p) (vector->list a)))
            (else (error '"parameter/argument mismatch:" param arg p a)))))
  (let ((cenv (filter (lambda (rib) (not (member (car rib) names))) env)))
    (lambda (a)
      (define (b->rib b)
        (let* ((cell (make-mvector 1 (cdr b)))
               (get (plift (lambda ()  (mvector-ref  cell 0))))
               (set (plift (lambda (v) (mvector-set! cell 0 v)))))
          (cons (car b) (list (cons '"ref" get) (cons '"set!" set)
                              '(applicative? . #t)))))
      (eval (append (map b->rib (tree->b* param a)) cenv) body))))

(define (@apply proc arg)   (proc arg))
(define (@quote env d)      d)
(define (@if env c t f)     (if (eval env c) (eval env t) (eval env f)))
(define (@reset env body)   (reset ((@lambda env '() body))))
(define (@shift env p body) (shift k ((@lambda env p body) k)))
(define (@set! env id v)    ((or (env-ref-prop env id '"set!" #f)
                                 (error '"identifier cannot be set!:" id))
                             (list v)))

(define (eval env form)
  (cond ((pair? form)
         (let* ((p (car form)) (a* (cdr form)) (proc (eval env p)))
           (if (and (string? p) (not (env-ref-prop env p '"applicative?" #f)))
             (proc (cons env a*))
             (@apply proc (map (lambda (a) (eval env a)) a*)))))
        ((string? form) ((or (env-ref-prop env form '"ref" #f)
                             (error '"identifier is not a variable:" form))
                         '()))
        ((or (boolean? form) (number? form)) (@quote env form))
        ((procedure? form)                   (form env))
        (#t                                  (error '"invalid syntax:" form))))

(define (ref-proc p) (cons 'ref (lambda _ (plift p))))
(define env:base
  (s->ns
    (append
      (map (lambda (b) (cons (car b) (list (ref-proc (cdr b)))))
           (list (cons 'quote  @quote)
                 (cons 'if     @if)
                 (cons 'reset  @reset)
                 (cons 'shift  @shift)
                 (cons 'lambda @lambda)
                 ;; TODO: derive?
                 (cons 'set!   @set!)
                 ;; TODO: cond, let, let*, letrec, begin, when, unless, and, or
                 ))
      (map (lambda (b) (cons (car b) (list (cons 'ref (lambda _ (cdr b))))))
           (list (cons '$ (lambda (args) ((eval (car args) (cadr args))
                                          (cons (car args) (cddr args)))))
                 ))

      (map (lambda (b) (cons (car b) (list (ref-proc (cdr b))
                                           '(applicative? . #t))))
           (list (cons 'eval  eval)
                 (cons 'apply @apply)

                 (cons 'mvector?   mvector?)
                 (cons 'vector?    vector?)
                 (cons 'pair?      pair?)
                 (cons 'null?      null?)
                 (cons 'string?    string?)
                 (cons 'number?    number?)
                 (cons 'integer?   integer?)
                 (cons 'boolean?   boolean?)
                 (cons 'procedure? procedure?)

                 (cons 'boolean=?   boolean=?)
                 (cons 'number=?    =)
                 (cons 'string=?    string=?)
                 (cons 'mvector=?   mvector=?)
                 (cons 'procedure=? procedure=?)

                 (cons 'string->vector string->vector)
                 (cons 'vector->string vector->string)

                 (cons 'cons cons)
                 (cons 'car  car)
                 (cons 'cdr  cdr)

                 (cons 'vector-ref    vector-ref)
                 (cons 'vector-length vector-length)

                 (cons 'make-mvector    make-mvector)
                 (cons 'mvector->vector mvector->vector)
                 (cons 'mvector-set!    mvector-set!)
                 (cons 'mvector-ref     mvector-ref)
                 (cons 'mvector-length  mvector-length)

                 (cons '=  =)
                 (cons '<= <=)
                 (cons '<  <)
                 (cons '+  +)
                 (cons '*  *)
                 (cons '-  -)
                 (cons '/  /)

                 ;; TODO: these and others?
                 ;bitwise-and
                 ;bitwise-ior
                 ;bitwise-xor
                 ;bitwise-not
                 ;bitwise-bit-set?
                 ;bitwise-bit-field
                 ;arithmetic-shift
                 ;integer-length

                 ;round
                 ;quotient
                 ;remainder
                 ))
      ;; TODO: derived base procedures?
      )))


;; Tests:
(define tests-total 0)
(define test-failures '())

(define (test-report)
  (define tests-failed (length test-failures))
  (define tests-passed (- tests-total tests-failed))
  (printf "********************************\nTests passed: ~a out of ~a\n"
          tests-passed tests-total)
  (unless (= tests-passed tests-total)
    (printf "Tests failed: ~a out of ~a\n" tests-failed tests-total)
    (printf "~s\n" test-failures)))

(define (test name actual expected)
  (printf "Testing ~a: " name)
  (set! tests-total (+ tests-total 1))
  (cond ((equal? expected actual) (printf "Succeeded.\n"))
        (else (printf "Failed.\nExpected: ~s\nActual: ~s\n****************\n"
                      expected actual)
              (set! test-failures (cons name test-failures)))))

(define (ev code) (ns->s (eval env:base (s->ns code))))

(test '$-1  ;; $ applies a procedure with current environment and raw syntax.
 (ev '($ (lambda (env . tree) tree) 4 5))
 '(4 5))
(test '$-2  ;; We can even apply such procedures with improper argument lists.
 (ev '($ (lambda (env . tree) tree) 4 . 5))
 '(4 . 5))

(test 'lambda-1  ;; Formal parameter lists are generalized to arbitrary trees.
  (ev '((lambda (() a (b)) (cons a b))
        '() 1 '(2)))
  '(1 . 2))
(test 'lambda-2  ;; Formal parameter lists are generalized to arbitrary trees.
  (ev '((lambda (() a #(b c)) (cons a (cons b (cons c '()))))
        '() 1 '#(2 3)))
  '(1 2 3))

(test 'apply-lambda-1
  (ev '((apply lambda (cons '()         ;; empty env
                            '(_ 5)))))  ;; quote is unbound
  5)
(test 'apply-lambda-2
  (ev `((apply lambda (cons ,(lambda (env) env)  ;; env via injected evaluator
                            '(_ '5)))))          ;; quote is bound
  5)
(test 'apply-lambda-3
  (ev '((apply lambda (cons ($ (lambda (env) env))  ;; env via $
                            '(_ '5)))))             ;; quote is bound
  5)

(test-report)
