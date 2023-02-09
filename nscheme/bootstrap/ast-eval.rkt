#lang racket/base
(provide ast-eval ast-pretty)
(require "../platform/racket/nscheme.rkt" racket/include racket/match)
(include "../include/ast.scm")

;;; NOTE: this code is only intended for testing until the compiler is finished.  The bootstrapping
;;; process should not depend on ast-eval.

(define cenv.empty '())
(define renv.empty '())

(define (cenv-ref cenv.0 addr)
  (let loop ((j 0) (cenv.1 cenv.0))
    (match cenv.1
      ('() (error "unbound or out-of-phase reference" addr))
      ((cons (cons rec? (cons variadic? addrs)) cenv)
       (let find ((i 0) (a* addrs))
         (match a*
           ('()         (loop (+ j 1) cenv))
           ((cons a a*) (if (eq? a addr)
                            (if rec?
                                (if variadic?
                                    (error "recursive frame cannot be variadic" cenv.1)
                                    (lambda (renv) (unbox (list-ref (list-ref renv j) i))))
                                (if (and variadic? (null? a*))
                                    (lambda (renv) (list-tail (list-ref renv j) i))
                                    (lambda (renv) (list-ref  (list-ref renv j) i))))
                            (find (+ i 1) a*)))))))))

(define (cenv-extend      cenv param) (cenv-extend/rec? cenv param #f))
(define (cenv-extend-rec  cenv param) (cenv-extend/rec? cenv param #t))
(define (cenv-extend/rec? cenv param rec?)
  (cons (cons rec? (let loop ((param param) (addrs '()))
                     (cond ((null? param) (cons #f (reverse addrs)))
                           ((pair? param) (loop (cdr param) (cons (car param) addrs)))
                           (else          (cons #t (reverse (cons param addrs)))))))
        cenv))

(define (param-arity param)
  (let loop ((param param) (arity 0))
    (cond ((null? param) arity)
          ((pair? param) (loop (cdr param) (+ arity 1)))
          (else          (- (+ arity 1))))))

(define (make-renv-extend arity count)
  (and (if (< arity 0)
         (<= (- (+ arity 1)) count)
         (= arity count))
       (lambda (renv args) (cons args renv))))

(define (ast-stage ast cenv)
  (let loop.full ((ast ast) (cenv cenv))
    (define (loop ast) (loop.full ast cenv))
    (match ast
      (`#(quote ,_ ,value)          (lambda (renv) value))
      (`#(ref   ,_ ,address)        (cenv-ref cenv address))
      (`#(if    ,_ ,a.c ,a.t ,a.f)  (let ((s.c (loop a.c)) (s.t (loop a.t)) (s.f (loop a.f)))
                                      (lambda (renv) (if (s.c renv) (s.t renv) (s.f renv)))))
      (`#(call  ,_ ,a.proc ,a*.arg) (let ((s.proc (loop a.proc)) (s*.arg (map loop a*.arg)))
                                      (lambda (renv) (apply (s.proc renv)
                                                            (map (lambda (s) (s renv)) s*.arg)))))
      (`#(case-lambda ,_ ,cc*)
        (let ((k (foldr (lambda (cc k)
                          (let* ((param  (case-lambda-clause-param cc))
                                 (cenv   (cenv-extend cenv param))
                                 (s.body (loop.full (case-lambda-clause-body cc) cenv))
                                 (arity  (param-arity param)))
                            (lambda (renv count args)
                              (let ((renv-extend (make-renv-extend arity count)))
                                (if renv-extend
                                    (s.body (renv-extend renv args))
                                    (k renv count args))))))
                        (lambda (renv count args)
                          (error "arity mismatch"
                                 'given count 'expected (map case-lambda-clause-param cc*)))
                        cc*)))
          (lambda (renv)
            (lambda args (k renv (length args) args)))))
      (`#(letrec ,_ ,bpair* ,a.body)
        (let* ((param       (map binding-pair-left bpair*))
               (arity       (param-arity param))
               (renv-extend (make-renv-extend arity (length param)))
               (s.body      (loop.full a.body (cenv-extend cenv param)))
               (cenv        (cenv-extend-rec cenv param))
               (s*.arg      (map (lambda (bp) (loop.full (binding-pair-right bp) cenv))
                                 bpair*)))
          (lambda (renv)
            (let ((loc*.arg (map (lambda (_) (box (void))) s*.arg)))
              (let ((renv     (renv-extend renv loc*.arg)))
                (for-each (lambda (loc s) (set-box! loc (s renv))) loc*.arg s*.arg))
              (s.body (renv-extend renv (map unbox loc*.arg)))))))
      (`#(prim ,_ ,name)
        (define-syntax-rule (case-symbol->var x sym ...)
          (case x ((sym) sym) ... (else (error "unknown primitive" x))))
        (let ((prim
                (case-symbol->var
                  name
                  call-with-escape-continuation call-in-empty-context
                  thread-register set-thread-register!
                  panic set-panic-handler!
                  yield set-yield-handler! set-timer enable-interrupts disable-interrupts
                  procedure-metadata
                  record? make-record record-type-descriptor record-ref record-set!
                  string->bytevector bytevector->string
                  apply call-with-values values
                  eq? eqv? null? boolean? procedure? symbol? string? rational? integer? f32? f64?
                  pair? vector? mvector? bytevector? mbytevector?
                  string->symbol symbol->string
                  cons car cdr
                  vector vector-length vector-ref
                  make-mvector mvector->vector mvector-length mvector-ref mvector-set!
                  bytevector bytevector-length
                  bytevector-u8-ref bytevector-u16-ref bytevector-u32-ref bytevector-u64-ref
                  make-mbytevector mbytevector->bytevector mbytevector-length
                  mbytevector-u8-ref mbytevector-u16-ref mbytevector-u32-ref mbytevector-u64-ref
                  mbytevector-u8-set! mbytevector-u16-set! mbytevector-u32-set! mbytevector-u64-set!
                  bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
                  bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length integer-floor-divmod
                  numerator denominator cmp + - * /
                  f32->u32 u32->f32 f64->u64 u64->f64
                  f32->f64 f64->f32 f32->rational rational->f32 f64->rational rational->f64
                  f32-cmp f32-floor f32-ceiling f32-truncate f32-round f32+ f32- f32* f32/
                  f64-cmp f64-floor f64-ceiling f64-truncate f64-round f64+ f64- f64* f64/)))
          (lambda (renv) prim))))))

(define (ast-eval ast) ((ast-stage ast cenv.empty) renv.empty))

(define (ast-pretty ast)
  (let loop ((ast ast))
    (match ast
      (`#(quote ,_ ,value)         `(quote ,value))
      (`#(ref   ,_ ,address)       `(ref ,address))
      (`#(if    ,_ ,c ,t ,f)       `(if ,(loop c) ,(loop t) ,(loop f)))
      (`#(call  ,_ ,proc ,arg*)    `(call ,(loop proc) . ,(map loop arg*)))
      (`#(case-lambda ,_ ,cc*)     `(case-lambda
                                      . ,(map (lambda (cc)
                                                (list (case-lambda-clause-param cc)
                                                      (loop (case-lambda-clause-body cc))))
                                              cc*)))
      (`#(letrec ,_ ,bpair* ,body) `(letrec ,(map (lambda (bp) (list (binding-pair-left bp)
                                                                     (loop (binding-pair-right bp))))
                                                  bpair*)
                                      ,(loop body)))
      (`#(prim ,_ ,name)           `(prim ,name)))))
