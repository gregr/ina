#lang racket/base
(require
  "interop.rkt"
  "module.rkt"
  ;"stage.rkt"
  racket/include
  racket/list
  racket/pretty
  racket/runtime-path
  racket/string
  )

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

(define (library-modules library-name module-names)
  (define (libmod module-name)
    (eval/module (read/file (library-path library-name module-name))))
  (map libmod module-names))
(define env:data
  (link/module*
    '() (library-modules 'data '(box tagged symbol assoc compare))))
(define env:nscheme
  (link/module*
    env:data (library-modules
               'nscheme '(common ast stage eval base-test backend-racket))))

;; Work in progress towards self-hosting.
;(call-with-output-file
  ;"racket-output.rkt"
  ;(lambda (out)
    ;(define prelude
      ;(string-join
        ;'("#lang racket"
          ;"(define (lift racket-proc) (lambda (a) (apply racket-proc a)))"
          ;"(define (procedure=? m n)   (eq? m n))"
          ;"(define (number=? m n)      (eqv? m n))"
          ;"(struct mvector (v) #:transparent)"
          ;"(define (make-mvector k d)      (mvector (make-vector k d)))"
          ;"(define (mvector=? m n)         (eq? m n))"
          ;"(define (mvector-length mv)     (vector-length (mvector-v mv)))"
          ;"(define (mvector-ref mv i)      (vector-ref (mvector-v mv) i))"
          ;"(define (mvector-set! mv i new) (vector-set! (mvector-v mv) i new) #t)"
          ;"(define (mvector->vector mv)    (vector-copy (mvector-v mv)))"
          ;"(define (string->vector s)"
          ;"  (list->vector (map char->integer (string->list s))))"
          ;"(define (vector->string v)"
          ;"  (list->string (map integer->char (vector->list v))))"
          ;"(define ((param-error p* a*) p a)"
          ;"  (error \"parameter/argument mismatch:\" p a p* a*))")
        ;'"\n"))

    ;(define (base:stage ast) ((cdr (assoc 'base:stage env:nscheme)) (list ast)))
    ;(define (ast->racket ast) ((cdr (assoc 'ast->racket env:nscheme)) (list ast)))

    ;;(define code
    ;;(map (lambda (v) (pretty-format v 200 #:mode (ns->s 'display)))
    ;;(ns->s (ast->racket
    ;;(stage env:primitive
    ;;(s->ns '((lambda (f) (f 5)) (lambda (x) x))))))))
    ;;(displayln `(racket: ,(string-join code (s->ns '"\n"))))

    ;;(define mod (parse/module (read/file (library-path 'nscheme 'ast))))
    ;;(define mod (parse/module (read/file (library-path 'nscheme 'stage))))

    ;(define staged-mod (stage/module (read/file (library-path 'nscheme 'stage))))
    ;(define ast (vector-ref staged-mod 2))
    ;;(define ast (base:stage (s->ns `(lambda ,(vector-ref mod 2) ,@(vector-ref mod 4)

    ;;))))

    ;;(define code
      ;;(map (lambda (v) (pretty-format v 200 #:mode (ns->s 'write)))
           ;;(ns->s (time (ast->racket ast)))))
    ;(define code
      ;(pretty-format (ns->s (time (ast->racket ast))) 200 #:mode (ns->s 'write)))

    ;;(define (test-ast->racket filename form)
    ;;(define ast (base:stage form))
    ;;(define racket (ast->racket ast))
    ;;(call-with-output-file filename (lambda (out) (displayln racket out))))


    ;;(displayln (string-join code (s->ns '"\n")) out)

    ;(displayln prelude out)
    ;(displayln code out)
    ;))
;(displayln `(racket: ,(string-join code (s->ns '"\n"))))


(for-each (lambda (t) (time (t (list (lift test)))))
          (reverse (map cdr (filter (lambda (rib) (eq? 'test! (car rib)))
                                    env:nscheme))))
(test-report)
