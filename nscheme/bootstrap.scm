;; TODO: modules should be able to require the entire env/alist as a single value?  Maybe not.

;; TODO: link/program; how should programs be constructed? require but not provide? optional provide?

(define module-utilities
  '((define (alist-ref alist k)
      (cdr (or (assoc k alist)
               (error '"alist-ref of non-existent key:" k alist))))
    (define (alist-ref* alist k*) (map (lambda (k) (alist-ref alist k)) k*))
    (define (module-apply m env)
      (apply (vector-ref m 2) (alist-ref* env (vector-ref m 0))))
    (define (module-apply-provide m env)
      (map cons (vector-ref m 1) (module-apply m env)))
    (define (link/module env m) (append (module-apply-provide m env) env))
    (define (link/module* env m*)
      (foldl (lambda (m e) (link/module e m)) env m*))

    (define (module:spec body)
      (define (i->r items rrns)
        (foldl (lambda (item rrns)
                 (if (and (pair? item) (equal? (car item) 'rename))
                   (append (reverse (cdr item)) rrns)
                   (cons (list item item) rrns))) rrns items))
      (let loop ((body body) (rrequired '()) (rprovided '()))
        (define next (and (pair? body) (pair? (car body)) (car body)))
        (cond
          ((equal? (car next) 'require)
           (loop (cdr body) (i->r (cdr next) rrequired) rprovided))
          ((equal? (car next) 'provide)
           (loop (cdr body) rrequired (i->r (cdr next) rprovided)))
          (#t (define rd (reverse rrequired)) (define pd (reverse rprovided))
           (define required (map car rd)) (define required-priv (map cadr rd))
           (define provided (map cadr pd)) (define provided-priv (map car pd))
           (vector required provided required-priv provided-priv body)))))))

(define nscheme.scm:main
  '(
    ;; TODO: more efficient to use existing defs.  How do we arrange this?
    ;(define base:values (list <insert existing base:names here>))
    (define base:values (ast-eval base:ast:values))

    (define (module:ast:unlinked body)
      (define mspec (module:spec body))
      (define ($list #f) (stage env:initial '(lambda xs xs)))
      (define code (append (list 'lambda (vector-ref mspec 2))
                           (append (vector-ref mspec 4)
                                   (list (cons $list (vector-ref mspec 3))))))
      (vector (vector-ref mspec 0) (vector-ref mspec 1) (base:stage code)))

    (define (module:ast/module:ast:unlinked staged)
      (vector (vector-ref staged 0) (vector-ref staged 1)
              (ast:apply (vector-ref staged 2) base:ast:values)))

    (define (module/module:ast:unlinked staged)
      (vector (vector-ref staged 0) (vector-ref staged 1)
              (apply (ast-eval (vector-ref staged 2)) base:values)))

    (define (module body)
      (module/module:ast:unlinked (module:ast:unlinked body)))
    ))

;; TODO: construct nscheme.scm programmatically.

;; TODO: lambda that gets all provisions from: ast stage base backend-racket

(lambda (host)
  (define (capability name)
    (or (alist-get host name #f) (error '"missing capability:" name)))
  (define printf     (capability 'printf))
  (define read*/file (capability 'read*/file))
  (define eval       (capability 'eval))

  ;; This $list only works with bootstrap.rkt eval.
  (define ($list . #f) (lambda env (lambda xs xs)))
  (define imports '(module-apply module-apply-provide link/module* module:spec))
  (def (module-apply module-apply-provide link/module* module:spec)
       (eval (append '(let ()) module-utilities (list (cons $list imports)))))
  (define (module body)
    (define mspec (module:spec body))
    (define code (append (list 'lambda (vector-ref mspec 2))
                         (append (vector-ref mspec 4)
                                 (list (cons $list (vector-ref mspec 3))))))
    (vector (vector-ref mspec 0) (vector-ref mspec 1) (eval code)))
  (define (libmod name)
    (module (read*/file (string-append '"lib/" name '".scm"))))

  (define modules      '(common ast stage base eval extended backend-racket))
  (define test-modules '(base-test extended-test))
  (define env:nscheme/tests (link/module* '() (map libmod modules)))
  (define env:test (link/module* env:nscheme/tests (map libmod test-modules)))

  (define tests-total 0)
  (define test-failures '())

  (define (test-report)
    (define tests-failed (length test-failures))
    (define tests-passed (- tests-total tests-failed))
    (printf '"********************************\nTests passed: ~a out of ~a\n"
            tests-passed tests-total)
    (unless (= tests-passed tests-total)
      (printf '"Tests failed: ~a out of ~a\n" tests-failed tests-total)
      (printf '"~s\n" test-failures)))

  (define (test name actual expected)
    (printf '"Testing ~a: " name)
    (set! tests-total (+ tests-total 1))
    (cond ((equal? expected actual) (printf '"Succeeded.\n"))
          (#t (printf '"Failed.\nExpected: ~s\nActual: ~s\n****************\n"
                      expected actual)
           (set! test-failures (cons name test-failures)))))

  (for-each (lambda (t) (t test))
            (reverse (map cdr (filter (lambda (rib) (equal? 'test! (car rib)))
                                      env:test))))
  (test-report)

  (define env:nscheme
    (filter-not (lambda (r) (equal? (car r) 'test!)) env:nscheme/tests))
  (define names:nscheme (map car env:nscheme))

  ;; TODO: add some other names, like capabilities?
  (define nscheme.scm
    (list* 'lambda names:nscheme (append module-utilities nscheme.scm:main)))

  '#(symbol TODO:generate-nscheme.rkt))
