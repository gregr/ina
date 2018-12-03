(define (alist-ref alist k)
  (cdr (or (assoc k alist) (error '"alist-ref of non-existent key:" k alist))))
(define (alist-ref* alist k*) (map (lambda (k) (alist-ref alist k)) k*))
(define (module-apply m env)
  (define imports (alist-ref* env (vector-ref m 0)))
  (map cons (vector-ref m 1) (apply (vector-ref m 2) imports)))
(define (link/module env m) (append (module-apply m env) env))
(define (link/module* env m*) (foldl (lambda (m e) (link/module e m)) env m*))

(define (parse/module body)
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
       (define required (map car rd)) (define required-private (map cadr rd))
       (define provided (map cadr pd)) (define provided-private (map car pd))
       (vector required provided required-private provided-private body)))))

(lambda (host)
  (define (capability name)
    (or (alist-get host name #f) (error '"missing capability:" name)))
  (define printf    (capability 'printf))
  (define read/file (capability 'read/file))
  (define eval      (capability 'eval))

  (define (eval/module body)
    (define parsed (parse/module body))
    ;; This code assumes 'list is not shadowed.  If this assumption is
    ;; violated, replace eval with fine-grained staging capabilities.
    (define code (append (list 'lambda (vector-ref parsed 2))
                         (append (vector-ref parsed 4)
                                 (list (cons 'list (vector-ref parsed 3))))))
    (vector (vector-ref parsed 0) (vector-ref parsed 1) (eval code)))
  (define (libmod name)
    (eval/module (read/file (string-append '"lib/nscheme/" name '".scm"))))

  (define modules
    '(common ast stage base eval base-test extended extended-test))
  (define env:nscheme (link/module* '() (map libmod modules)))

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
                                      env:nscheme))))
  (test-report)

  '#(symbol TODO:generate-nscheme.rkt))
