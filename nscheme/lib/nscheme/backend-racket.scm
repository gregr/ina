(provide ast->racket)

(require param-map param-names)

(define (natural->string/digits digits n)
  (define base (vector-length digits))
  (if (= n 0) (vector-ref digits 0)
    (let loop ((n n))
      (if (= n 0) '""
        (let ((quotient (truncate (/ n base))))
          (string-append (loop quotient)
                         (vector-ref digits (- n (* base quotient)))))))))

(define (natural->string n)
  (natural->string/digits '#("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") n))

(define (list-join separator xs)
  (if (null? xs) '()
    (cons (car xs)
          (apply append (map (lambda (x) (list separator x)) (cdr xs))))))

;; TODO: escape properly.
(define (write-string s)
  (define content (map (lambda (c) (let ((s (vector->string (vector c))))
                                     (if (equal? s '"|") '"|\\||" s)))
                       (vector->list (string->vector s))))
  (cons '"|" (append content (list '"|"))))
(define (write/string d)
  (cond ((list? d) (cons '"(" (append (list-join
                                        '" " (apply append
                                                    (map write/string d)))
                                      (list '")"))))
        ((pair? d) (cons '"(" (append (write/string (car d)) (list '" . ")
                                      (write/string (cdr d)) (list '")"))))
        ((vector? d) (define ss (apply append
                                       (map write/string (vector->list d))))
                     (cons '"#(" (append (list-join '" " ss) (list '")"))))
        ;; TODO: support writing other numbers.
        ((and (integer? d) (<= 0 d)) (list (natural->string d)))
        ((string? d)   (write-string d))
        ((null? d)     (list '"()"))
        ((not d)       (list '"#f"))
        ((equal? d #t) (list '"#t"))
        (#t (error '"cannot write:" d))))

(define prelude
  (list-join
    '"\n"
    '("#lang racket"
      "(define (plift racket-proc) (lambda (a) (apply racket-proc a)))"
      "(define (procedure=? m n)   (eq? m n))"
      "(define (number=? m n)      (eqv? m n))"
      "(struct mvector (v) #:transparent)"
      "(define (make-mvector k d)      (mvector (make-vector k d)))"
      "(define (mvector=? m n)         (eq? m n))"
      "(define (mvector-length mv)     (vector-length (mvector-v mv)))"
      "(define (mvector-ref mv i)      (vector-ref (mvector-v mv) i))"
      "(define (mvector-set! mv i new) (vector-set! (mvector-v mv) i new) #t)"
      "(define (mvector->vector mv)    (vector-copy (mvector-v mv)))"
      "(define (string->vector s)"
      "  (list->vector (map char->integer (string->list s))))"
      "(define (vector->string v)"
      "  (list->string (map integer->char (vector->list v))))"
      "(define ((param-error p* a*) p a)"
      "  (error \"parameter/argument mismatch:\" p a p* a*)))")))

(define (ast->racket ast)
  (define name->var
    (let ((i 0))
      (lambda (n)
        (define name (mvector-ref n 0))
        (define index (natural->string i))
        (unless (pair? name)
          (mvector-set! n 0 (cons (string-append 'v index '|.| name) name))
          (set! i (+ i 1)))
        (car (mvector-ref n 0)))))
  (define (param-set! param)
    (define p* (param-map (lambda (p) (name->var p) (cdr (mvector-ref p 0)))
                          param))
    (list 'let (list (list 'set!.fail
                           (list 'param-error (list 'quote p*) 'set!.arg)))
          (let loop ((p param))
            (cond
              ((pair? p)
               (list 'and '(or (pair? set!.arg) (set!.fail 'pair? set!.arg))
                     (list 'let '((set!.arg (car set!.arg))) (loop (car p)))
                     (list 'let '((set!.arg (cdr set!.arg))) (loop (cdr p)))))
              ((vector? p)
               (list 'and '(or (vector? set!.arg) (set!.fail 'vector? set!.arg))
                     (list 'let '((set!.arg (vector->list set!.arg)))
                           (loop (vector->list p)))))
              ((null? p) '(or (null? set!.arg) (set!.fail 'null? set!.arg)))
              ((not p)   #t)
              (#t (list 'set! (name->var p) 'set!.arg))))))
  (define code
    (let ev ((ast ast))
      (define (@ i) (vector-ref ast i)) (define (? tag) (equal? (@ 0) tag))
      (if (procedure? ast) ast
        (cond ((? 'quote)  (let ((datum (@ 1))) (list 'quote datum)))
              ((? 'var)    (let ((n (@ 1)))     (name->var n)))
              ((? 'set!)   (let ((param (@ 1)) (arg (ev (@ 2))))
                             (list 'let (list (list 'set!.arg arg))
                                   (param-set! param))))
              ((? 'if)     (let ((c (ev (@ 1))) (t (ev (@ 2))) (f (ev (@ 3))))
                             (list 'if c t f)))
              ((? 'apply)  (let ((proc (ev (@ 1))) (arg (ev (@ 2))))
                             (list proc arg)))
              ((? 'lambda) (let ((param (@ 1)) (body (ev (@ 2))))
                             (list 'lambda '(set!.arg)
                                   (list 'let (map (lambda (n)
                                                     (list (name->var n) #t))
                                                   (param-names param))
                                         (param-set! param) body))))
              ((? 'reset)  (let ((body (ev (@ 1)))) (list 'reset body)))
              ((? 'shift)  (let ((proc (ev (@ 1))))
                             (list 'shift 'k (list proc '(plift k)))))
              ((? 'prim)   (let ((name (@ 1)) (a* (map ev (@ 2))))
                             (cons name a*)))
              (#t          (error '"unknown ast:" ast))))))
  code
  ;(apply string-append (append prelude (write/string code)))
  )
