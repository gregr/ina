#lang racket/base
(provide (all-from-out 'interop) (all-from-out 'common)
         interop-eval racket-eval)

(module interop racket/base
  (provide
    local-path library-path s->ns ns->s
    read/port read/file write/port write/file racket-datum
    lift lower lower-arg0 $apply
    mvector? make-mvector mvector=? mvector-length mvector-ref mvector-set!
    mvector->vector string->vector vector->string procedure=? number=?
    alist-get alist-remove* alist-at list-at
    nscm-quote nscm-quasiquote
    (rename-out (equal? nscm-equal?) (member nscm-member) (assoc nscm-assoc)))

  (require racket/path racket/port racket/vector (for-syntax racket/base))

  (define (local-path rpath)
    (build-path (path-only (path->complete-path (find-system-path 'run-file)))
                rpath))
  (define (library-path library-name module-name)
    (local-path
      (build-path "lib" (symbol->string library-name)
                  (string-append (symbol->string module-name) ".scm"))))

  (define (s->ns d)
    (cond ((symbol? d) (symbol->string d))
          ((pair? d) (cons (s->ns (car d)) (s->ns (cdr d))))
          ((vector? d) (vector-map s->ns d))
          ((or (char? d) (keyword? d)) (error "invalid nscheme datum:" d))
          (else d)))

  (define (ns->s d)
    (cond ((string? d) (string->symbol d))
          ((pair? d) (cons (ns->s (car d)) (ns->s (cdr d))))
          ((vector? d) (vector-map ns->s d))
          (else d)))

  (define (ns->s/write d)
    (cond ((string? d) (string->symbol d))
          ((pair? d) (cons (ns->s (car d)) (ns->s (cdr d))))
          ((vector? d) (vector-map ns->s d))
          ((or (boolean? d) (null? d) (number? d)) d)
          (else (error "cannot write:" d))))

  (define (read/port in)
    (let loop ((rbody '()))
      (define datum (ns->s (read in)))
      (if (eof-object? datum) (reverse rbody) (loop (cons datum rbody)))))
  (define (read/file path)    (call-with-input-file path read/port))
  (define (write/port out d)  (write (ns->s/write d) out))
  (define (write/file path d) (call-with-output-file path write/port))

  (define (racket-datum form)
    (define (? tag) (equal? (vector-ref form 0) tag))
    (cond ((pair? form) (cons (racket-datum (car form))
                              (racket-datum (cdr form))))
          ((not (vector? form)) form)
          ((? "quote")   (vector-ref form 1))
          ((? "vector")  (vector-map racket-datum (vector-ref form 1)))
          ((? "symbol")  (string->symbol (vector-ref form 1)))
          ((? "keyword") (string->keyword (vector-ref form 1)))
          ((? "char")    (call-with-input-string
                           (string-append "#\\" (vector-ref form 1))
                           (lambda (in) (read in))))
          (#t (error "invalid racket-datum form:" form))))

  ;; For safe interop, provided definitions must not override Racket names.
  (define (equal? a b)
    (or (eqv? a b)
        (and (string? a) (string? b) (string=? a b))
        (and (pair? a) (pair? b)
             (equal? (car a) (car b))
             (equal? (cdr a) (cdr b)))
        (and (vector? a) (vector? b)
             (equal? (vector->list a) (vector->list b)))))
  (define (member v xs) (memf (lambda (x) (equal? x v)) xs))
  (define (assoc k xs) (cond ((null? xs) #f)
                             ((equal? k (caar xs)) (car xs))
                             (else (assoc k (cdr xs)))))

  (define (lift racket-proc)   (lambda (a) (apply racket-proc a)))
  (define (lower nscheme-proc) (lambda a   (nscheme-proc a)))
  (define (lower-arg0 proc)    (lambda (f . args) (apply proc (lower f) args)))
  (define ($apply proc arg . args)
    (define (cons* x xs) (if (null? xs) x (cons x (cons* (car xs) (cdr xs)))))
    (proc (cons* arg args)))

  (struct mvector (v) #:transparent)
  (define (make-mvector k d)          (mvector (make-vector k d)))
  (define (mvector=? m n)             (eq? m n))
  (define (mvector-length mv)         (vector-length (mvector-v mv)))
  (define (mvector-ref mv i)          (vector-ref (mvector-v mv) i))
  (define (mvector-set! mv i new)     (vector-set! (mvector-v mv) i new) #t)
  ;; TODO: update Racket to use this.
  ;(define (mvector-cas! mv i old new) (vector-cas! (mvector-v mv) i old new))
  (define (mvector->vector mv)        (vector-copy (mvector-v mv)))
  (define (string->vector s)
    (list->vector (map char->integer (string->list s))))
  (define (vector->string v)
    (list->string (map integer->char (vector->list v))))
  (define (procedure=? m n) (eq? m n))
  (define (number=? m n)    (eqv? m n))

  (define (alist-get rs key default)
    (define rib (assoc key rs))
    (if rib (cdr rib) default))
  (define (alist-remove* rs keys)
    (filter (lambda (rib) (not (member (car rib) keys))) rs))
  (define (list-at xs ?)  ;; produce a zipper referencing the desired location
    (let loop ((suffix xs) (prefix '()))
      (if (or (null? suffix) (? (car suffix))) (cons suffix prefix)
        (loop (cdr suffix) (cons (car suffix) prefix)))))
  (define (alist-at rs key) (list-at rs (lambda (kv) (equal? (car kv) key))))

  (define-syntax (nscm-quote stx)
    (syntax-case stx ()
      ((_ id)       (identifier? #'id)
                    #`(nscm-quote #,(symbol->string (syntax->datum #'id))))
      ((_ (a . d))  #'(cons (nscm-quote a) (nscm-quote d)))
      ((_ #(d ...)) #'(vector (nscm-quote d) ...))
      ((_ d)        #'(quote d))))
  (define-syntax nscm-quasiquote (syntax-rules () ((_ d) (nscm-qq () d))))
  (define-syntax nscm-qq
    (syntax-rules (nscm-quasiquote unquote unquote-splicing)
      ((_ lvl (nscm-quasiquote d))
       (list (nscm-quote quasiquote)       (nscm-qq (s . lvl) d)))
      ((_ (s . p) (unquote e))
       (list (nscm-quote unquote)          (nscm-qq p e)))
      ((_ (s . p) (unquote-splicing e))
       (list (nscm-quote unquote-splicing) (nscm-qq p (unquote-splicing e))))
      ((_ () (unquote e))                e)
      ((_ () ((unquote-splicing e) . d)) (append e (nscm-qq () d)))
      ((_ lvl unquote)          (error "invalid unquote"))
      ((_ lvl unquote-splicing) (error "invalid unquote-splicing"))
      ((_ lvl (a . d))          (cons (nscm-qq lvl a) (nscm-qq lvl d)))
      ((_ lvl #(d ...))         (list->vector (nscm-qq lvl (d ...))))
      ((_ lvl d)                (nscm-quote d)))))

(module common racket/base
  (provide length=? length>=? param?! bpair*?!
           ncons param-map param-names param-bind
           ctx:var ctx:set! ctx:op ctx:def
           env:empty env-ref env-get-prop env-remove* env-add* env-extend*
           defstate:empty defstate-env defstate-names defstate-actions
           defstate-env-set defstate-names-add defstate-actions-add)
  (require (submod ".." interop))

  ;; Pattern matching
  (define (length=? n xs)  (and (list? xs) (= (length xs) n)))
  (define (length>=? n xs) (and (list? xs) (>= (length xs) n)))
  (define (param?! param) (unless (andmap string? (param-names param))
                            (error '"invalid parameters:" param)))
  (define (bpair*?! b*)
    (define (? b) (and (length=? 2 b) (param?! (car b))))
    (unless (and (list? b*) (andmap ? b*))
      (error '"invalid binding list:" b*)))

  ;; Formal parameters
  (define (ncons name names)
    (when (member name names) (error '"duplicate name:" name names))
    (cons name names))
  (define (param-map f p)
    (cond ((pair? p)   (cons (param-map f (car p)) (param-map f (cdr p))))
          ((vector? p) (list->vector (param-map f (vector->list p))))
          ((null? p)   '())
          ((not p)     #f)
          (#t          (f p))))
  (define (param-names param)
    (let loop ((p param) (ns '()))
      (cond ((pair? p)   (loop (cdr p) (loop (car p) ns)))
            ((vector? p) (loop (vector->list p) ns))
            ((null? p)   ns)
            ((not p)     ns)
            (#t          (ncons p ns)))))
  (define (param-bind param arg)
    (let loop ((p param) (a arg))
      (cond ((and (pair? p) (pair? a)) (append (loop (car p) (car a))
                                               (loop (cdr p) (cdr a))))
            ((and (vector? p) (vector? a))
             (loop (vector->list p) (vector->list a)))
            ((and (null? p) (null? a)) '())
            ((not p)                   '())
            ((not (or (pair? p) (vector? p) (null? p))) (list (cons p a)))
            (#t (error '"parameter/argument mismatch:" param arg p a)))))

  (define ctx:var  '"ref")
  (define ctx:set! '"set!")
  (define ctx:op   '"syntax?")
  (define ctx:def  '"define")
  (define env:empty                      '())
  (define (env-ref env n)                (alist-get env n '()))
  (define (env-get-prop env n k default) (alist-get (env-ref env n) k default))
  (define (env-remove* env n*)           (alist-remove* env n*))
  (define (env-add* env b*)              (append b* env))
  (define (env-extend* env b*) (env-add* (env-remove* env (map car b*)) b*))

  (define (defstate:empty env)  (vector env '() '()))
  (define (defstate-env st)     (vector-ref st 0))
  (define (defstate-names st)   (vector-ref st 1))
  (define (defstate-actions st) (vector-ref st 2))
  (define (defstate-env-set st env)
    (vector env (defstate-names st) (defstate-actions st)))
  (define (defstate-names-add st names)
    (define new (foldl ncons (defstate-names st) names))
    (vector (defstate-env st) new (defstate-actions st)))
  (define (defstate-actions-add st act)
    (define new (cons act (defstate-actions st)))
    (vector (defstate-env st) (defstate-names st) new)))

(require racket/runtime-path 'interop 'common)
(define-runtime-module-path interop-mod (submod "." interop))

(define (interop-eval form)
  (define local-ns (current-namespace))
  (parameterize ((current-namespace (make-base-namespace)))
    (namespace-attach-module local-ns interop-mod)
    (namespace-require/constant interop-mod)
    (namespace-set-variable-value! 'interop-eval interop-eval)
    (namespace-set-variable-value! 'racket-eval racket-eval)
    (eval form)))

(define (racket-eval rkt-datum) (interop-eval (racket-datum rkt-datum)))
