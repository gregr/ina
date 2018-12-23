(provide extended:stage extended:ast:values)

(require length=? length>=? env-get-prop env-update* ctx:op
         ast:var ast:quote ast:if
         ast:null ast:true ast:false ast:cons ast:apply* ast:let
         @or @body*
         binding:syntax/validation stage env:initial language base:stage)

(define keyword-bindings
  '((quasiquote       (keyword:quasiquote . quasiquote))
    (unquote          (keyword:quasiquote . unquote))
    (unquote-splicing (keyword:quasiquote . unquote-splicing))
    (else             (keyword:cond . else) (keyword:case . else))
    (=>               (keyword:cond . =>))))

(define (renamings->bindings:syntax renamings)
  (define (renamed name) (ast:var (or (alist-get renamings name #f)
                                      (error '"missing renaming for:" name))))
  (define $append               (renamed 'append))
  (define $list->vector         (renamed 'list->vector))
  (define $equal?               (renamed 'equal?))
  (define (ast:append xs ys)    (ast:apply* $append       (list xs ys)))
  (define (ast:list->vector xs) (ast:apply* $list->vector (list xs)))
  (define (ast:equal? a b)      (ast:apply* $equal?       (list a b)))

  (define (@quasiquote env template)
    (define (keyword? form)
      (and (string? form) (env-get-prop env form 'keyword:quasiquote #f)))
    (define (? tag d) (and (length=? 2 d) (equal? tag (keyword? (car d)))))
    (define (^ t e) (ast:cons (ast:quote t) (ast:cons e ast:null)))
    (let loop ((level 0) (qq template))
      (cond ((? 'quasiquote qq) (^ 'quasiquote (loop (+ level 1) (cadr qq))))
            ((? 'unquote qq)    (if (= 0 level) (stage env (cadr qq))
                                  (^ 'unquote (loop (- level 1) (cadr qq)))))
            ((and (pair? qq) (? 'unquote-splicing (car qq)))
             (define qqd (loop level (cdr qq)))
             (if (= 0 level) (ast:append (stage env (cadar qq)) qqd)
               (ast:cons (^ 'unquote-splicing (loop (- level 1) (cadar qq)))
                         qqd)))
            ((pair? qq) (ast:cons (loop level (car qq)) (loop level (cdr qq))))
            ((vector? qq)  (ast:list->vector (loop level (vector->list qq))))
            ((keyword? qq) (error '"malformed quasiquote:"
                                  (keyword? qq) qq template))
            (#t            (ast:quote qq)))))

  (define (@case env scrutinee . clauses)
    (define (keyword? form)
      (and (string? form) (env-get-prop env form 'keyword:case #f)))
    (define $x (make-mvector 1 'scrutinee))
    (define ast:x (ast:var $x))
    (ast:let
      (list $x) (list (stage env scrutinee))
      (foldr
        (lambda (c rest)
          (cond ((not (length>=? 2 c)) (error '"invalid case clause:" c))
                ((equal? (keyword? (car c)) 'else) (@body* env (cdr c)))
                ((not (list? (car c))) (error '"invalid case clause:" c))
                (#t (ast:if (foldr (lambda (d r)
                                     (ast:if (ast:equal? ast:x (ast:quote d))
                                             ast:true r)) ast:false (car c))
                            (@body* env (cdr c)) rest)))) ast:true clauses)))

  (define (@cond env . clauses)
    (define (keyword? form)
      (and (string? form) (env-get-prop env form 'keyword:cond #f)))
    (foldr (lambda (c rest)
             (unless (length>=? 1 c) (error '"invalid cond clause:" c))
             (cond ((null? (cdr c)) (@or env (car c) (lambda (_) rest)))
                   ((equal? (keyword? (car c)) 'else) (@body* env (cdr c)))
                   ((equal? (keyword? (cadr c)) '=>)
                    (unless (length=? 3 c) (error '"invalid cond clause:" c))
                    (define $x (make-mvector 1 'scrutinee))
                    (ast:let (list $x) (list (stage env (car c)))
                             (ast:apply* (stage env (caddr c))
                                         (list (ast:var $x)))))
                   (#t (ast:if (stage env (car c)) (@body* env (cdr c))
                               rest)))) ast:true clauses))

  (env-update*
    keyword-bindings
    (map (lambda (desc) (apply binding:syntax/validation (cons ctx:op desc)))
         (list (list 'quasiquote @quasiquote 1 #t)
               (list 'case       @case       1 #f)
               (list 'cond       @cond       0 #f)))))

(def #(extended:stage extended:ast:values)
     (language base:stage env:initial '()
               ;; Provide private bindings that users can't set!.
               '((let . ((append       append)
                         (list->vector list->vector)
                         (equal?       equal?))))
               renamings->bindings:syntax))