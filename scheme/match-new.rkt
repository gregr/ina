#lang racket/base
(provide
  match-new
  )

(module
  util2 racket/base
  (provide
    compile-match-lambda
    compile-pat
    )

  (module
    util1 racket/base
    (provide (all-defined-out))
    (require
      "syntax-new.rkt"
      "type.rkt"
      racket/match
      (for-template racket/base)
      )

    (define-variant-type
      pat?
      (pat-exist pat-exist? pat-exist-ids pat-exist-p)
      (pat-any pat-any?)
      (pat-var pat-var? pat-var-id)
      (pat-literal pat-literal? pat-literal-datum)

      (pat-cons pat-cons? pat-cons-car pat-cons-cdr)
      (pat-segment
        pat-segment? pat-segment-min-length pat-segment-p pat-segment-cdr)
      (pat-vector pat-vector? pat-vector-lp)
      (pat-syntax pat-syntax? pat-syntax-vlp)

      (pat-and pat-and? pat-and-c1 pat-and-c2)
      (pat-or pat-or? pat-or-d1 pat-or-d2)
      (pat-not pat-not? pat-not-p)

      (pat-? pat-?? pat-?-predicate)
      (pat-app pat-app? pat-app-transformer pat-app-p)
      )

    (define id-set-empty '())
    (define (set x) (list x))
    (define (id-set-empty? s) (null? s))
    (define (id-set-member? ys x) (ormap (lambda (y) (free-identifier=? x y)) ys))
    (define (id-set-union a b)
      (define (id-set-add x ys) (if (id-set-member? ys x) ys (cons x ys)))
      (foldl id-set-add b a))
    (define (id-set-subtract a b)
      (define (id-set-add x ys) (if (id-set-member? b x) ys (cons x ys)))
      (foldl id-set-add '() a))
    (define (list->set xs) (id-set-union xs '()))

    (define (bound-pattern-ids pat)
      (cond
        ((pat-exist? pat)
         (id-set-subtract (bound-pattern-ids (pat-exist-p pat)) (pat-exist-ids pat)))
        ((pat-var? pat) (set (pat-var-id pat)))
        ((pat-and? pat)     (id-set-union (bound-pattern-ids (pat-and-c1 pat))
                                          (bound-pattern-ids (pat-and-c2 pat))))
        ((pat-or? pat)      (id-set-union (bound-pattern-ids (pat-or-d1 pat))
                                          (bound-pattern-ids (pat-or-d2 pat))))
        ((pat-cons? pat)    (id-set-union (bound-pattern-ids (pat-cons-car pat))
                                          (bound-pattern-ids (pat-cons-cdr pat))))
        ((pat-segment? pat) (id-set-union (bound-pattern-ids (pat-segment-p pat))
                                          (bound-pattern-ids (pat-segment-cdr pat))))
        ((pat-vector? pat)  (bound-pattern-ids (pat-vector-lp pat)))
        ((pat-syntax? pat)  (bound-pattern-ids (pat-syntax-vlp pat)))
        ((pat-app? pat)     (bound-pattern-ids (pat-app-p pat)))
        (else id-set-empty)))

    (define (leaky-pattern-ids pat)
      (cond
        ((pat-exist? pat)
         (id-set-subtract (leaky-pattern-ids (pat-exist-p pat)) (pat-exist-ids pat)))
        ((pat-and? pat)
         (define l1 (leaky-pattern-ids (pat-and-c1 pat)))
         (define l2 (leaky-pattern-ids (pat-and-c2 pat)))
         (define g1 (id-set-subtract (bound-pattern-ids (pat-and-c1 pat)) l1))
         (define g2 (id-set-subtract (bound-pattern-ids (pat-and-c2 pat)) l2))
         (id-set-union (id-set-subtract l1 g2) (id-set-subtract l2 g1)))
        ((pat-or? pat)
         (define l1 (leaky-pattern-ids (pat-or-d1 pat)))
         (define l2 (leaky-pattern-ids (pat-or-d2 pat)))
         (define b1 (bound-pattern-ids (pat-or-d1 pat)))
         (define b2 (bound-pattern-ids (pat-or-d2 pat)))
         (id-set-union l1 (id-set-union l2 (id-set-union (id-set-subtract b1 b2)
                                                         (id-set-subtract b2 b1)))))
        ((pat-cons? pat)
         (define l1 (leaky-pattern-ids (pat-cons-car pat)))
         (define l2 (leaky-pattern-ids (pat-cons-cdr pat)))
         (define g1 (id-set-subtract (bound-pattern-ids (pat-cons-car pat)) l1))
         (define g2 (id-set-subtract (bound-pattern-ids (pat-cons-cdr pat)) l2))
         (id-set-union (id-set-subtract l1 g2) (id-set-subtract l2 g1)))
        ((pat-segment? pat)
         (define l1 (leaky-pattern-ids (pat-segment-p pat)))
         (define l2 (leaky-pattern-ids (pat-segment-cdr pat)))
         (define g1 (id-set-subtract (bound-pattern-ids (pat-segment-p pat)) l1))
         (define g2 (id-set-subtract (bound-pattern-ids (pat-segment-cdr pat)) l2))
         (id-set-union (id-set-subtract l1 g2) (id-set-subtract l2 g1)))
        ((pat-vector? pat) (leaky-pattern-ids (pat-vector-lp pat)))
        ((pat-syntax? pat) (leaky-pattern-ids (pat-syntax-vlp pat)))
        ((pat-app? pat)    (leaky-pattern-ids (pat-app-p pat)))
        (else id-set-empty)))

    (define (scopify-pat pat)
      (define (scopify pat bound)
        (cond
          ((pat-exist? pat)
           (pat-exist (pat-exist-ids pat)
                      (scopify (pat-exist-p pat)
                               (id-set-union bound (pat-exist-ids pat)))))
          ((pat-not? pat)
           (define nbound (id-set-subtract (bound-pattern-ids (pat-not-p pat)) bound))
           (define inner (scopify (pat-not-p pat) (id-set-union nbound bound)))
           (pat-not (if (id-set-empty? nbound) inner (pat-exist nbound inner))))
          ((pat-and? pat)    (pat-and (scopify (pat-and-c1 pat) bound)
                                      (scopify (pat-and-c2 pat) bound)))
          ((pat-or? pat)     (pat-or (scopify (pat-or-d1 pat) bound)
                                     (scopify (pat-or-d2 pat) bound)))
          ((pat-cons? pat)   (pat-cons (scopify (pat-cons-car pat) bound)
                                       (scopify (pat-cons-cdr pat) bound)))
          ((pat-cons? pat)   (pat-segment (pat-segment-min-length pat)
                                          (scopify (pat-segment-p pat) bound)
                                          (scopify (pat-segment-cdr pat) bound)))
          ((pat-vector? pat) (pat-vector (scopify (pat-vector-lp pat) bound)))
          ((pat-syntax? pat) (pat-syntax (scopify (pat-syntax-vlp pat) bound)))
          ((pat-app? pat)    (pat-app (pat-app-transformer pat)
                                      (scopify (pat-app-p pat) bound)))
          (else pat)))
      (scopify pat (bound-pattern-ids pat)))

    (define (simplify-pat pat)
      (cond
        ((pat-exist? pat)
         (pat-exist (pat-exist-ids pat) (simplify-pat (pat-exist-p pat))))
        ((pat-and? pat)
         (pat-and (simplify-pat (pat-and-c1 pat)) (simplify-pat (pat-and-c2 pat))))
        ((pat-or? pat)
         (pat-or (simplify-pat (pat-or-d1 pat)) (simplify-pat (pat-or-d2 pat))))
        ((pat-not? pat) (pat-not (simplify-pat (pat-not-p pat))))
        ((pat-cons? pat)
         (define pcar (simplify-pat (pat-cons-car pat)))
         (define pcdr (simplify-pat (pat-cons-cdr pat)))
         (if (and (pat-literal? pcar) (pat-literal? pcdr))
           (pat-literal (cons (pat-literal-datum pcar) (pat-literal-datum pcdr)))
           (pat-cons pcar pcdr)))
        ((pat-segment? pat) (pat-segment (pat-segment-min-length pat)
                                         (simplify-pat (pat-segment-p pat))
                                         (simplify-pat (pat-segment-cdr pat))))
        ((pat-vector? pat)
         (define lp (simplify-pat (pat-vector-lp pat)))
         (if (pat-literal? lp)
           (pat-literal (list->vector (pat-literal-datum lp)))
           (pat-vector lp)))
        ((pat-syntax? pat)
         (define vlp (simplify-pat (pat-syntax-vlp pat)))
         (if (pat-literal? vlp)
           (let ((datum (pat-literal-datum vlp)))
             (pat-literal
               (if (syntax-new? datum) datum (datum->syntax-new #f datum))))
           (pat-syntax vlp)))
        ((pat-app? pat)
         (pat-app (pat-app-transformer pat) (simplify-pat (pat-app-p pat))))
        (else pat)))

    (define ida-empty '())
    (define (ida-empty? ida) (null? ida))
    (define (ida-assoc ida id)
      (cond ((ida-empty? ida) #f)
            ((and (pair? (car ida)) (free-identifier=? id (caar ida))) (car ida))
            (else (ida-assoc (cdr ida) id))))
    (define (ida-set ida id value) (cons (cons id value) ida))
    (define (ida-forget ida ids)
      (cond ((ida-empty? ida) ida-empty)
            ((not (car ida)) (cdr ida))
            ((id-set-member? ids (caar ida)) (ida-forget (cdr ida) ids))
            (else (cons (car ida) (ida-forget (cdr ida) ids)))))

    (define (id-list id*) (map (lambda (i) #`#'#,i) id*))

    (define (equiv-data? a b)
      (or (eqv? a b)
          (and (pair? a) (pair? b)
               (equiv-data? (car a) (car b))
               (equiv-data? (cdr a) (cdr b)))
          (and (vector? a) (vector? b)
               (equiv-data? (vector->list a) (vector->list b)))
          (and (identifier-new? a) (identifier-new? b)
               (free-identifier-new=? a b))
          (and (syntax-new? a) (syntax-new? b)
               (equiv-data? (syntax-unwrap a) (syntax-unwrap b)))))

    (define (syntax->pat stx)
      (define (non-null-atom? datum)
        (or (eq? #t datum)
            (not datum)
            (char? datum)
            (string? datum)
            (number? datum)))

      (define (k-or-more? id)
        (define (valid-k? k-chars)
          (define k (and (andmap char-numeric? k-chars)
                         (string->number (list->string k-chars))))
          (and (integer? k) (<= 0 k) k))
        (and (identifier? id)
             (match (string->list (symbol->string (syntax->datum id)))
               ((list #\. #\. k ...) #:when (valid-k? k) (valid-k? k))
               ((list #\_ #\_ k ...) #:when (valid-k? k) (valid-k? k))
               (_ #f))))

      (define-syntax define-quasi->pat
        (syntax-rules ()
          ((_ qq-syntax->pat quasiqu qu unqu unqu-splicing)
           (define (qq-syntax->pat stx)
             (syntax-case stx (list qu unqu unqu-splicing)
               ((unqu p) (syntax->pat #'p))
               (((unqu-splicing (list p (... ...))) . qq)
                (syntax->pat #'(list* p (... ...) (quasiqu qq))))
               (((unqu-splicing (qu (d (... ...)))) . qq)
                (syntax->pat #'((unqu-splicing (list (qu d) (... ...))) . qq)))
               (((unqu-splicing p) . qq)
                (identifier? #'p)
                (syntax->pat #'(list* p ___ (quasiqu qq))))
               ((a . d)
                (not (and (identifier? #'a)
                          (or (free-identifier=? #'unqu #'a)
                              (free-identifier=? #'unqu-splicing #'a))))
                (syntax->pat #'(list* (quasiqu a) (quasiqu d))))
               (#(p (... ...)) (pat-vector (qq-syntax->pat #'(p (... ...)))))
               (datum (syntax->pat #'(qu datum))))))))

      (define-quasi->pat
        qq-syntax->pat quasiquote quote unquote unquote-splicing)
      (define-quasi->pat
        qs-syntax->pat quasisyntax syntax unsyntax unsyntax-splicing)

      (syntax-case stx (var exist ___
                            and or not ? app
                            cons list list* vector
                            quote quasiquote syntax quasisyntax)
        (_ (and (identifier? stx) (free-identifier=? #'_ stx)) pat-any)
        (id (identifier? #'id) (pat-var #'id))
        ((var id) (identifier? #'id) (pat-var #'id))
        ((exist (id ...) pat ...)
         (andmap identifier? (syntax->list #'(id ...)))
         (pat-exist (list->set (syntax->list #'(id ...)))
                    (syntax->pat #'(and pat ...))))

        ((list* p) (syntax->pat #'p))
        ((list* p0 ooo p ...)
         (and (identifier? #'ooo) (or (free-identifier=? #'ooo #'___)
                                      (free-identifier=? #'ooo #'(... ...))))
         (pat-segment 0 (syntax->pat #'p0) (syntax->pat #'(list* p ...))))
        ((list* p0 __k p ...)
         (k-or-more? #'__k)
         (pat-segment (k-or-more? #'__k) (syntax->pat #'p0)
                      (syntax->pat #'(list* p ...))))
        ((list* p0 p ...) (pat-cons (syntax->pat #'p0) (syntax->pat #'(list* p ...))))

        ((cons a d) (syntax->pat #'(list* a d)))
        ((list p ...) (syntax->pat #'(list* p ... '())))
        ((vector p ...) (pat-vector (syntax->pat #'(list p ...))))

        ((and p) (syntax->pat #'p))
        ((and p0 p ...) (pat-and (syntax->pat #'p0) (syntax->pat #'(and p ...))))
        ((or p) (syntax->pat #'p))
        ((or p0 p ...) (pat-or (syntax->pat #'p0) (syntax->pat #'(or p ...))))

        ((not p) (pat-not (syntax->pat #'p)))
        ((not p ...) (syntax->pat #'(not (or p ...))))

        ((? predicate) (pat-? #'predicate))
        ((? predicate p ...) (syntax->pat #'(and (? predicate) p ...)))
        ((app transformer p ...)
         (pat-app #'transformer (syntax->pat #'(and p ...))))

        ((quasiquote qq) (qq-syntax->pat #'qq))
        ((quasisyntax qs) (pat-syntax (qs-syntax->pat #'qs)))

        ((syntax datum) (pat-literal (racket-syntax->syntax-new #'datum)))
        ((quote datum) (pat-literal (syntax->datum #'datum)))
        (atom (non-null-atom? (syntax->datum #'atom))
              (pat-literal (syntax->datum #'atom)))))
    )

  (require
    'util1
    (for-template 'util1)
    (for-template racket/base)
    )

  (define (compile-pat pat k-succeed k-fail)
    (define (not-leaky pat)
      (define leaky (leaky-pattern-ids pat))
      (when (not (id-set-empty? leaky))
        (error "pattern doesn't always bind variables:" leaky)))
    (not-leaky pat)
    ;; TODO: pass env and value exprs to reduce administrative redexes?
    (let loop ((pat pat) (k-succeed k-succeed) (k-fail k-fail))
      (cond ((pat-exist? pat)
             #`(lambda (env value)
                 (#,(loop (pat-exist-p pat)
                          #`(lambda (env value)
                              (#,k-succeed
                               (ida-forget env (list #,@(id-list (pat-exist-ids pat))))
                               value))
                          k-fail)
                  (cons #f env) value)))
            ((pat-any? pat) k-succeed)
            ((pat-var? pat)
             (with-syntax
               (((k-s) (generate-temporaries #'(k-s))))
               #`(lambda (env value)
                   (define k-s #,k-succeed)
                   (define binding (ida-assoc env #'#,(pat-var-id pat)))
                   (if binding
                     ((if (equiv-data? (cdr binding) value)
                        k-s #,k-fail) env value)
                     (k-s (ida-set env #'#,(pat-var-id pat) value) value)))))
            ((pat-literal? pat)
             #`(lambda (env value)
                 ((if (equiv-data? '#,(pat-literal-datum pat) value)
                    #,k-succeed #,k-fail) env value)))

            ((pat-and? pat)
             (loop (pat-and-c1 pat) (loop (pat-and-c2 pat) k-succeed k-fail)
                   k-fail))
            ;; TODO: support full backtracking?
            ((pat-or? pat)
             (loop (pat-or-d1 pat) k-succeed
                   (loop (pat-or-d2 pat) k-succeed k-fail)))
            ;; TODO: propagate negations for not-yet-bound variables?
            ((pat-not? pat) (loop (pat-not-p pat) k-fail k-succeed))

            ((pat-cons? pat)
             (with-syntax
               (((k-s k-f k-cdr) (generate-temporaries #'(k-s k-f k-cdr))))
               #`(lambda (env value)
                   (define (k-s env _) (#,k-succeed env value))
                   (define (k-f env _) (#,k-fail env value))
                   (define (k-cdr env _) (#,(loop (pat-cons-cdr pat) #'k-s #'k-f)
                                          env (cdr value)))
                   (if (pair? value)
                     (#,(loop (pat-cons-car pat) #'k-cdr #'k-f) env (car value))
                     (k-f env value)))))
            ((pat-vector? pat)
             (with-syntax
               (((k-s k-f) (generate-temporaries #'(k-s k-f))))
               #`(lambda (env value)
                   (define (k-s env _) (#,k-succeed env value))
                   (define (k-f env _) (#,k-fail env value))
                   (if (vector? value)
                     (#,(loop (pat-vector-lp pat) #'k-s #'k-f)
                      env (vector->list value))
                     (k-f env value)))))
            ((pat-syntax? pat)
             (with-syntax
               (((k-s k-f) (generate-temporaries #'(k-s k-f))))
               #`(lambda (env value)
                   (define (k-s env _) (#,k-succeed env value))
                   (define (k-f env _) (#,k-fail env value))
                   (if (syntax-new? value)
                     (#,(loop (pat-syntax-vlp pat) #'k-s #'k-f)
                      env (syntax-unwrap value))
                     (k-f env value)))))

            ;; TODO: support full backtracking?
            ((pat-segment? pat)
             (define subpat (pat-segment-p pat))
             (not-leaky subpat)
             (define min-len (pat-segment-min-length pat))
             (with-syntax
               (((k-s k-f) (generate-temporaries #'(k-s k-f))))
               #`(lambda (env value)
                   (define bound (list #,@(id-list (bound-pattern-ids subpat))))
                   (define (k-s env) (#,k-succeed env value))
                   (define (k-f env _) (#,k-fail env value))
                   (define try-seg
                     #,(loop subpat #'(lambda (env _) env) #'(lambda _ #f)))
                   (define try-cdr
                     #,(loop (pat-segment-cdr pat)
                             #'(lambda (env _) env) #'(lambda _ #f)))
                   (let seg-loop ((value value) (seg* '()) (env* '()))
                     (cond
                       ;; greedily extend segment as far as possible
                       ((and (pair? value) (try-seg ida-empty (car value)))
                        => (lambda (env) (seg-loop (cdr value)
                                                   (cons (car value) seg*)
                                                   (cons env env*))))
                       (else
                         (let cdr-loop ((value value) (seg* seg*) (env* env*))
                           ;; each retry will be less greedy by one element
                           (define (retry) (if (pair? seg*)
                                             (cdr-loop (cons (car seg*) value)
                                                       (cdr seg*) (cdr env*))
                                             (k-f env value)))
                           (cond
                             ((> #,min-len (length seg*)) (k-f env value))
                             ((foldl
                                ;; zip sub envs and check consistency w/ main env
                                (lambda (bv* env)
                                  (define b (car bv*))
                                  (define binding (and env (ida-assoc env b)))
                                  (and env
                                       (if binding
                                         (and (equiv-data? (cdr binding)
                                                           (cdr bv*)) env)
                                         (ida-set env b (cdr bv*)))))
                                env
                                (apply map (lambda bv* bv*)
                                       (cons bound
                                             (map (lambda (e)
                                                    (map (lambda (b)
                                                           (cdr (ida-assoc e b)))
                                                         bound))
                                                  (reverse env*)))))
                              => (lambda (env)
                                   (cond ((try-cdr env value) => k-s)
                                         (else (retry)))))
                             (else (retry))))))))))

            ((pat-app? pat)
             #`(lambda (env value)
                 (#,(loop (pat-app-p pat) k-succeed k-fail) env
                  (#,(pat-app-transformer pat) value))))
            ((pat-?? pat)
             #`(lambda (env value)
                 ((if (#,(pat-?-predicate pat) value) #,k-succeed #,k-fail)
                  env value)))
            (else (error "invalid pattern:" pat)))))

  (define (compile-match-lambda alts)
    #`(lambda (value)
        (#,(for/fold
             ((k-fail #'(lambda (_ v) (error "no matching clause for:" v))))
             ((alt (reverse alts)))
             (syntax-case alt ()
               ;; TODO: support #:when and (=> fail).
               ((pattern body ...)
                (let ((pat (scopify-pat (simplify-pat (syntax->pat #'pattern)))))
                  (define bound (bound-pattern-ids pat))
                  (define k-succeed
                    #`(lambda (env _)
                        (let #,(map (lambda (b)
                                      #`(#,b (cdr (ida-assoc env #'#,b)))) bound)
                          body ...)))
                  #`(lambda (env value)
                      (#,(compile-pat pat k-succeed k-fail) ida-empty value))))))
         ida-empty value)))
  )

(require
  ;; Require these for debugging.
  ;(submod 'util2 util1)
  ;'util2 ;;(submod "." util2)  ;; This form seems to be the same as 'util2.
  (for-syntax 'util2)
  (for-syntax racket/base)
  )

(define-syntax (match-new stx)
  (syntax-case stx ()
    ((_ scrutinee clause ...)
     #`(#,(compile-match-lambda (syntax->list #'(clause ...)))
        scrutinee))))

;; TODO: named match for simple catamorphisms

(module+
  test (require rackunit)

  (define (test input)
    (match-new input
      (4 'four)
      (5 'five)
      (`(,a ,b) `(list-2-rev ,b ,a))
      ((list fst `(,k ,v) ___ almost k lst)
       `(segment: ,k ,v ,fst ,lst ,almost))
      ((list 1 repeat ___  5 repeat ___ remaining ___)
       `(repeat1: ,repeat remaining: ,remaining))
      ((list 1 repeat ___  5 `(,repeat) ___ remaining ___)
       `(repeat2: ,repeat remaining: ,remaining))
      (_ 'anything)))

  (check-equal? (test 4) 'four)
  (check-equal? (test 5) 'five)
  (check-equal? (test '(1 2)) '(list-2-rev 2 1))
  (check-equal? (test '(1 (2 3) (4 5) (6 7) 8 (2 4 6) 9))
                '(segment: (2 4 6) (3 5 7) 1 9 8))
  (check-equal? (test '(1 2 3 4 5 2 3 4 9 10))
                '(repeat1: (2 3 4) remaining: (9 10)))
  (check-equal? (test '(1 2 3 4 5 (2) (3) (4) 9 10))
                '(repeat2: (2 3 4) remaining: (9 10)))
  (check-equal? (test '(1 2 3 4 5 (2) (3) (5) 9 10))
                'anything)
  )
