#lang racket/base
(provide
  std1
  std1-module
  std1-applicatives
  std1-operatives
  unsafe2-program
  )

(require
  "linking.rkt"
  "term.rkt"
  "unsafe1.rkt"
  gregr-misc/sugar
  racket/function
  racket/match
  )

(module+ test
  (require
    "denotation.rkt"
    "parsing.rkt"
    rackunit
    ))

(define std1-module (unsafe1-module
  '(identity const compose fix
    boolean->bit pcons nil cons
    symbol? boolean? nil? cons? integer?
    symbol=? head tail
    integer=? integer<? integer<=? integer>? integer>=?
    (+ integer+) (*-1 integer-invert))
  `((bit0 (bit 0)) (bit1 (bit 1)) (unpair (lambda (bt pr) (unpair bt pr)))
    (fswap (lambda (f x y) (f y x)))

    (- (lambda (i0 i1) (+ i0 (*-1 i1))))

    (error (lambda (msg) (() msg)))  ; intentionally break

    (foldl (fix (lambda (foldl f acc xs)
                  (if (nil? xs) acc (foldl f (f (head xs) acc) (tail xs))))))
    (foldr (fix (lambda (foldr f acc xs)
                  (if (nil? xs) acc (f (head xs) (foldr f acc (tail xs)))))))
    (map (lambda (f) (foldr (compose cons f) '())))
    (reverse (foldl cons '()))
    (append (fswap (foldr cons)))
    (list-ref-tail (foldl (const tail)))
    (list-ref (lambda (xs index) (head (list-ref-tail xs index))))

    (assoc/? (lambda (match?)
               (fix (lambda (assoc key kvs)
                      (if (cons? kvs) (if (match? key (head (head kvs)))
                                        (head kvs) (assoc key (tail kvs)))
                        #f)))))

    (senv-empty '())
    (renv-empty '())
    (renv-add (fswap cons))
    (senv-add (lambda (senv key syntax? op)
                (let ((pos (cons () senv)))
                  (cons (cons key (cons (cons syntax? op) pos)) senv))))
    (senv-get (lambda (senv key) (let ((found (assoc/? symbol=? key senv)))
                                   (if (cons? found) (tail found) #f))))
    (senv-get/f (lambda (hit miss senv key)
                  (if (symbol? key)
                    (let ((assignment (senv-get senv key)))
                      (if (cons? assignment) (hit assignment) miss))
                    miss)))
    (sym->operative (senv-get/f head (cons #f ())))
    (sym->index (lambda (senv key)
                  (senv-get/f (compose (list-ref-tail senv) tail) #f
                              senv key)))

    (apply-to (lambda (arg proc) (proc arg)))
    (apply (foldl (lambda (arg f) (f arg))))
    (seval (fix (lambda (seval senv stx)
                  (if (cons? stx)
                    (let ((?op (sym->operative senv (head stx))))
                      (if (head ?op)
                        ((tail ?op) senv (tail stx))
                        (let ((pvs (map (seval senv) stx)))
                          (lambda (renv)
                            (let ((proc*args (map (apply-to renv) pvs)))
                              (apply (head proc*args) (tail proc*args)))))))
                    (if (symbol? stx)
                      (let ((index (sym->index senv stx)))
                        (if (boolean? index)
                          (error (cons 'undefined-identifier stx))
                          (lambda (renv) (list-ref renv index))))
                      (const (if (nil? stx) () stx)))))))

    (env-empty (cons senv-empty renv-empty))
    (env-add (lambda (env key syntax-type val)
               (let ((senv (head env)) (renv (tail env)))
                 (cons (senv-add senv key syntax-type (if syntax-type val ()))
                       (renv-add renv val)))))
    (eval (lambda (env stx) (seval (head env) stx (tail env))))

    ($lambda/syntax-type
      (lambda (syntax? senv stx)
        ((let* ((params (head stx))
                (body   (head (tail stx)))
                (build
                  (lambda (sbindings)
                    (let* ((senv (foldl (lambda (sb senv)
                                          (senv-add senv (head sb)
                                                    syntax? (tail sb)))
                                        senv sbindings))
                           (body (seval senv body)))
                      (foldr (lambda (_ body renv arg)
                               (body (renv-add renv arg))) body params)))))
           (if syntax?
             (lambda (renv)
               (foldr (lambda (param k rbindings rargs arg)
                        (k (cons (cons param arg) rbindings) (cons arg rargs)))
                      (lambda (rbs rargs)
                        (apply (build (reverse rbs) renv) (reverse rargs)))
                      params '() '()))
             (build (map (fswap cons ()) params)))))))
    ($lambda  ($lambda/syntax-type #f))
    ($lambda$ ($lambda/syntax-type #t))
    )))

(define std1 (compose t-value (curry hash-ref std1-module)))

(module+ test
  (define ((std1-apply stx . std1-idents) . args)
    (denote (build-apply
              (build-apply (unsafe1-parse stx) (map std1 std1-idents)) args)))

  (check-equal?
    ((std1-apply '(lambda (append)
                    (append '((a 1) (b 2)) '((c 3) (d 4)))) 'append))
    (denote (unsafe1-parse ''((a 1) (b 2) (c 3) (d 4)))))
  (check-equal?
    ((std1-apply '(lambda (map +) (map (+ 1) '(-2 -1 0))) 'map '+))
    (denote (unsafe1-parse ''(-1 0 1))))
  (check-equal?
    ((std1-apply '(lambda (apply +) (apply + '(-1 1))) 'apply '+))
    (denote (unsafe1-parse 0)))

  (check-equal?
    ((std1-apply '(lambda (eval env-empty env-add +)
                    ((eval (env-add env-empty 'i+ #f +) '(i+ 2)) 5))
                 'eval 'env-empty 'env-add '+))
    (denote (unsafe1-parse 7)))
  (check-equal?
    ((std1-apply '(lambda ($lambda senv-empty senv-add renv-empty renv-add +)
                    (($lambda (senv-add senv-empty 'i+ #f ())
                              '((i0 i1) (i+ (i+ i0 i1) 1)))
                     (renv-add renv-empty +) 3 2))
                 '$lambda 'senv-empty 'senv-add 'renv-empty 'renv-add '+))
    (denote (unsafe1-parse 6)))
  )

(define std1-applicatives (open-module std1
  '(bit0 bit1 (pair pcons) unpair boolean->bit
    identity const compose fix fswap
    nil cons symbol? boolean? nil? cons? integer? symbol=? head tail
    (=? integer=?) (<? integer<?) (<=? integer<=?)
    (>? integer>?) (>=? integer>=?) + *-1 -
    foldl foldr map reverse append assoc/?
    senv-empty senv-add renv-empty renv-add env-empty env-add apply-to apply
    seval eval
    )))
(define std1-operatives (open-module std1
  '((lambda $lambda) (lambda$ $lambda$))))

(define ((unsafe2-program (applicatives std1-applicatives)
                          (operatives std1-operatives)) body)
  (lets
    imports = (append applicatives operatives)
    typed-names = (append (forl (list name _) <- applicatives (list #f name))
                          (forl (list name _) <- operatives (list #t name)))
    env = (forf env = 'env-empty
                (list stype name) <- typed-names
                `(env-add ,env ',name ,stype ,name))
    ((link-program unsafe1-parse) imports `(eval ,env ',body))))

(define unsafe2-std1-program (unsafe2-program))

(module+ test
  (check-equal?
    (denote (unsafe2-std1-program '(pair (unpair bit1 (pair bit1 bit0))
                                         (unpair bit0 (pair bit1 bit0)))))
    '(0 . 1))
  (check-equal?
    (denote (unsafe2-std1-program '((lambda (x f) (f x)) (cons #f #t) head)))
    (denote (unsafe1-parse #f)))
  (check-equal?
    (denote (unsafe2-std1-program
              '((lambda$ (x f) (f x)) (cons #f #t) (lambda (_ t _) (head t)))))
    (denote (unsafe1-parse ''x)))
  )

; TODO:
; quasiquote/unquote/unquote-splicing,
; cond, match,
; and-map?, or-map?, datum->tag (tag as a symbol)
; match versions of let[rec][$][*], lambda[$]

(define unsafe2-std2-eval
  (unsafe2-std1-program
    '((lambda (let/binder let*/syntax-type fix* first second third)
        ((lambda$ (@ $ let let$ let* let$* quote if list list*)
           (let$* ((letrec (lambda (senv stx)
                             (let* ((defs (first stx))
                                    (body (second stx))
                                    (names (map (lambda (def)
                                                  (head (first def))) defs))
                                    (body-proc
                                      (@ lambda senv (list names body)))
                                    (procs-raw
                                      (map (lambda (def)
                                             (@ lambda senv
                                                (list
                                                  (append
                                                    names (tail (first def)))
                                                  (second def)))) defs)))
                               (lambda (renv)
                                 (apply (body-proc renv)
                                        (fix* (map (apply-to renv)
                                                   procs-raw)))))))
                   (env-current (lambda (senv _ renv) (cons senv renv))))

             (let* ((filter (lambda (keep? xs)
                              (foldr (lambda (x ys)
                                       (if (keep? x) (cons x ys) ys)) '() xs)))
                    (not? (lambda (b) (if b #f #t)))
                    (and? (lambda (a b) (if a (if b #t #f) #f)))
                    (or?  (lambda (a b) (if a (if b #t #t) (if b #t #f))))
                    (boolean=? (lambda (lhs rhs) (if lhs rhs (not? rhs))))
                    (equal?
                      (fix (lambda (equal? lhs rhs)
                             (if (symbol? lhs)
                               (if (symbol? rhs) (symbol=? lhs rhs) #f)
                               (if (boolean? lhs)
                                 (if (boolean? rhs) (boolean=? lhs rhs) #f)
                                 (if (nil? lhs) (nil? rhs)
                                   (if (cons? lhs)
                                     (if (cons? rhs)
                                       (and? (equal? (head lhs) (head rhs))
                                             (equal? (tail lhs) (tail rhs)))
                                       #f)
                                     (if (integer? lhs)
                                       (if (integer? rhs) (=? lhs rhs) #f)
                                       #f))))))))
                    (assoc (assoc/? equal?)))
               (let$* ((and (lambda (senv stx)
                              ((lambda (sargs renv)
                                 ((fix (lambda (self prev sargs)
                                         (if (nil? sargs) prev
                                           (if (equal? #f prev) #f
                                             (self ((head sargs) renv)
                                                   (tail sargs))))))
                                  #t sargs))
                               (map (seval senv) stx))))
                       (or (lambda (senv stx)
                             ((lambda (sargs renv)
                                ((fix (lambda (self prev sargs)
                                        (if (nil? sargs) prev
                                          (if (equal? #f prev)
                                            (self ((head sargs) renv)
                                                  (tail sargs))
                                            prev))))
                                 #f sargs))
                              (map (seval senv) stx)))))
                      (eval (env-current))))))
         ; @
         (lambda (senv stx)
           ((lambda (sproc sargs renv)
              (apply (sproc renv) (map (apply-to renv) sargs)))
            (seval senv (head stx)) (map (seval senv) (tail stx))))
         ; $
         (lambda (senv stx)
           ((lambda (sproc renv) ((sproc renv) senv (tail stx)))
            (seval senv (head stx))))
         (let/binder lambda) ; let
         (let/binder lambda$) ; let$
         (let*/syntax-type #f) ; let*
         (let*/syntax-type #t) ; let$*
         (lambda (_ stx) (const (head stx))) ; quote
         ; if
         (lambda (senv stx)
           ((lambda (condition consequent alternative renv)
              ((unpair (boolean->bit (condition renv))
                       (pair consequent alternative)) renv))
            (seval senv (first stx))
            (seval senv (second stx))
            (seval senv (third stx))))
         ; list
         (lambda (senv stx) ((lambda (sargs renv) (map (apply-to renv) sargs))
                             (map (seval senv) stx)))
         ; list*
         (lambda (senv stx)
           ((lambda (rsargs renv)
              (foldl (lambda (sarg args) (cons (sarg renv) args))
                     ((head rsargs) renv) (tail rsargs)))
            (reverse (map (seval senv) stx))))
         ))
      ; let/binder
      (lambda (binder senv stx)
        ((lambda (params args body)
           ((lambda (sproc sargs renv)
              (apply (sproc renv) (map (apply-to renv) sargs)))
            (binder senv (cons params (cons body nil)))
            (map (seval senv) args)))
         (map head (head stx))
         (map (compose head tail) (head stx))
         (head (tail stx))))
      ; let*/syntax-type
      (lambda (syntax-type senv stx)
        ((lambda (sapply slam sev xxbindings xxbody)
           (foldr (lambda (xxbinding xbody)
                    (sapply (slam (head xxbinding) xbody)
                            (sev (head (tail xxbinding)))))
                  (sev xxbody) xxbindings senv))
         (lambda (xproc xarg senv)
           ((lambda (sproc sarg renv) ((sproc renv) (sarg renv)))
            (xproc senv) (xarg senv)))
         (unpair (boolean->bit syntax-type)
                 (pair
                   (lambda (param xbody senv renv arg)
                     (xbody (senv-add senv param #t arg) (renv-add renv arg)))
                   (lambda (param xbody senv)
                     ((lambda (sbody renv arg) (sbody (renv-add renv arg)))
                      (xbody (senv-add senv param #f ()))))))
         (fswap seval) (head stx) (head (tail stx))))
      ; fix*
      (fix (lambda (self ps)
             (map (lambda (pi x) ((apply pi (self ps)) x)) ps)))
      head ; first
      (compose head tail) ; second
      (compose (compose head tail) tail) ; third
      )))

(module+ test
  (define unsafe2-std2-eval-denoted (denote unsafe2-std2-eval))
  (define (unsafe2-std2-denote body)
    (unsafe2-std2-eval-denoted (denote (unsafe1-parse (list 'quote body)))))
  (check-equal?
    (unsafe2-std2-denote '(head '(a b)))
    (denote (unsafe1-parse ''a)))
  (check-equal?
    (unsafe2-std2-denote '(if (head (cons #t #f))
                            (if (tail (cons #t #f)) 'a 'b) 'c))
    (denote (unsafe1-parse ''b)))
  (check-equal?
    (unsafe2-std2-denote '(third (list* 'a 'b '(c d))))
    (denote (unsafe1-parse ''c)))
  (check-equal?
    (unsafe2-std2-denote
      '(letrec (((even? n) (if (=? 0 n) #t (odd? (- n 1))))
                ((odd? n) (if (=? 0 n) #f (even? (- n 1)))))
         (list (even? 3) (odd? 3))))
    (denote (unsafe1-parse ''(#f #t))))
  (check-equal?
    (unsafe2-std2-denote
      '(and 2 ()))
    (denote (unsafe1-parse '())))
  (check-equal?
    (unsafe2-std2-denote
      '(and 2 #f () ()))
    (denote (unsafe1-parse #f)))
  (check-equal?
    (unsafe2-std2-denote
      '(or 2 () ()))
    (denote (unsafe1-parse 2)))
  (check-equal?
    (unsafe2-std2-denote
      '(or #f ()))
    (denote (unsafe1-parse '())))
  (check-equal?
    (unsafe2-std2-denote
      '(tail (assoc '(a (() (#f . 1))) '(((a (() (#t . 1))) . one)
                                         ((a (() (#f . 1))) . two)
                                         ((a (() (#f . 1))) . three)))))
    (denote (unsafe1-parse ''two)))
  )
