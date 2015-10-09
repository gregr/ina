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
    boolean->bit pcons cons
    symbol? boolean? nil? cons? integer?
    symbol=? head tail
    integer=? integer<? integer<=? integer>? integer>=?
    (+ integer+) (*-1 integer-invert))
  `((unit ()) (bit0 (bit 0)) (bit1 (bit 1)) (unpair (lambda (bt pr) (unpair bt pr)))

    (- (lambda (i0 i1) (+ i0 (*-1 i1))))

    (foldl (fix (lambda (foldl f acc xs)
                  (if (nil? xs) acc (foldl f (f (head xs) acc) (tail xs))))))
    (foldr (fix (lambda (foldr f acc xs)
                  (if (nil? xs) acc (f (head xs) (foldr f acc (tail xs)))))))
    (map    (lambda (f xs) (foldr (compose cons f) '() xs)))
    (append (lambda (xs ys) (foldr cons ys xs)))

    (assoc/? (lambda (match?)
               (fix (lambda (assoc key kvs)
                      (if (cons? kvs) (if (match? key (head (head kvs)))
                                        (head kvs) (assoc key (tail kvs)))
                        #f)))))
    (env-empty '())
    (env-add (lambda (env key syntax-type val)
               (cons (cons key (cons syntax-type val)) env)))
    (env-get (lambda (env key) (let ((found (assoc/? symbol=? key env)))
                                 (if (cons? found) (tail found) #f))))
    (syntactic? (lambda (env key)
                  (if (symbol? key)
                    (let ((assignment (env-get env key)))
                      (if (cons? assignment) (head assignment) #f))
                    #f)))

    (apply (foldl (lambda (arg f) (f arg))))
    (eval (fix (lambda (eval env stx)
                 (if (cons? stx) (apply (eval env (head stx))
                                        (if (syntactic? env (head stx))
                                          (cons env (cons (tail stx) '()))
                                          (map (eval env) (tail stx))))
                   (if (symbol? stx) (tail (env-get env stx)) stx)))))

    ($lambda/syntax-type
      (lambda (syntax-type env stx)
        ((let ((params (head stx))
               (body   (head (tail stx))))
            (foldr (lambda (param body env arg)
                     (body (env-add env param syntax-type arg)))
                   (lambda (env) (eval env body))
                   params))
         env)))
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
    ((std1-apply '(lambda ($lambda env-empty env-add +)
                    (($lambda (env-add env-empty 'i+ #f +)
                              '((i0 i1) (i+ (i+ i0 i1) 1)))
                     3 2))
                 '$lambda 'env-empty 'env-add '+))
    (denote (unsafe1-parse 6)))
  )

(define std1-applicatives (open-module std1
  '(unit bit0 bit1 (pair pcons) unpair boolean->bit identity const compose fix
    cons symbol? boolean? nil? cons? integer? symbol=? head tail
    (=? integer=?) (<? integer<?) (<=? integer<=?)
    (>? integer>?) (>=? integer>=?) + *-1 -
    foldl foldr map append
    assoc/? env-empty env-add env-get syntactic? apply eval
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
    (denote (unsafe2-std1-program
              '((lambda (x f) (f x)) (cons #f #t) head)))
    (denote (unsafe1-parse #f)))
  (check-equal?
    (denote (unsafe2-std1-program
              '((lambda$ (x f) (f x)) (cons #f #t)
                                      (lambda (_ t) (head t)))))
    (denote (unsafe1-parse ''x)))
  )

; TODO:
; quasiquote/unquote/unquote-splicing,
; cond, match,
; and-map?, or-map?, datum->tag (tag as a symbol)
; equalities?
; match versions of let[rec][$][*], lambda[$]

(define (unsafe2-std2-program body)
  (unsafe2-std1-program
    `((lambda (let/binder let*/syntax-type fix* first second third reverse)
        ((lambda$ (@ $ let let$ let* let$* quote if list list*)
           (let$* ((letrec (lambda (env stx)
                             (let* ((defs (first stx))
                                    (body (second stx))
                                    (names (map (lambda (def)
                                                  (head (first def))) defs))
                                    (procs-raw
                                      (map (lambda (def)
                                             (@ lambda env
                                                (list
                                                  (append
                                                    names (tail (first def)))
                                                  (second def)))) defs))
                                    (procs-final (fix* procs-raw)))
                               (apply (@ lambda env (list names body))
                                      procs-final)))))
             (let* ((filter (lambda (keep? xs)
                              (foldr (lambda (x ys)
                                       (if (keep? x) (cons x ys) ys)) () xs)))
                    (not? (lambda (b) (if b #f #t)))
                    (and? (lambda (a b) (if a (if b #t #f) #f)))
                    (or?  (lambda (a b) (if a (if b #t #t) (if b #t #f)))))
               ,body)))
         ; @
         (lambda (env stx)
           (apply (eval env (head stx)) (map (eval env) (tail stx))))
         (lambda (env stx) ((eval env (head stx)) env (tail stx))) ; $
         (let/binder lambda) ; let
         (let/binder lambda$) ; let$
         (let*/syntax-type #f) ; let*
         (let*/syntax-type #t) ; let$*
         (lambda (_ stx) (head stx)) ; quote
         ; if
         (lambda (env stx)
           ((unpair (boolean->bit (eval env (first stx)))
                    (pair (lambda (_) (eval env (second stx)))
                          (lambda (_) (eval env (third stx))))) unit))
         ; list
         (lambda (env stx) (map (eval env) stx))
         ; list*
         (lambda (env stx)
           ((lambda (rargs) (foldl cons (head rargs) (tail rargs)))
            (reverse (map (eval env) stx))))
         ))
      ; let/binder
      (lambda (binder env stx)
        ((lambda (params args body)
           (apply (binder env (cons params (cons body ())))
                  (map (eval env) args)))
         (map head (head stx))
         (map (compose head tail) (head stx))
         (head (tail stx))))
      ; let*/syntax-type
      (lambda (syntax-type env stx)
        (eval (foldl (lambda (binding env)
                       (env-add env (head binding) syntax-type
                                (eval env (head (tail binding)))))
                     env (head stx))
              (head (tail stx))))
      ; fix*
      (fix (lambda (self ps)
             (map (lambda (pi x) ((apply pi (self ps)) x)) ps)))
      head ; first
      (compose head tail) ; second
      (compose (compose head tail) tail) ; third
      (foldl cons ()) ; reverse
      )))

(module+ test
  (check-equal?
    (denote (unsafe2-std2-program '(head '(a b))))
    (denote (unsafe1-parse ''a)))
  (check-equal?
    (denote (unsafe2-std2-program '(if (head (cons #t #f))
                                     (if (tail (cons #t #f)) 'a 'b) 'c)))
    (denote (unsafe1-parse ''b)))
  (check-equal?
    (denote (unsafe2-std2-program '(third (list* 'a 'b '(c d)))))
    (denote (unsafe1-parse ''c)))
  (check-equal?
    (denote (unsafe2-std2-program
              '(letrec (((even? n) (if (=? 0 n) #t (odd? (- n 1))))
                        ((odd? n) (if (=? 0 n) #f (even? (- n 1)))))
                 (list (even? 3) (odd? 3)))))
    (denote (unsafe1-parse ''(#f #t))))
  )
