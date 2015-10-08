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
    true false cons
    symbol? boolean? nil? cons? integer?
    symbol=? head tail
    integer=? integer<? integer<=? integer>? integer>=?
    (+ integer+) (*-1 integer-invert))
  `((- (lambda (i0 i1) (+ i0 (*-1 i1))))

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
  '(identity const compose fix
    true false cons symbol? boolean? nil? cons? integer? symbol=? head tail
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
