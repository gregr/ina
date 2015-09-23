#lang racket/base
(provide
  )

(require
  "term.rkt"
  "unsafe1.rkt"
  racket/function
  )

(module+ test
  (require
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
                  (if (nil? xs) acc (foldl (f (head xs) acc) (tail xs))))))
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
                                 (if found (tail found) #f))))
    (syntactic? (lambda (env key)
                  (if (symbol? key) (let ((assignment (env-get env key)))
                                      (if assignment (head assignment) #f))
                    #f)))

    (apply (lambda (f xs) (foldl (lambda (arg f) (f arg)) f xs)))
    (eval (fix (lambda (eval env stx)
                 (if (cons? stx) (apply (eval env (head stx))
                                        (if (syntactic? env (head stx))
                                          (cons env (cons stx '()))
                                          (map (eval env) (tail stx))))
                   (if (symbol? stx) (tail (env-get env stx)) stx)))))

    ($lambda/syntax-type
      (lambda (syntax-type env tree)
        (((lambda (params body)
            (foldr (lambda (param body env arg)
                     (body (env-add env param syntax-type arg)))
                   (lambda (env) (eval env body))
                   params))
          (head tree)
          (head (tail tree)))
         env)))
    ($lambda  ($lambda/syntax-type #f))
    ($lambda$ ($lambda/syntax-type #t))
    )))

(define std1 (compose t-value (curry hash-ref std1-module)))

(module+ test
  )
