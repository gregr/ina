((provide language:io module:io)
 (require primitive-op-type-signature primitive-op-handler
          ast:var ast:lambda ast:prim ast:let ast:list
          language module env:empty printf file-exists? eof?
          read read*/string read*/file write write/file))

(define io-op-descriptions
  (list (list 'printf       printf       '((string #f) boolean?))
        (list 'file-exists? file-exists? '((#f)        boolean?))
        (list 'eof?         eof?         '(()          boolean?))
        (list 'read         read         '(()          #f))
        (list 'read*/string read*/string '((string)    #f))
        (list 'read*/file   read*/file   '((#f)        boolean?))
        (list 'write        write        '((#f)        boolean?))
        (list 'write/file   write/file   '((#f #f)     boolean?))))

(define names       (map car io-op-descriptions))
(define language:io (language names '() names '() env:empty))
(define module:io
  (let* ((ls (map (lambda (po-desc)
                    (define (x i) (vector-ref '#(x0 x1 x2 x3 x4) i))
                    (define type-sig (primitive-op-type-signature po-desc))
                    (define p* (map x (range (length (car type-sig)))))
                    (ast:lambda p* (ast:prim (car po-desc) (map ast:var p*))))
                  io-op-descriptions))
         (ast (ast:lambda
                '() (ast:let names ls (apply ast:list (map ast:var names))))))
    (module '() names ast io-op-descriptions '())))
