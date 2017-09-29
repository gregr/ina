(printf "\nNaive evaluate\n")
(load "eval-naive.scm")
(load "test-defs.scm")
(load "test.scm")

(printf "\nHigher order evaluate\n")
(load "eval-ho.scm")
(load "test-defs.scm")
(load "test.scm")

(printf "\nFirst order evaluate\n")
(load "eval-fo.scm")
(load "test-defs.scm")
(load "test.scm")

(printf "\neval\n")
(define (vector-reify v)
  (if (vector? v)
    v
    (error 'vector-reify (format "invalid vector ~s" v))))
(define (evaluate expr env) (eval expr))
(load "test-defs.scm")
(load "test.scm")

(printf "\nCompilation to JavaScript\n")
(load "compile-js.scm")
(define (vector-reify v) v)
(define (ev expr) `(,'unquote ,expr))
(define-syntax test
  (syntax-rules ()
    ((_ name expr expected-expr)
     (begin
       (define (assign vn rhs)
         (string-append "var " vn " = JSON.stringify(" rhs "());"))
       (let* ((expected expected-expr) (actual expr))
         (with-output-to-file
           (string-append "generated-test-" (symbol->string name) ".js")
           (lambda ()
             (write-string
               (string-append
                 (assign "actual"
                         (compile `(,'quasiquote ,actual) env-initial))
                 "\n"
                 (assign "expected"
                         (compile `(quote ,expected) env-initial))
                 "if(actual!==expected){console.log('FAIL','"
                 (symbol->string name)
                 "', '\\nACTUAL:',actual,'\\nEXPECTED:',expected);}")))))))))
(load "test.scm")
