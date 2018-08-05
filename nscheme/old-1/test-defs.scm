(define-syntax test
  (syntax-rules ()
    ((_ name expr expected-expr)
     (begin
       (printf "Testing ~s: " name)
       (let* ((expected expected-expr) (actual expr))
         (if (equal? expected actual)
           (printf "Succeeded.\n")
           (printf "\nFailed: ~a\nExpected: ~a\nActual: ~a\n"
                   'expr expected actual)))))))

(define (ev expr) (evaluate expr env-initial))
