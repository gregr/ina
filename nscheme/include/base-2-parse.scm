(define (parse-begin-meta-definition dst env.scope env stx)
  (let* ((dst      ((definition-operator-parser parse-begin-definition 0 #f)
                    dst env.scope env stx))
         (def*     (defstate-definitions dst))
         (bpair*   (definitions->binding-pairs def*))
         (assign!* (definitions->assigners def*))
         (addr*    (map binding-pair-lhs bpair*)))
    (for-each (lambda (assign! result) (assign! result))
              assign!* (ast-eval (ast:letrec (syntax-provenance stx) bpair*
                                             ($begin ((defstate-expression dst))
                                                     (apply $list (map $ref addr*)))))))
  dst)

(define (parse-begin-meta-expression env stx)
  ($quote (ast-eval ((expression-operator-parser parse-begin-expression 1 #f) env stx))))

(define env.base-2
  (let ((env.scope (make-env))
        (b*.def-and-expr
          (list
            (list 'begin-meta parse-begin-meta-definition parse-begin-meta-expression))))
    (for-each (lambda (id op.def op.expr)
                (let ((addr (identifier->fresh-address id)))
                  (env-bind! env.scope id addr)
                  (env-set!  env.scope vocab.definition-operator addr op.def)
                  (env-set!  env.scope vocab.expression-operator addr op.expr)))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (env-extend env.base-0 env.scope)))
