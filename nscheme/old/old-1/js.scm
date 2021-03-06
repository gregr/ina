;; WARNING: This is not a fully abstract code generator.  Assume insecurity by
;; default.  These operations do very little sanity checking and validation.

(define (js-stmt* stx*) (map js-stmt stx*))
(define (js-stmt stx)
  (match stx
    (`(vars . ,(? list? binding*))
      (string-append "var " (comma* (js-binding* binding*)) ";"))
    (`(return ,expr) (string-append "return " (js-expr expr) ";"))
    (`(throw ,expr) (string-append "throw " (js-expr expr) ";"))
    (`(break ,label) (string-append "break " (js-label label) ";"))
    (`(continue ,label) (string-append "continue " (js-label label) ";"))
    (`(break) "break;")
    (`(continue) "continue;")
    (`(if/else ,test ,(? list? true*) ,(? list? false*))
      (string-append "if(" (js-expr test) "){"
                     (adjacent* (js-stmt* true*)) "}else{"
                     (adjacent* (js-stmt* false*)) "}"))
    (`(for ,pre ,test ,iter ,(? list? stmt*))
      (string-append "for("(js-stmt pre) ";" (js-expr test) ";"
                     (js-expr iter) "){" (adjacent* (js-stmt* stmt*)) "}"))
    (`(for/in ,(? symbol? iname) ,obj ,(? list? stmt*))
      (string-append "for(var " (js-var iname) " in " (js-expr obj)
                     "){" (adjacent* (js-stmt* stmt*)) "}"))
    (`(while ,test ,(? list? stmt*))
      (string-append "while(" (js-expr test) "){"
                     (adjacent* (js-stmt* stmt*)) "}"))
    (`(do/while ,(? list? stmt*) ,test)
      (string-append "do{" (adjacent* (js-stmt* stmt*))
                     "}while(" (js-expr test) ");"))
    (`(switch ,scrutinee ,(? list? case*))
      (string-append "switch(" (js-expr scrutinee) "){"
                     (adjacent* (js-case* case*)) "}"))
    (`(try/catch/finally ,(? list? try*) ,(? symbol? ename)
                         ,(? list? catch*) ,(? list? finally*))
      (string-append "try{" (adjacent* (js-stmt* try*))
                     "}catch(" (js-var ename) "){"
                     (adjacent* (js-stmt* catch*)) "}finally{"
                     (adjacent* (js-stmt* finally*)) "}"))
    (`(labeled ,(and (not #f) label) stmt)
      (string-append (js-label label) ":" (js-stmt stmt)))
    (`(block ,(? list? stmt*))
      (string-append "{;" (adjacent* (js-stmt* stmt*)) "}"))
    ('debugger "debugger;")
    (_ (string-append (js-expr stx) ";"))))

(define (js-binding* binding*) (map js-binding binding*))
(define (js-binding binding)
  (match binding
    ((? symbol?) (js-var binding))
    (`(,(? symbol? name) ,rhs)
      (string-append (js-var name) "=" (js-expr rhs)))))

(define (js-case* case*) (map (lambda (cstx) (js-case cstx)) case*))
(define (js-case case-stx)
  (match case-stx
    (`(case ,val ,(? list? stmt*))
      (string-append "case " (js-expr val) ":{"
                     (adjacent* (js-stmt* stmt*)) "}"))
    (`(default ,(? list? stmt*))
      (string-append "default:{" (adjacent* (js-stmt* stmt*)) "}"))))

(define (js-label label) (if label (symbol->string label) ""))

(define (adjacent* s*) (string-join s* ""))
(define (comma* s*) (string-join s* ","))

(define (js-var stx) (symbol->string stx))
(define (js-number stx)
  (number->string (if (integer? stx) stx (exact->inexact stx))))
(define (js-string stx) (with-output-to-string (lambda () (write stx))))
(define (js-regexp pattern flags)
  (string-append "/" (string-replace pattern "/" "\\/") "/" flags))

(define (js-kv* kv*) (map (lambda (kv) (js-kv kv)) kv*))
(define (js-kv kv)
  (match kv
    (`(,(? string? key) ,value)
      (string-append (js-string key) ":" (js-expr value)))))

(define (js-unop name expr)
  (string-append "(" name (js-expr expr) ")"))
(define (js-binop name ea eb)
  (string-append "(" (js-expr ea) name (js-expr eb) ")"))

(define (js-ref/lhs lhs stx)
  (match stx
    ((? symbol?) (js-var stx))
    (`(get ,ref ,index) (string-append (lhs ref) "[" (js-expr index) "]"))))
(define (js-ref stx) (js-ref/lhs js-ref stx))
(define (js-put op lhs rhs)
  (string-append "(" (js-ref lhs) op (js-expr rhs) ")"))

(define (sym->ident* param*) (map symbol->string param*))

(define (js-expr* stx*) (map (lambda (stx) (js-expr stx)) stx*))
(define (js-expr stx)
  (match stx
    ((? number?) (js-number stx))
    ((? string?) (js-string stx))
    (#t "true")
    (#f "false")
    ('null "null")
    ('undefined "undefined")
    (`(regexp ,pattern ,flags) (js-regexp pattern flags))
    (`(array . ,(? list? element*))
      (string-append "[" (comma* (js-expr* element*)) "]"))
    (`(object . ,(? list? kv*))
      (string-append "{" (comma* (js-kv* kv*)) "}"))
    (`(function ,(? list? param*) ,(? list? body*))
      (string-append "(function(" (comma* (sym->ident* param*))
                     "){\"use strict\";" (adjacent* (js-stmt* body*)) "})"))
    (`(app ,rator ,(? list? rand*))
      (string-append (js-expr rator) "(" (comma* (js-expr* rand*)) ")"))
    (`(?: ,test ,true ,false)
      (string-append (js-expr test) "?" (js-expr true) ":" (js-expr false)))
    (`(put ,lhs ,rhs) (js-put "=" lhs rhs))
    (`(put+ ,lhs ,rhs) (js-put "+=" lhs rhs))
    (`(put- ,lhs ,rhs) (js-put "-=" lhs rhs))
    (`(put* ,lhs ,rhs) (js-put "*=" lhs rhs))
    (`(put/ ,lhs ,rhs) (js-put "/=" lhs rhs))
    (`(put% ,lhs ,rhs) (js-put "%=" lhs rhs))
    (`(prefix++ ,lhs) (string-append "(++" (js-ref lhs) ")"))
    (`(prefix-- ,lhs) (string-append "(--" (js-ref lhs) ")"))
    (`(postfix++ ,lhs) (string-append "(" (js-ref lhs) "++)"))
    (`(postfix-- ,lhs) (string-append "(" (js-ref lhs) "--)"))
    (`(typeof ,expr) (string-append "(typeof " (js-expr expr) ")"))
    (`(instanceof ,instance ,class) (js-binop " instanceof " instance class))
    (`(in ,ea ,eb) (js-binop " in " ea eb))
    (`(- ,expr) (js-unop "-" expr))
    (`(- ,ea ,eb) (js-binop "-" ea eb))
    (`(+ ,ea ,eb) (js-binop "+" ea eb))
    (`(* ,ea ,eb) (js-binop "*" ea eb))
    (`(/ ,ea ,eb) (js-binop "/" ea eb))
    (`(% ,ea ,eb) (js-binop "%" ea eb))
    (`(bneg ,expr) (js-unop "~" expr))
    (`(band ,ea ,eb) (js-binop "&" ea eb))
    (`(bor ,ea ,eb) (js-binop "|" ea eb))
    (`(bxor ,ea ,eb) (js-binop "^" ea eb))
    (`(bshl ,ea ,eb) (js-binop "<<" ea eb))
    (`(bshrs ,ea ,eb) (js-binop ">>" ea eb))
    (`(bshru ,ea ,eb) (js-binop ">>>" ea eb))
    (`(lneg ,expr) (js-unop "!" expr))
    (`(land ,ea ,eb) (js-binop "&&" ea eb))
    (`(lor ,ea ,eb) (js-binop "||" ea eb))
    (`(=== ,ea ,eb) (js-binop "===" ea eb))
    (`(== ,ea ,eb) (js-binop "==" ea eb))
    (`(!== ,ea ,eb) (js-binop "!==" ea eb))
    (`(!= ,ea ,eb) (js-binop "!=" ea eb))
    (`(> ,ea ,eb) (js-binop ">" ea eb))
    (`(>= ,ea ,eb) (js-binop ">=" ea eb))
    (`(< ,ea ,eb) (js-binop "<" ea eb))
    (`(<= ,ea ,eb) (js-binop "<=" ea eb))
    (`(comma ,ea ,eb) (js-binop "," ea eb))
    (`(delete ,expr) (js-unop "delete " expr))
    (`(new ,expr) (js-unop "new " expr))
    (_ (js-ref/lhs js-expr stx))))
