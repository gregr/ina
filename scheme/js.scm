;; Misc
;; publicly visible (global?) names (and symbol->name table for linking)
;; bind used globals to local variables
;; inline single-use temp vars?
;; debugger;
;; new, this, call/apply
;; minification
;; JSON subset
;; Node.js has global,require instead of window

;; TODO: support weakly-unique object keys.

(define (js-stmt* ctx stx*) (map (lambda (stx) (js-stmt ctx stx)) stx*))
(define (js-stmt ctx stx)
  ;; TODO: need to produce an updated ctx.
  (match stx
    (`(vars ,(? list? binding*))
      (string-append "var " (comma* (js-binding* ctx binding*)) ";"))
    (`(return ,expr) (string-append "return " (js-expr ctx expr) ";"))
    (`(throw ,expr) (string-append "throw " (js-expr ctx expr) ";"))
    (`(break ,label) (string-append "break " (js-label ctx label) ";"))
    (`(continue ,label) (string-append "continue " (js-label ctx label) ";"))
    (`(if/else ,test ,(? list? true*) ,(? list? false*))
      (string-append "if(" (js-expr ctx test) "){"
                     (adjacent* (js-stmt* ctx true*)) "}else{"
                     (adjacent* (js-stmt* ctx false*)) "}"))
    (`(for ,pre ,test ,iter ,(? list? stmt*))
      ;; TODO: update ctx with pre.
      (string-append "for("(js-stmt ctx pre) ";" (js-expr ctx test) ";"
                     (js-expr ctx iter) "){"
                     (adjacent* (js-stmt* ctx stmt*)) "}"))
    (`(for/in ,(? symbol? iname) ,obj ,(? list? stmt*))
      ;; TODO: update ctx with iname.
      (string-append "for(var " (js-ref ctx iname) " in " (js-expr ctx obj)
                     "){" (adjacent* (js-stmt* ctx stmt*)) "}"))
    (`(while ,test ,(? list? stmt*))
      (string-append "while(" (js-expr ctx test) "){"
                     (adjacent* (js-stmt* ctx stmt*)) "}"))
    (`(do/while ,(? list? stmt*) ,test)
      (string-append "do{" (adjacent* (js-stmt* ctx stmt*))
                     "}while(" (js-expr ctx test) ");"))
    (`(switch ,scrutinee ,(? list? case*))
      (string-append "switch(" (js-expr ctx scrutinee) "){"
                     (adjacent* (js-case* ctx case*)) "}"))
    (`(try/catch/finally ,(? list? try*) ,(? symbol? ename)
                         ,(? list? catch*) ,(? list? finally*))
      ;; TODO: update ctx with ename.
      (string-append "try{" (adjacent* (js-stmt* ctx try*))
                     "}catch(" (js-ref ctx ename) "){"
                     (adjacent* (js-stmt* ctx catch*)) "}finally{"
                     (adjacent* (js-stmt* ctx finally*)) "}"))
    (`(labeled ,(and (not #f) label) stmt)
      ;; TODO: incorporate label into ctx.
      (string-append (js-label ctx label) ":" (js-stmt ctx stmt)))
    (`(block ,(? list? stmt*))
      (string-append "{;" (adjacent* (js-stmt* ctx stmt*)) "}"))
    (_ (string-append (js-expr ctx stx) ";"))))

(define (js-binding* ctx binding*)
  (map (lambda (b) (js-binding ctx b)) binding*))
(define (js-binding ctx binding)
  ;; TODO: update ctx.
  (match binding
    ((? symbol?) (js-var ctx binding))
    (`(,(? symbol? name) ,rhs)
      (string-append (js-var ctx name) "=" (js-expr ctx rhs)))))

(define (js-case* ctx case*) (map (lambda (cstx) (js-case ctx cstx)) case*))
(define (js-case ctx case-stx)
  (match case-stx
    (`(case ,val ,(? list? stmt*))
      (string-append "case " (js-expr ctx val) ":{"
                     (adjacent* (js-stmt* ctx stmt*)) "}"))
    (`(default ,(? list? stmt*))
      (string-append "default:{" (adjacent* (js-stmt* ctx stmt*)) "}"))))

;; TODO: refer to label table.
(define (js-label ctx label) (if label (symbol->string label) ""))

(define (adjacent* s*) (string-join s* ""))
(define (comma* s*) (string-join s* ","))

;; TODO: refer to global symbol table.
(define (js-var ctx stx) (symbol->string stx))
;; TODO: convert or validate double.
(define (js-number stx) (number->string stx))
;; TODO: escape properly.
(define (js-string stx) (string-append "\"" stx "\""))

(define (js-kv* ctx kv*) (map (lambda (kv) (js-kv ctx kv)) kv*))
(define (js-kv ctx kv)
  ;; TODO: indicate arbitrary field names using symbols.
  (match kv
    (`(,(? string? key) ,value)
      (string-append (js-string key) ":" (js-expr ctx value)))))

(define (js-unop ctx name expr)
  (string-append "(" name (js-expr ctx expr) ")"))
(define (js-binop ctx name ea eb)
  (string-append "(" (js-expr ctx ea) name (js-expr ctx eb) ")"))

(define (js-ref/lhs lhs ctx stx)
  (match stx
    ((? symbol?) (js-var ctx stx))
    (`(index ,ref ,index)
      (string-append (lhs ctx ref) "[" (js-expr ctx index) "]"))))
(define (js-ref ctx stx) (js-ref/lhs js-ref ctx stx))
(define (js-put op ctx lhs rhs)
  (string-append "(" (js-ref ctx lhs) op (js-expr ctx rhs) ")"))

;; TODO: manage ctx.
(define (sym->ident* ctx param*) (map symbol->string param*))

(define (js-expr* ctx stx*) (map (lambda (stx) (js-expr ctx stx)) stx*))
(define (js-expr ctx stx)
  ;; TODO: regular expressions?
  (match stx
    ((? number?) (js-number stx))
    ((? string?) (js-string stx))
    (#t "true")
    (#t "false")
    ('null "null")
    ('undefined "undefined")
    (`(array ,(? list? element*))
      (string-append "[" (comma* (js-expr* ctx element*)) "]"))
    (`(object ,(? list? kv*))
      (string-append "{" (comma* (js-kv* ctx kv*)) "}"))
    (`(function ,(? list? param*) ,(? list? body*))
      (string-append "function(" (comma* (sym->ident* ctx param*))
                     "){\"use strict\";" (adjacent* (js-stmt* ctx body*)) "}"))
    (`(app ,rator ,(? list? rand*))
      (string-append
        (js-expr ctx rator) "(" (comma* (js-expr* ctx rand*)) ")"))
    (`(?: ,test ,true ,false)
      (string-append
        (js-expr ctx test) "?" (js-expr ctx true) ":" (js-expr ctx false)))
    (`(get ,ref) (js-ref/lhs js-expr ctx ref))
    (`(put ,lhs ,rhs) (js-put "=" ctx lhs rhs))
    (`(put+ ,lhs ,rhs) (js-put "+=" ctx lhs rhs))
    (`(put- ,lhs ,rhs) (js-put "-=" ctx lhs rhs))
    (`(put* ,lhs ,rhs) (js-put "*=" ctx lhs rhs))
    (`(put/ ,lhs ,rhs) (js-put "/=" ctx lhs rhs))
    (`(put% ,lhs ,rhs) (js-put "%=" ctx lhs rhs))
    (`(prefix++ ,lhs) (string-append "(++" (js-ref ctx lhs) ")"))
    (`(prefix-- ,lhs) (string-append "(--" (js-ref ctx lhs) ")"))
    (`(postfix++ ,lhs) (string-append "(" (js-ref ctx lhs) "++)"))
    (`(postfix-- ,lhs) (string-append "(" (js-ref ctx lhs) "--)"))
    (`(typeof ,expr) (string-append "(typeof " (js-expr ctx expr) ")"))
    (`(instanceof ,instance ,class)
      (js-binop ctx " instanceof " instance class))
    (`(in ,ea ,eb) (js-binop ctx " in " ea eb))
    (`(- ,expr) (js-unop ctx "-" expr))
    (`(- ,ea ,eb) (js-binop ctx "-" ea eb))
    (`(+ ,ea ,eb) (js-binop ctx "+" ea eb))
    (`(* ,ea ,eb) (js-binop ctx "*" ea eb))
    (`(/ ,ea ,eb) (js-binop ctx "/" ea eb))
    (`(% ,ea ,eb) (js-binop ctx "%" ea eb))
    (`(bneg ,expr) (js-unop ctx "~" expr))
    (`(band ,ea ,eb) (js-binop ctx "&" ea eb))
    (`(bor ,ea ,eb) (js-binop ctx "|" ea eb))
    (`(bxor ,ea ,eb) (js-binop ctx "^" ea eb))
    (`(bshl ,ea ,eb) (js-binop ctx "<<" ea eb))
    (`(bshrs ,ea ,eb) (js-binop ctx ">>" ea eb))
    (`(bshru ,ea ,eb) (js-binop ctx ">>>" ea eb))
    (`(lneg ,expr) (js-unop ctx "!" expr))
    (`(land ,ea ,eb) (js-binop ctx "&&" ea eb))
    (`(lor ,ea ,eb) (js-binop ctx "||" ea eb))
    (`(=== ,ea ,eb) (js-binop ctx "===" ea eb))
    (`(== ,ea ,eb) (js-binop ctx "==" ea eb))
    (`(!== ,ea ,eb) (js-binop ctx "!==" ea eb))
    (`(!= ,ea ,eb) (js-binop ctx "!=" ea eb))
    (`(> ,ea ,eb) (js-binop ctx ">" ea eb))
    (`(>= ,ea ,eb) (js-binop ctx ">=" ea eb))
    (`(< ,ea ,eb) (js-binop ctx "<" ea eb))
    (`(<= ,ea ,eb) (js-binop ctx "<=" ea eb))
    (`(comma ,ea ,eb) (js-binop ctx "," ea eb))
    (`(delete ,expr) (js-unop "delete " expr))))
