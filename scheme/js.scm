;; Value transfer
;; declare: var
;; Control transfer
;; apply, label: {}, if/else, switch/case, [label:] for, for/in, [label:] while, do/while, break [any label], continue [loop label], return, throw, try/catch

;; Misc
;; publicly visible (global?) names (and symbol->name table for linking)
;; bind used globals to local variables
;; inline single-use temp vars?
;; debugger;
;; new, this, call/apply
;; minification
;; JSON subset
;; Node.js has global,require instead of window

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
      (string-append "function(" (comma* (sym->ident* param*))
                     "){\"use strict\";" (js-stmt* ctx body*) "}"))
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
