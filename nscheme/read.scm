;; TODO: numeric operations
;; expt make-rectangular make-polar exact->inexact inexact->exact exact?

(define (read-error? d) (and (procedure? d) (not (read-eof? d))))
(define (read-eof? d)   (and (procedure? d) (eq? 'eof (d))))
(define (eof-object? d) (read-eof? d))
(define (eof) 'eof)
(define (char c)
  (define v (string->vector c))
  (and (= (vector-length v) 1) (vector-ref v 0)))

;; tokenization: type value src
;; punc
;; datum
;; eof
;; error
;(define EOF          (rx '(or eof "#!eof")))
;(define linebreak    '(or (set "\n\r") 133 8232 8233))
;(define space        `(or ,linebreak (set "\t\v\f ")))
;(define comment:line `(seq (or ";" "#!") (* (~ ,linebreak)) ,linebreak))
;(define skip         (rx `(* (or ,space ,comment:line))))

;(define comment:datum:begin (rx "#;"))
;(define comment:block:begin (rx "#|"))
;(define comment:block:end   (rx "|#"))

;(define punctuation (rx '($ (or ",@" "#(" "#[" "#{" "#'" "#`" "#,@" "#,"
                                ;(set "()[]{}'`,")))))

;(define atom:true  (rx '(or "#t" "#T")))
;(define atom:false (rx '(or "#f" "#F")))

;;; TODO: postprocess escape codes?
;(define atom:string
  ;(rx '(seq "\"" ($ (* (or (~ (set "\\\"")) (seq "\\" any)))) "\"")))

;(define separator `(or (set "#;'`,()[]{}\"") ,space))

;(define atom:number
  ;(let* ((sign      '($: sign (set "+-")))
         ;(exactness '(seq "#" ($: exactness (set "eEiI"))))
         ;(special
           ;`(seq ,sign (or ($: nan (seq (set "nN") (set "aA") (set "nN")))
                           ;($: inf (seq (set "iI") (set "nN") (set "fF"))))
                 ;".0"))
         ;(digit10 '(range "0" "9"))
         ;(digit16 `(or ,digit10 (range "A" "F") (range "a" "f")))
         ;(exp10   '(set "dDeEfFlLsS"))
         ;(exp16   '(set "lLsS")))
    ;(define (radix rs) `(or (seq            "#" (set ,rs))
                            ;(seq ,exactness "#" (set ,rs))
                            ;(seq            "#" (set ,rs) ,exactness)))
    ;(define (num pre d e)
      ;(define unsigned-real
        ;(let ((float-suffix `(seq "." ($: float (+ ,d)))))
          ;`(seq (or ,float-suffix
                    ;(seq ($: lhs (+ ,d))
                         ;(? (or ,float-suffix (seq "/" ($: denom (+ ,d)))))))
                ;(? ,e ($: exp (? ($: sign (set "+-"))) (+ ,d))))))
      ;(define signed-real `(or (seq ,sign     ,unsigned-real) ,special))
      ;(define real        `(or (seq (? ,sign) ,unsigned-real) ,special))
      ;(define rectangular `(seq (? ($:: real ,real))
                                ;($:: imaginary (seq (or ,sign ,signed-real)
                                                    ;(set "iI")))))
      ;(define polar       `(seq ($:: magnitude ,real) "@" ($:: angle ,real)))
      ;`(seq ,pre (or ,rectangular ,polar ($:: real ,real))))
    ;(define radix10 `(or "" ,exactness ,(radix "dD")))
    ;(rx `(seq ($: number (or ,(num radix10      digit10          exp10)
                             ;,(num (radix "bB") '(range "0" "1") exp10)
                             ;,(num (radix "oO") '(range "0" "7") exp10)
                             ;,(num (radix "xX") digit16          exp16)))
              ;(or eof ,separator)))))

;(define atom:symbol
  ;(rx `($ (or (seq (* ".") (+ (or (seq "\\" any) (seq "|" (* (~ "|")) "|")
                                  ;(~ (set".\\|") ,separator))
                              ;(* ".")))
              ;(seq "." (+ "."))))))

;(define dot (rx "."))

(define (read in)
  (define (return v pos)   (in 'forget pos) v)
  (define (fail msg p0 p1) (return (thunk (vector msg p0 p1)) p1))
  (let/cps _ (t v p0 p1) (read-datum (lambda (i) (in 'peek i)) 0 #f _)
    (case t
      ((datum) (return v   p1))
      ((eof)   (return eof p1))
      ((error) (fail v                             p0 p1))
      (else    (fail (list "unexpected token" t v) p0 p1)))))

(define-syntax-rule (case/token (k t v p0 p1) clause ...)
  (case t clause ...
    ((error) (k t v p0 p1))
    ((eof)   (k 'error "unexpected EOF" p0 p1))
    (else    (k 'error (list "unexpected token" t v) p0 p1))))
(define-syntax-rule (let/token name (k params ...) expr clause ...)
  (let/cps name (params ...) expr (case/token (k params ...) clause ...)))
(define separator? (cset #(" " ("\t" . "\r") "\"#'(),;[]`{}" #f)))

(define (read-token get pos k)
  (define (Any pos)
    (define (lbrack value len) (k 'lbracket value pos (+ pos len)))
    (define (rbrack value len) (k 'rbracket value pos (+ pos len)))
    (define (tag value len)    (k 'tag      value pos (+ pos len)))
    (case/char (get pos)
      (#f                   (k 'eof #f pos pos))
      ;; TODO: 133 8232 8233 8239 8287 12288, etc. via cseq?
      (#(" " ("\t" . "\r")) (Any (+ pos 1)))
      ("(" (lbrack 'round 1)) ("[" (lbrack 'square 1)) ("{" (lbrack 'curly 1))
      (")" (rbrack 'round 1)) ("]" (rbrack 'square 1)) ("}" (rbrack 'curly 1))
      ("'" (tag 'quote 1)) ("`" (tag 'quasiquote 1))
      ("," (case/char (get (+ pos 1))
             ("@"  (tag 'unquote-splicing 2))
             (else (tag 'unquote          1))))
      (";" (Comment:line (+ pos 1)))
      ("#" (Hash pos)) ("\"" (String pos)) ("." (Dot pos))
      (else (Number/Symbol pos))))
  (define (Hash pos)
    (define (datum  value)     (k 'datum     value pos (+ pos 2)))
    (define (lbrack value len) (k 'hlbracket value pos (+ pos len)))
    (define (tag    value len) (k 'tag       value pos (+ pos len)))
    ;; TODO: #<<EOS
    ;; ...string...
    ;; EOS
    (case/char (get (+ pos 1))
      ("Tt" (datum #t)) ("Ff" (datum #f))
      ("(" (lbrack 'round 2)) ("[" (lbrack 'square 2)) ("{" (lbrack 'curly 2))
      ("'" (tag 'syntax 2)) ("`" (tag 'quasisyntax 2))
      ("," (case/char (get (+ pos 2))
             ("@"  (tag 'unsyntax-splicing 3))
             (else (tag 'unsyntax          2))))
      ("|" (Comment:block (+ pos 2) 0 #f))
      (";" (Comment:datum (+ pos 2)))
      ("!" (cond ((cseq? get (+ pos 2) "Ee" "Oo" "Ff")
                  => (lambda (next-pos) (k 'eof #f pos next-pos)))
                 (else (Comment:line (+ pos 2)))))
      ("BDEIOXbdeiox" (read-number get pos k))
      (#f   (k 'error "unexpected EOF following #"    pos (+ pos 1)))
      (else (k 'error "invalid character following #" pos (+ pos 2)))))
  (define (Dot pos) (case/char (get (+ pos 1))
                      ;; TODO: 133 8232 8233
                      (separator? (k 'dot #f pos (+ pos 1)))
                      (else       (Number/Symbol pos))))
  (define (Comment:line pos) (case/char (get pos)
                               (#f     (k 'eof #f pos pos))
                               ;; TODO: 133 8232 8233
                               ("\n\r" (Any          (+ pos 1)))
                               (else   (Comment:line (+ pos 1)))))
  (define (Comment:block pos level mode)
    (case/char (get pos)
      ("#"  (if (eq? mode 'closing)
              (cond ((= level 0) (Any (+ pos 1)))
                    (else (Comment:block (+ pos 1) (- level 1) #f)))
              (Comment:block (+ pos 1) level 'opening)))
      ("|"  (if (eq? mode 'opening) (Comment:block (+ pos 1) (+ level 1) #f)
              (Comment:block (+ pos 1) level 'closing)))
      (#f   (k 'eof #f pos pos))
      (else (Comment:block (+ pos 1) level #f))))
  (define (Comment:datum p) (let/token _ (k t v p0 p1) (read-datum get p #f _)
                              ((datum) (Any p1))))

  (define (String start)
    (define (escape-code validate pos kv)
      (let/cps _ (type n _ next-pos) (read-number get pos _)
        (define (err msg) (k 'error msg pos next-pos))
        (case type
          ((datum) (case/char (get next-pos)
                     (";" (let ((v (validate n)))
                            (if v (kv v (+ next-pos 1))
                              (err (list "invalid escape code value" n)))))
                     (else (err "invalid escape code terminator"))))
          ((error) (k type n _ next-pos)))))
    (define (code->byte n) (and (byte? n) n))
    (define (escape-byte    pos k) (escape-code code->byte    pos k))
    (define (escape-unicode pos k) (escape-code unicode->utf8 pos k))
    (let loop ((pos (+ start 1)) (len 0))
      (case/char (get pos)
        ("\"" (let ((mv (make-mvector len 0)) (last-pos pos))
                (let loop ((pos (+ start 1)) (i 0))
                  (define (store c pos)
                    (mvector-set! mv i c) (loop pos (+ i 1)))
                  (define (store* cs pos)
                    (loop pos (+ i (mvector-copy!/list mv i cs))))
                  (define (estore s) (store (string-ref s 0) (+ pos 2)))
                  (if (= pos last-pos)
                    (k 'datum (mvector->string mv) start (+ last-pos 1))
                    (let ((c (get pos)))
                      (case/char c
                        ;; TODO: support newline escaping
                        ("\\" (case/char (get (+ pos 1))
                                ("\"" (estore "\"")) ("\\" (estore "\\"))
                                ("a"  (estore "\a")) ("b"  (estore "\b"))
                                ("e"  (estore "\e")) ("f"  (estore "\f"))
                                ("n"  (estore "\n")) ("r"  (estore "\r"))
                                ("t"  (estore "\t")) ("v"  (estore "\v"))
                                ("#"  (escape-byte    (+ pos 1) store))
                                ("Uu" (escape-unicode (+ pos 2) store*))))
                        (else (store c (+ pos 1)))))))))
        ("\\" (case/char (get (+ pos 1))
                ("\"\\abefnrtv" (loop (+ pos 2) (+ len 1)))
                ("#"  (let/cps _ (_ p) (escape-byte    (+ pos 1) _)
                        (loop p (+ len 1))))
                ("Uu" (let/cps _ (v p) (escape-unicode (+ pos 2) _)
                        (loop p (+ len (length v)))))
                (else (k 'error (list "invalid escape character"
                                      (get (+ pos 1))) pos (+ pos 2)))))
        (#f   (k 'error "unexpected EOF while reading string" start pos))
        (else (loop (+ pos 1) (+ len 1))))))

  (define (Number/Symbol p0) (let/cps _ (p1) (read-number/symbol get p0 k _)
                               (Symbol p0 p1 (- p1 p0))))
  (define (Symbol start pos len)
    (let loop ((pos pos) (len len))
      (case/char (get pos)
        (separator?
          (let ((mv (make-mvector len 0)) (last-pos pos))
            (let loop ((pos start) (i 0))
              (define (store c pos) (mvector-set! mv i c) (loop pos (+ i 1)))
              (if (= pos last-pos)
                (k 'datum (string->symbol (mvector->string mv)) start pos)
                (let ((c (get pos)))
                  (case/char c
                    ("\\" (store (get (+ pos 1)) (+ pos 2)))
                    ("|" (let eloop ((pos (+ pos 1)) (i i))
                           (let ((c (get pos)))
                             (case/char c
                               ("|"  (loop (+ pos 1) i))
                               (else (mvector-set! mv i c)
                                     (eloop (+ pos 1) (+ i 1)))))))
                    (else (store c (+ pos 1)))))))))
        ("\\" (if (get (+ pos 1)) (loop (+ pos 2) (+ len 1))
                (k 'error "unexpected EOF in symbol" start (+ pos 1))))
        ("|" (let eloop ((pos (+ pos 1)) (len len))
               (case/char (get pos)
                 ("|"  (loop (+ pos 1) len))
                 (#f   (k 'error "unexpected EOF in symbol" start pos))
                 (else (eloop (+ pos 1) (+ len 1))))))
        (else (loop (+ pos 1) (+ len 1))))))
  (Any pos))

(define (read-number get p0 k) (let/cps _ (p1) (read-number/symbol get p0 k _)
                                 (k 'error "invalid number" p0 p1)))
(define (read-number/symbol get start k ksymbol)
  (define (Rect r i pos)
    (case/char (get pos)
      ("Ii" (case/char (get (+ pos 1))
              (separator? (k 'datum (make-rectangular r i) start (+ pos 1)))
              (else       (ksymbol (+ pos 1)))))
      (else (ksymbol pos))))
  (define (NumberPrefix pos cexactness cradix)
    (NumberSign
      pos (and cexactness (case/char cexactness ("Ee" 'exact) ("Ii" 'inexact)))
      (if cradix (case/char cradix ("Bb" 2) ("Dd" 10) ("Oo" 8) ("Xx" 16)) 10)
      #f #f))
  (define (NumberSign pos ex radix prev mode)
    (case/char (get pos)
      ("+"  (NumberBody (+ pos 1) ex radix  1 prev mode))
      ("-"  (NumberBody (+ pos 1) ex radix -1 prev mode))
      (else (NumberBody pos       ex radix #f prev mode))))
  (define (NumberBody pos ex radix sign prev mode)
    (define (Natural start kd)
      (let loop ((pos start) (value 0))
        ;; TODO: compute char at compile time
        (let* ((c (get pos)) (d (case/char c
                                  (("0" . "9")       (- c (char "0")))
                                  (("A" . "F") (+ 10 (- c (char "A"))))
                                  (("a" . "f") (+ 10 (- c (char "a"))))
                                  (else        #f))))
          (if (and d (< d radix)) (loop (+ pos 1) (+ (* radix value) d))
            (kd value pos)))))
    (define (Float sign whole frac start pos ex)
      (Exp (* (or sign 1) (+ whole (* frac (expt radix (- start pos)))))
           pos ex))
    (define (Exp real pos ex)
      (define (Real real pos ex)
        (cond ((eq? ex 'inexact) (Suffix (exact->inexact real) pos ex))
              ((or (= real +inf.0) (= real -inf.0) (eqv? real +nan.0))
               (k 'error "no exact representation" start pos))
              (else (Suffix (inexact->exact real) pos ex))))
      (define (Rest sign pos) (let/cps _ (e ep) (Natural pos _)
                                (if (= ep pos) (ksymbol ep)
                                  (Real (* real (expt radix (* sign e))) ep
                                        (or ex 'inexact)))))
      (case/char (get pos)
        ("DEFLSdefls" (case/char (get (+ pos 1))
                        ("+"  (Rest  1 (+ pos 2)))
                        ("-"  (Rest -1 (+ pos 2)))
                        (else (Rest  1 (+ pos 1)))))
        (else (Real real pos ex))))
    (define (Suffix real pos ex)
      (case mode
        ((#f) (case/char (get pos)
                (separator? (k 'datum real start pos))
                ("+"        (NumberBody (+ pos 1) ex radix  1 real 'i))
                ("-"        (NumberBody (+ pos 1) ex radix -1 real 'i))
                ("@"        (NumberSign (+ pos 1) ex radix    real '@))
                (else       (Rect 0 real pos))))
        ((i) (Rect prev real pos))
        ((@) (case/char (get pos)
               (separator? (if (eq? ex 'exact)
                             (k 'error "no exact representation" start pos)
                             (k 'datum (make-polar prev real)    start pos)))
               (else       (ksymbol pos))))))
    (define (NoLHS)
      (define (Special v) (Suffix (* (or sign 1) v) (+ pos 5) ex))
      (case/char (get pos)
        ("."  (let/cps _ (f fp) (Natural (+ pos 1) _)
                (if (= fp (+ pos 1)) (ksymbol fp)
                  (Float sign 0 f (+ pos 1) fp (or ex 'inexact)))))
        (else (if (not sign) (ksymbol pos)
                (cond ((cseq? get pos "Ii" "Nn" "Ff" "." "0") (Special +inf.0))
                      ((cseq? get pos "Nn" "Aa" "Nn" "." "0") (Special +nan.0))
                      ((not mode)    (Rect 0    (or sign 1) pos))
                      ((eq? mode 'i) (Rect prev     sign    pos))
                      (else (ksymbol pos)))))))
    (define (LHS n np)
      (case/char (get np)
        ("." (let/cps _ (f fp) (Natural (+ np 1) _)
               (Float sign n f (+ np 1) fp (or ex 'inexact))))
        ("/" (let/cps _ (d dp) (Natural (+ np 1) _)
               (define v (if (= d 0) (if (= n 0) +nan.0 +inf.0) (/ n d)))
               (Exp (* (or sign 1) v) dp ex)))
        (else (Exp (* (or sign 1) n) np ex))))
    (Natural pos (lambda (n np) (if (= np pos) (NoLHS) (LHS n np)))))
  (define (herr hp) (k 'error "invalid character following #" hp (+ hp 2)))
  (case/char (get start)
    ("#" (let ((c0 (get (+ start 1))))
           (case/char c0
             ("EIei" (case/char (get (+ start 2))
                       ("#" (let ((c1 (get (+ start 3))))
                              (case/char c1
                                ("BDOXbdox" (NumberPrefix (+ start 4) c0 c1))
                                (else (herr (+ start 2))))))
                       (else (NumberPrefix (+ start 2) c0 #f))))
             ("BDOXbdox" (case/char (get (+ start 2))
                           ("#" (let ((c1 (get (+ start 2))))
                                  (case/char c1
                                    ("EIei" (NumberPrefix (+ start 4) c1 c0))
                                    (else (herr (+ start 2))))))
                           (else (NumberPrefix (+ start 2) #f c0))))
             (else (herr start)))))
    (else (NumberSign start #f 10 #f #f))))

(define (read-datum get pos annotate k)
  (define (dloop pos k) (read-datum get pos annotate k))
  (define (read-compound start pos delim-shape vec?)
    (let eloop ((pos pos) (acc '()))
      (let/token _ (k t v p0 p1) (dloop pos _)
        ;; TODO: annotate element
        ((datum)    (eloop p1 (cons v acc)))
        ((rbracket) (cond ((eq? delim-shape v)
                           ;; TODO: annotate compound
                           (define c (reverse acc))
                           (k 'datum (if vec? (list->vector c) c) start p1))
                          (else (k 'error (list "mismatched closing bracket" v)
                                   p0 p1))))
        ((dot) (if (or (null? acc) vec?) (k 'error "misplaced dot" p0 p1)
                 (let/token _ (k t v p0 p1) (dloop p1 _)
                   ((datum)
                    (let/token _ (k t v2 p0 p1) (dloop p1 _)
                      ;; TODO: annotate element and compound
                      ((rbracket) (k 'datum (foldl cons v acc) start p1))
                      ((datum) (k 'error "extra item after dot" p0 p1))))))))))
  (let/cps _ (type value start end) (read-token get pos _)
    (case type
      ;; TODO: annotate
      ((datum)     (k 'datum value start end))
      ((lbracket)  (read-compound start end value #f))
      ((hlbracket) (read-compound start end value #t))
      ((tag) (let/token _ (k t v p0 p1) (dloop end _)
               ;; TODO: annotate both tag value and list
               ((datum) (k 'datum (list value v) start p1))))
      (else (k type value start end)))))

(define (string->number s)
  (define in (port:string:input s))
  (read-number/symbol (lambda (i) (in 'peek i)) 0
                      (lambda (t v p0 p1) (case t ((datum) v) (else #f)))
                      (lambda (_) #f)))

;; TODO: Re-implement reader using this grammar
;(define (digit->nat ch)
  ;(define (in-range? start end) (and ch (<= (char start) ch (char end))))
  ;(cond ((in-range? "0" "9")       (- ch (char "0")))
        ;((in-range? "A" "F") (+ 10 (- ch (char "A"))))
        ;((in-range? "a" "f") (+ 10 (- ch (char "a"))))
        ;(else                #f)))
;(define (digits->nat ds radix)
  ;(foldl (lambda (d acc) (+ (* acc radix) d)) 0 ds))

;(define Radix (alt (seq (alt* "bB") (return 2))
                   ;(seq (alt* "oO") (return 8))
                   ;(seq (alt* "dD") (return 10))
                   ;(seq (alt* "xX") (return 16))))

;(define Digit-2  (app/seq digit->nat (alt* "01")))
;(define Digit-8  (app/seq digit->nat (alt* "01234567")))
;(define Digit-10 (app/seq digit->nat (alt* "0123456789")))
;(define Digit-16 (app/seq digit->nat (alt* "0123456789ABCDEFabcdef")))
;(define (Digit/radix radix)
  ;(case radix ((2) Digit-2) ((8) Digit-8) ((10) Digit-10) ((16) Digit-16)))

;(define-grammar*
  ;; TODO: lower unicode code points to units
  ;(Linebreak     (alt (alt* "\n\r") 133 8232 8233))
  ;(WSpace        (alt (alt* "\t\v\f ") Linebreak))
  ;(Separator     (alt WSpace (alt* "#;'`,()[]{}\"") #f))
  ;(Comment:datum (seq "#;" Datum))
  ;(Comment:line  (seq (alt ";" (seq "#!" (not/peek "eof")))
                      ;(*/seq (not/one Linebreak)) Linebreak))
  ;(Comment:block (seq "#|" (*/seq (alt (not/one (alt "#|" "|#"))
                                       ;Comment:block)) "|#"))
  ;(Comment (alt Comment:datum Comment:line Comment:block))
  ;(Space   (alt WSpace Comment))
  ;(EOF     (seq (*/seq Space) (alt #f "#!eof") (return eof)))
  ;(Datum (seq (*/seq Space)
              ;(alt (alt Hash
                        ;(List "(" ")") (List "[" "]") (List "{" "}")
                        ;(Tag  "'" 'quote)
                        ;(Tag  "`" 'quasiquote)
                        ;(Tag@ "," 'unquote 'unquote-splicing)
                        ;String
                        ;Number/Symbol))))
  ;(Hash (seq "#" (alt (seq (alt* "tT") (return #t))
                      ;(seq (alt* "fF") (return #f))
                      ;(Vector "(" ")") (Vector "[" "]") (Vector "{" "}")
                      ;(Tag  "'" 'syntax)
                      ;(Tag  "`" 'quasisyntax)
                      ;(Tag@ "," 'unsyntax 'unsyntax-splicing)
                      ;NumberAfterHash)))

  ;((List ldelim rdelim)
   ;;; TODO: this version factors out parsing of init to avoid worst-case
   ;;; exponential parsing when using naive non-memoizing DFS.  But for some
   ;;; reason it's about 50% slower in the typical case.
   ;;(seq ldelim
        ;;(seq0 (alt (let/return
                     ;;((init (+/seq Datum))
                      ;;(last (alt (return '())
                                 ;;(seq (*/seq Space) "." (peek Separator)
                                      ;;Datum))))
                     ;;(if (null? last) init (append init last)))
                   ;;(return '()))
              ;;(*/seq Space) rdelim))
   ;;; Even this partial factoring is about 3% slower.  Maybe the problem is
   ;;; how nested alts behave in the current DFS parser.
   ;;(seq ldelim
        ;;(seq0 (alt (*/seq Datum)
                   ;;(seq (let/return ((init (seq0 (+/seq Datum) (*/seq Space) "."
                                                 ;;(peek Separator)))
                                     ;;(last Datum))
                          ;;(append init last))))
              ;;(*/seq Space) rdelim))
   ;(alt (seq ldelim (seq0 (*/seq Datum) (*/seq Space) rdelim))
        ;(seq ldelim
             ;(let/return ((init (seq0 (+/seq Datum) (*/seq Space) "."
                                      ;(peek Separator)))
                          ;(last (seq0 Datum (*/seq Space) rdelim)))
               ;(append init last)))))
  ;((Vector ldelim rdelim)
   ;(seq ldelim (app/seq list->vector
                 ;(seq0 (*/seq Datum) (*/seq Space) rdelim))))
  ;((Tag leader tag) (seq leader (let/return ((d Datum)) (list tag d))))
  ;((Tag@ leader tag1 tag2)
   ;(seq leader (alt (let/return ((d (seq "@" Datum))) (list tag2 d))
                    ;(let/return ((d (seq (peek (not/alt* "@")) Datum)))
                      ;(list tag1 d)))))
  ;(String
    ;(define Escape-code       (let/seq ((r Radix))
                                ;(app/seq (lambda (ds) (digits->nat ds r))
                                  ;(seq0 (*/seq (Digit/radix r)) ";"))))
    ;(define Escape-code-unit  (let/seq ((n (seq "\\" Escape-code)))
                                ;(if (<= 0 n 255) (return (list n))
                                  ;(fail "invalid code unit" n))))
    ;(define Escape-code-point (let/seq ((n (seq "\\" (alt* "uU") Escape-code)))
                                ;(define units (unicode->utf8 n))
                                ;(if units (return units)
                                  ;(fail "invalid code point" n))))
    ;(define Ecodes (alt Escape-code-unit Escape-code-point))
    ;(define Echars
      ;(apply alt (map (lambda (p) (seq (car p) (return (list (char (cdr p))))))
                      ;'(("\\a"  . "\a") ("\\t"  . "\t") ("\\n"  . "\n")
                        ;("\\v"  . "\v") ("\\f"  . "\f") ("\\r"  . "\r")
                        ;("\\e"  . "\e") ("\\\"" . "\"") ("\\\\" . "\\")))))
    ;(app/seq (lambda (css) (list->string (append* css)))
      ;(seq "\"" (seq0 (*/seq (alt (app/seq list (not/alt* "\\\""))
                                  ;Ecodes Echars)) "\""))))

  ;(Symbol
    ;(app/seq (lambda (css) (string->symbol (list->string (append* css))))
      ;(seq (not/peek "." Separator)
           ;(seq0 (+/seq (alt (app/seq list
                               ;(alt (not/one (alt (alt* "\\|") Separator))
                                    ;(seq "\\" any)))
                             ;(seq "|" (seq0 (*/seq (not/alt* "|")) "|"))))
                 ;(peek Separator)))))
  ;(Number/Symbol (alt (NumberNoPrefix #f 10) Symbol))
  ;(Number (alt (seq "#" NumberAfterHash) (NumberNoPrefix #f 10)))
  ;(NumberAfterHash
    ;(define Exact (alt (seq (alt* "eE") (return 'exact))
                       ;(seq (alt* "iI") (return 'inexact))))
    ;(alt (let/seq ((e Exact) (r (?/seq 10 "#" Radix))) (NumberNoPrefix e r))
         ;(let/seq ((r Radix) (e (?/seq #f "#" Exact))) (NumberNoPrefix e r))))

  ;((NumberNoPrefix exactness radix)
   ;(define (fail-repr . args) (fail "no exact representation" args))
   ;(define (make-dec lhs rhs ex)
     ;(define float? (or (eq? exactness 'inexact)
                        ;(and (not (eq? exactness 'exact)) (or rhs ex))))
     ;(define m (digits->nat (append lhs (or rhs '())) radix))
     ;(define d (expt radix (- (length (or rhs '())))))
     ;(define e (expt radix (digits->nat (or ex '()) radix)))
     ;(define v (* m d e))
     ;(if float? (exact->inexact v) v))
   ;(define (make-rat num denom ex)
     ;(define float? (or (eq? exactness 'inexact) ex))
     ;(define n (digits->nat num radix))
     ;(define d (digits->nat denom radix))
     ;(define e (expt radix (digits->nat (or ex '()) radix)))
     ;(define v (cond ((and (eqv? d 0) (eqv? n 0)) +nan.0)
                     ;((eqv? d 0)                  +inf.0)
                     ;(else                        (* (/ n d) e))))
     ;(cond (float?       (return (exact->inexact v)))
           ;((inexact? v) (fail-repr (list n "/" d "*" e)))
           ;(else         (return v))))
   ;(define D  (Digit/radix radix))
   ;(define D* (*/seq D))
   ;(define D+ (+/seq D))
   ;(define Sign (alt (seq "+" (return 1)) (seq "-" (return -1))))
   ;;; TODO: negative exponents
   ;(define Expt (seq (if (< radix 16) (alt* "dDeEfFlLsS") (alt* "lLsS")) D+))
   ;(define NAN (seq (alt* "nN") (alt* "aA") (alt* "nN") ".0" (return +nan.0)))
   ;(define INF (seq (alt* "iI") (alt* "nN") (alt* "fF") ".0" (return +inf.0)))
   ;(define Unsigned-real
     ;(alt (app/seq  make-dec D+      (?/seq #f "." D*) (?/seq #f Expt))
          ;(app/seq  make-dec (return '()) (seq "." D+) (?/seq #f Expt))
          ;(bind/seq make-rat D+           (seq "/" D+) (?/seq #f Expt))))
   ;(define Signed-real (app/seq * Sign (alt Unsigned-real NAN INF)))
   ;(define Real        (alt Signed-real Unsigned-real))
   ;(define Imaginary   (seq0 (alt Signed-real Sign) (alt* "iI")))
   ;(define Polar (let/seq ((mag Real) (_ "@") (ang Real))
                   ;(if (and (eq? exactness 'exact) (not (eqv? ang 0)))
                     ;(fail-repr (list mag "@" ang))
                     ;(return (make-polar mag ang)))))
   ;(seq0 (alt Real (app/seq make-rectangular (?/seq 0 Real) Imaginary) Polar)
         ;(peek Separator))))

;(define parse (grammar->parser/dfs (alt Datum EOF)))
;(define (read/experimental in) (parse in (lambda (result-thunk) (result-thunk))
                                      ;(lambda (reasons) (thunk reasons))))
