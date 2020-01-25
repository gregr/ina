;; TODO: numeric operations
;; expt make-rectangular make-polar exact->inexact inexact->exact exact?

(define (read-error? d) (and (procedure? d) (not (read-eof? d))))
(define (read-eof? d)   (and (procedure? d) (eq? 'eof (d))))
(define (eof-object? d) (read-eof? d))
(define (eof) 'eof)

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

(define (read/experimental in)
  (define (get i)          (in 'peek i))
  (define (return v pos)   (in 'forget pos) v)
  (define (fail msg p0 p1) (return (thunk (vector msg p0 p1)) p1))
  (read-datum/token
    get 0 #f
    (lambda (t v p0 p1)
      (case t
        ((datum) (return v   p1))
        ((eof)   (return eof p1))
        ((error) (fail v                              p0 p1))
        (else    (fail (list "unexpected token:" t v) p0 p1))))))

(define (read-token get pos k)
  (define (Any pos)
    (define (punc type value len) (k type value pos (+ pos len)))
    (define (lbrack value len) (k 'lbracket value pos (+ pos len)))
    (define (rbrack value len) (k 'rbracket value pos (+ pos len)))
    ;(define (par type value len) (k type value    pos (+ pos len)))
    (define (tag value len) (k 'tag value pos (+ pos len)))
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
      (";" (Comment:line  (+ pos 1)))
      ("#" (Hash pos)) ("\"" (String pos)) ("." (Dot pos))
      (else (Number/Symbol pos))))
  (define (Hash pos)
    (define (datum v)       (k 'datum v   pos (+ pos 2)))
    ;(define (punc type len) (k type  #f   pos (+ pos len)))
    (define (lbrack value len) (k 'hlbracket value pos (+ pos len)))
    (define (tag value len) (k 'tag value pos (+ pos len)))
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
      (#f   (k 'error "unexpected EOF following #" pos (+ pos 1)))
      (else (k 'error (list "invalid character following #" (get (+ pos 1)))
               pos (+ pos 1)))))
  (define (Dot pos)
    (case/char (get (+ pos 1))
      ;; TODO: 133 8232 8233
      (#(" " ("\t" . "\r") "\"#'(),;[]`{}" #f) (k 'dot #f pos (+ pos 1)))
      (else                                    (Number/Symbol pos))))
  (define (Comment:line pos)
    (case/char (get pos)
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
  (define (Comment:datum pos)
    (read-datum/token
      get pos #f
      (lambda (t v p0 p1)
        (case t
          ((datum) (Any p1))
          ((error) (k t v p0 p1))
          ((eof) (k 'error "unexpected EOF" p0 p1))
          (else  (k 'error (list "unexpected token" t v) p0 p1))))))

  (define (String start)
    (define (escape-byte pos consume)
      (read-number
        get (+ pos 2)
        (lambda (type n _ next-pos)
          (define (err msg) (k 'error msg pos next-pos))
          (case type
            ((datum) (case/char (get next-pos)
                       (";" (if (byte? n) (consume n (+ next-pos 1))
                              (err (list "invalid byte escape value:" n))))
                       (else (err "invalid byte escape sequence terminator"))))
            ((error) (k type n _ next-pos))))))
    (define (escape-unicode pos consume)
      (read-number
        get (+ pos 2)
        (lambda (type n _ next-pos)
          (define (err msg) (k 'error msg pos next-pos))
          (case type
            ((datum)
             (case/char (get next-pos)
               (";" (let ((bs (unicode->utf8 n)))
                      (if bs (consume bs (+ next-pos 1))
                        (err (list "invalid unicode escape value:" n)))))
               (else (err "invalid unicode escape sequence terminator"))))
            ((error) (k type n _ next-pos))))))
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
                                ("#"  (escape-byte    pos store))
                                ("Uu" (escape-unicode pos store*))))
                        (else (store c (+ pos 1)))))))))
        ("\\" (case/char (get (+ pos 1))
                ("\"\\abefnrtv" (loop (+ pos 2) (+ len 1)))
                ("#"  (escape-byte pos (lambda (_ pos) (loop pos (+ len 1)))))
                ("Uu" (escape-unicode pos (lambda (bs pos)
                                            (loop pos (+ len (length bs))))))
                (else (k 'error (list "invalid escape character:"
                                      (get (+ pos 1))) pos (+ pos 2)))))
        (#f   (k 'error "unexpected EOF while reading string" start pos))
        (else (loop (+ pos 1) (+ len 1))))))

  (define (Number/Symbol start)
    (read-number/symbol
      get start k (lambda (pos) (Symbol start pos (- pos start)))))
  (define (Symbol start pos len)
    (let loop ((pos pos) (len len))
      (case/char (get pos)
        (#(" " ("\t" . "\r") "\"#'(),;[]`{}" #f)
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

(define (read-number get pos k)
  (read-number/symbol
    get pos k (lambda (epos) (k 'error "invalid number" pos epos))))
(define (read-number/symbol get pos k ksymbol)
  ; NumberAfterHash NumberNoPrefix Number
  ;; TODO: attempt to read an actual number
  (case/char (get pos)
    ("|\\" (ksymbol pos))
    (else (ksymbol (+ pos 1))))
  )

(define (read-datum/token get pos annotate k)
  (define (read-compound start pos delim-shape vec?)
    (let loop ((pos pos) (acc '()))
      (read-datum/token
        get pos annotate
        (lambda (t v p0 p1)
          (case t
            ;; TODO: annotate element
            ((datum) (loop p1 (cons v acc)))
            ((rbracket)
             ;; TODO: annotate compound
             (cond ((eq? delim-shape v)
                    (define c (reverse acc))
                    (k 'datum (if vec? (list->vector c) c) start p1))
                   (else (k 'error (list "mismatched closing bracket" v)
                            p0 p1))))
            ((dot) (if (or (null? acc) vec?) (k 'error "misplaced dot" p0 p1)
                     (read-datum/token
                       get p1 annotate
                       (lambda (t v p0 p1)
                         (case t
                           ((datum)
                            (read-datum/token
                              get p1 annotate
                              (lambda (t v2 p0 p1)
                                (case t
                                  ;; TODO: annotate element and compound
                                  ((rbracket) (k 'datum (foldl cons v acc)
                                                 start p1))
                                  ((datum) (k 'error "extra datum after dot"
                                              p0 p1))
                                  ((eof) (k 'error "unexpected EOF" p0 p1))
                                  ((error) (k t v2 p0 p1))
                                  (else (k 'error (list "unexpected token" t v2)
                                           p0 p1))))))
                           ((eof) (k 'error "unexpected EOF" p0 p1))
                           ((error) (k t v p0 p1))
                           (else (k 'error (list "unexpected token" t v)
                                    p0 p1)))))))
            ((error) (k t v p0 p1))
            ((eof)   (k 'error "unexpected EOF" p0 p1))
            (else    (k 'error (list "unexpected token" t v) p0 p1)))))))
  (read-token get pos
              (lambda (type value start end)
                (case type
                  ;; TODO: annotate
                  ((datum) (k 'datum value start end))
                  ((lbracket)  (read-compound start end value #f))
                  ((hlbracket) (read-compound start end value #t))
                  ((tag) (read-datum/token
                           get end annotate
                           (lambda (t v p0 p1)
                             (case t
                               ;; TODO: annotate both tag value and list
                               ((datum) (k 'datum (list value v) start p1))
                               ((error) (k t v p0 p1))
                               ((eof)   (k 'error "unexpected EOF" p0 p1))
                               (else    (k 'error (list "unexpected token" t v)
                                           p0 p1))))))
                  (else (k type value start end))))))

;(define (string->number s)
  ;)

(define (char c)
  (define v (string->vector c))
  (and (= (vector-length v) 1) (vector-ref v 0)))
(define (char=? ch c) (equal? ch (char c)))

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

(define linebreaks (append (map char '("\n" "\r")) '(133)))
(define spaces     (string->list "\t\n\v\f\r "))
(define separators (cons #f (append spaces (string->list "#;'`,()[]{}\""))))

(define (read in)
  (read/k (lambda () (in 'peek 0)) (lambda () (in 'get))
          (lambda (x) (lambda () x)) (lambda () eof)
          (lambda (d) d)))

(define (read/k peek next k:error k:eof k)
  (define (k:fail ch) (k:error (list "unexpected" ch)))
  (define (Datum k:dot k:delim k)
    (define ch (next))
    (define (=? . cs) (ormap (lambda (c) (char=? ch c)) cs))
    (define (Comment:line k)
      (define ch (next))
      (cond ((member ch linebreaks) (Datum k:dot k:delim k))
            ;; TODO: grammar doesn't support escaping linebreaks.  Should it?
            ((char=? ch "\\")       (next) (Comment:line k))
            (else                          (Comment:line k))))
    (define (Comment:line/eof targets)
      (if (null? targets) (k:delim #f)
        (cond ((char=? (peek) (car targets)) (next)
                                             (Comment:line/eof (cdr targets)))
              (else                          (Comment:line k)))))
    (define (Comment:datum k) (Datum k:fail k:fail
                                     (lambda (_) (Datum k:dot k:delim k))))
    (define (Comment:block k)
      (let loop ((ch (next)) (level 0))
        (define (=2? c1 c2) (and (char=? ch c1) (char=? (next) c2)))
        (cond ((not ch)      (k:eof))
              ((=2? "|" "#") (cond ((= level 0) (Datum k:dot k:delim k))
                                   (else        (loop (next) (- level 1)))))
              ((=2? "#" "|") (loop (next) (+ level 1)))
              (else          (loop (next)    level)))))
    (define (Dot dot k)
      (cond ((member (peek) separators) (k:dot dot))
            (else                       (Number/Symbol (list dot) k))))
    (define (Hash k)
      (define ch (next))
      (define (=? . cs) (ormap (lambda (c) (char=? ch c)) cs))
      ;; TODO: #<<EOS
      ;; ...string...
      ;; EOS
      (cond ((=? "t" "T") (k #t))
            ((=? "f" "F") (k #f))
            ((=? "!") (Comment:line/eof '("e" "o" "f")))
            ((=? ";") (Comment:datum                       k))
            ((=? "|") (Comment:block                       k))
            ((=? "(") (Vector ")"                          k))
            ((=? "[") (Vector "]"                          k))
            ((=? "{") (Vector "}"                          k))
            ((=? "'") (Tag    'syntax                      k))
            ((=? "`") (Tag    'quasisyntax                 k))
            ((=? ",") (Tag@   'unsyntax 'unsyntax-splicing k))
            ((=? "i" "I" "e" "E" "b" "B" "d" "D" "o" "O" "x" "X")
             (define acc (if (char=? (peek) "#")
                           (list (next) ch (char "#"))
                           (list        ch (char "#"))))
             (Number/Symbol acc (lambda (v)
                                  (if (number? v) (k v)
                                    (k:error (list "invalid number" v))))))
            (else (k:fail ch))))

    (cond ((member ch spaces) (Datum k:dot k:delim k))
          ((=? ";")  (Comment:line                    k))
          ((=? "#")  (Hash                            k))
          ((=? "(")  (List ")"                        k))
          ((=? "[")  (List "]"                        k))
          ((=? "{")  (List "}"                        k))
          ((=? "'")  (Tag  'quote                     k))
          ((=? "`")  (Tag  'quasiquote                k))
          ((=? ",")  (Tag@ 'unquote 'unquote-splicing k))
          ((=? "\"") (String                          k))
          ((=? ".")  (Dot  ch                         k))
          ((or (not ch) (=? ")" "]" "}")) (k:delim ch))
          (else                           (Number/Symbol (list ch) k))))

  (define (Sequence dot-allowed? delim k)
    (let loop ((data '()))
      (define (Delimiter last ch)
        (cond ((char=? ch delim) (k (foldl cons last data)))
              (else              (k:fail ch))))
      (define (k:dot dot)
        (cond (dot-allowed? (Datum k:fail k:fail (lambda (datum)
                                                   (Delimiter datum (next)))))
              (else         (k:fail dot))))
      (Datum k:dot (lambda (ch) (Delimiter '() ch))
             (lambda (datum) (loop (cons datum data))))))
  (define (List   delim k) (Sequence #t delim k))
  (define (Vector delim k) (Sequence #f delim (lambda (ds)
                                                (k (list->vector ds)))))
  (define (Tag tag k)
    (Datum k:fail k:fail (lambda (d) (k (list tag d)))))
  (define (Tag@ tag tag@ k)
    (cond ((char=? (peek) "@") (next) (Tag tag@ k))
          (else                       (Tag tag  k))))

  (define (String k)
    (let loop ((ch (next)) (acc '()))
      (define (escape-code radix code->bytes)
        (let c-loop ((num (list radix (char "#"))))
          (define ch (next))
          (cond ((char=? ch ";")
                 (let* ((code (rlist->string num)) (n (string->number code)))
                   (cond ((code->bytes n)
                          => (lambda (bs)
                               (loop (next) (reverse-append bs acc))))
                         (else (k:error (list "invalid code" code))))))
                ((not ch) (k:error (list "missing code delimiter" num)))
                (else     (c-loop (cons ch num))))))
      (define (escape-code-unit radix)
        (escape-code radix (lambda (n) (and n (<= 0 n 255) (list n)))))
      (define (escape-code-point radix)
        (escape-code radix (lambda (n) (and n (unicode->utf8 n)))))
      (define (escape ch)
        (cond ((assoc ch (map (lambda (kv) (cons (char (car kv))
                                                 (char (cdr kv))))
                              '(("a"  . "\a") ("t"  . "\t") ("n"  . "\n")
                                ("v"  . "\v") ("f"  . "\f") ("r"  . "\r")
                                ("e"  . "\e") ("\"" . "\"") ("\\" . "\\"))))
               => (lambda (kv) (loop (next) (cons (cdr kv) acc))))
              ((and (member ch     (string->list "uU"))
                    (member (peek) (string->list "bBoOdDxX")))
               (escape-code-point (next)))
              ((member ch (string->list "bBoOdDxX")) (escape-code-unit ch))
              ((char=? ch "\n") (loop (next) acc))
              ((char=? ch "\r") (define ch (next))
                                (loop (if (char=? ch "\n") (next) ch) acc))
              (else (k:fail ch))))
      (cond ((char=? ch "\"") (k (rlist->string acc)))
            ((char=? ch "\\") (escape (next)))
            (else             (loop (next) (cons ch acc))))))

  (define (Number/Symbol acc k)
    (define (loop acc num? escape?)
      (cond (escape? (cond ((not (peek))               (loop acc num? #f))
                           ((char=? (peek) "|") (next) (loop acc num? #f))
                           (else (loop (cons (next) acc) num? escape?))))
            ((member (peek) separators)
             (let* ((s (rlist->string acc)) (n (and num? (string->number s))))
               (k (if n n (string->symbol s)))))
            ((char=? (peek) "\\") (next) (define ch (next))
                                  (and ch (loop (cons ch acc) #f escape?)))
            ((char=? (peek) "|")  (next) (loop acc #f #t))
            (else (loop (cons (next) acc) num? escape?))))
    (cond ((char=? (car acc) "\\") (define ch (next))
                                   (and ch (loop (cons ch (cdr acc)) #f #f)))
          ((char=? (car acc) "|")          (loop          (cdr acc)  #f #t))
          (else                            (loop               acc   #t #f))))

  (Datum k:fail (lambda (delim) (if delim (k:fail delim) (k:eof)))
         (lambda (d) d)))

(define (string->number s)
  (define in (port:string:input s))
  (define (peek) (in 'peek 0))
  (define (next) (in 'get))
  (let loop ((ch (next)) (sign #f) (radix #f) (exactness #f)
             (lhs #f) (rhs #f) (frac-type #f) (exp #f) (real #f) (rad #f))
    (define (=? . cs) (ormap (lambda (c) (char=? ch c)) cs))
    (define (digit)
      (define (in-range? start end) (and ch (<= (char start) ch (char end))))
      (define d (cond ((in-range? "0" "9")       (- ch (char "0")))
                      ((in-range? "A" "F") (+ 10 (- ch (char "A"))))
                      ((in-range? "a" "f") (+ 10 (- ch (char "a"))))
                      (else                #f)))
      (and d (< d (or radix 10)) d))
    (define (whole digits)
      (let loop ((ds digits) (acc 0) (mult 1))
        (if (null? ds) acc
          (loop (cdr ds) (+ (* (car ds) mult) acc) (* radix mult)))))
    (define (make-real) (if (number? lhs) lhs (make-real/digits)))
    (define (make-real/digits)
      (define nlhs (whole lhs))
      (define nrhs (whole (or rhs '())))
      ;; TODO: negative exponents
      (define nexp (expt radix (whole (or exp '()))))
      (define m (cond ((eq? (or frac-type 'dec) 'dec)
                       (+ nlhs (* nrhs (expt radix
                                             (- (length (or rhs '())))))))
                      ((and (eqv? nrhs 0) (eqv? nlhs 0)) +nan.0)
                      ((and (eqv? nrhs 0) (> nlhs 0))    +inf.0)
                      ((and (eqv? nrhs 0) (< nlhs 0))    -inf.0)
                      (else                              (/ nlhs nrhs))))
      (define n (* (or sign 1) nexp m))
      (and n (case exactness
               ('exact   (and (exact? n) n))
               ('inexact (exact->inexact n))
               ('infer   (if (or exp (eq? frac-type 'dec)) (exact->inexact n)
                           (and (exact? n) n))))))
    (define (make k)
      (define n (make-real))
      (and n (k (cond (real (make-rectangular real n))
                      (rad  (make-polar       rad  n))
                      (else                        n)))))
    (define (k/special cs v.0 v.f)
      (define v
        (and sign (not lhs) (not (eq? exactness 'exact))
             (andmap (lambda (cs)
                       (let ((c1 (car cs)) (c2 (cadr cs)) (ch (next)))
                         (or (char=? ch c1) (char=? ch c2)))) cs)
             (char=? (next) ".")
             (let ((last (next)))
               (cond ((char=? last "0")                        (* sign v.0))
                     ((or (char=? last "f") (char=? last "F")) (* sign v.f))
                     (else                                     #f)))))
      (and v (loop (next) sign radix exactness v rhs frac-type exp real rad)))
    (define (k/sign s)
      (if lhs (and (not real) (make (lambda (n) (loop (next) s radix exactness
                                                      #f #f #f #f n #f))))
        (and (not sign) (loop (next) s radix exactness
                              lhs rhs frac-type exp real rad))))
    (define (k/new ch lhs rhs ftype real rad)
      (loop ch sign (or radix 10) (or exactness 'infer)
            lhs rhs ftype #f real rad))
    (cond ((digit)
           => (lambda (d)
                (cond (exp  (loop (next) sign radix exactness
                                  lhs rhs frac-type (cons d exp) real rad))
                      (rhs  (loop (next) sign radix exactness
                                  lhs (cons d rhs) frac-type exp real rad))
                      (lhs  (loop (next) sign radix exactness
                                  (cons d lhs) rhs frac-type exp real rad))
                      (else (k/new (next) (list d) rhs frac-type real rad)))))
          ((=? "#")
           (define ch (next))
           (define (=? . cs)       (ormap (lambda (c) (char=? ch c)) cs))
           (define (k/radix r)     (and (not sign) (not radix)
                                        (loop (next) sign r exactness
                                              lhs rhs frac-type exp real rad)))
           (define (k/exactness e) (and (not sign) (not exactness)
                                        (loop (next) sign radix e
                                              lhs rhs frac-type exp real rad)))
           (cond ((=? "b" "B") (k/radix 2))
                 ((=? "o" "O") (k/radix 8))
                 ((=? "d" "D") (k/radix 10))
                 ((=? "x" "X") (k/radix 16))
                 ((=? "i" "I") (k/exactness 'inexact))
                 ((=? "e" "E") (k/exactness 'exact))
                 (else         #f)))
          ((=? "-") (k/sign -1))
          ((=? "+") (k/sign  1))
          ((=? ".") (cond ((not lhs) (k/new (next) '() '() 'dec real rad))
                          ((not rhs) (loop (next) sign radix exactness
                                           lhs '() 'dec exp real rad))
                          (else      #f)))
          ((=? "/") (cond ((not lhs) #f)
                          ((not rhs) (loop (next) sign radix exactness
                                           lhs '() '/ exp real rad))
                          (else      #f)))
          ((=? "l" "L" "d" "D" "e" "E" "s" "S" "f" "F")
           (and lhs (not exp) (loop (next) sign radix exactness
                                    lhs (or rhs '()) (or frac-type 'dec)
                                    '() real rad)))
          ((=? "@") (and lhs (not real) (not rad) (not (eq? exactness 'exact))
                         (make (lambda (n) (loop (next) #f radix exactness
                                                 #f #f #f #f #f n)))))
          ((=? "i" "I")
           (if (and sign (not (peek)))
             (cond ((not lhs)  (k/new ch '(1) rhs frac-type real rad))
                   ((not real) (k/new ch lhs  rhs frac-type 0    rad))
                   (else (and (not (next)) (make (lambda (n) n)))))
             ;; TODO: grammar doesn't support .f versions.  Should it?
             (k/special '(("n" "N") ("f" "F")) +inf.0 +inf.f)))
          ((=? "n" "N") (k/special '(("a" "A") ("n" "N")) +nan.0 +nan.f))
          ((and (not ch) (not real)) (and lhs (make (lambda (n) n))))
          (else                      #f))))
