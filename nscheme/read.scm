;; TODO: move these
(define (reverse-append xs ys) (foldl cons ys xs))
(define (string->list s) (vector->list (string->vector s)))
(define (list->string cs) (vector->string (list->vector cs)))
(define (rlist->string cs) (list->string (reverse cs)))
(define (integer->octets i)
  (define (lead tag shift)
    (bitwise-ior tag (bitwise-arithmetic-shift i shift)))
  (define (follow shift)
    (bitwise-ior #b10000000 (bitwise-and
                              #b111111 (bitwise-arithmetic-shift i shift))))
  (and (<= 0 i #x10ffff) (cond ((<= i 255)    (list i))
                               ((<= i #x07ff) (list (lead #b11000000 -6)
                                                    (follow           0)))
                               ((<= i #xffff) (list (lead #b11100000 -12)
                                                    (follow          -6)
                                                    (follow           0)))
                               (else          (list (lead #b11110000 -18)
                                                    (follow          -12)
                                                    (follow          -6)
                                                    (follow           0))))))

(define (read-error? d) (and (procedure? d) (not (read-eof? d))))
(define (read-eof? d)   (and (procedure? d) (eq? 'eof (d))))
(define (eof-object? d) (read-eof? d))
(define (eof) 'eof)

(define (char c)
  (define v (string->vector c))
  (and (= (vector-length v) 1) (vector-ref v 0)))
(define (char=? ch c) (equal? ch (char c)))

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
        (escape-code radix (lambda (n) (and n (integer->octets n)))))
      (define (escape ch)
        (cond ((assoc ch (map (lambda (kv) (cons (char (car kv))
                                                 (char (cdr kv))))
                              '(("a"  . "\a")
                                ("t"  . "\t")
                                ("n"  . "\n")
                                ("v"  . "\v")
                                ("f"  . "\f")
                                ("r"  . "\r")
                                ("e"  . "\e")
                                ("\"" . "\"")
                                ("\\" . "\\"))))
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
      (define nexp (expt radix (whole (or exp '()))))
      (define m (cond ((eq? (or frac-type 'dec) 'dec)
                       (+ nlhs (* nrhs (expt 10 (- (length (or rhs '())))))))
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
             (k/special '(("n" "N") ("f" "F")) +inf.0 +inf.f)))
          ((=? "n" "N") (k/special '(("a" "A") ("n" "N")) +nan.0 +nan.f))
          ((and (not ch) (not real)) (and lhs (make (lambda (n) n))))
          (else                      #f))))
