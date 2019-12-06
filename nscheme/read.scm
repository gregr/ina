;; TODO: move these
(define (string->list s) (vector->list (string->vector s)))
(define (list->string cs) (vector->string (list->vector cs)))

(define (read-error? d) (and (procedure? d) (not (read-eof? d))))
(define (read-eof? d)   (and (procedure? d) (eq? 'eof (d))))
(define (eof-object? d) (read-eof? d))
(define (eof) 'eof)

(define (char c)
  (define v (string->vector c))
  (and (= (vector-length v) 1) (vector-ref v 0)))
(define (char=? ch c) (equal? ch (char c)))

;; TODO: include these? 133 8232 8233
(define linebreaks (map char '("\n" "\r")))
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
    (define (=? . cs)             (ormap (lambda (c) (char=? ch c)) cs))
    (define (in-range? start end) (<= (char start) ch (char end)))
    (define (Comment:line k)
      ;; TODO: allow \NL \CRNL \CR to elide linebreak and continue comment.
      (cond ((member (peek) linebreaks) (Datum k:dot k:delim k))
            (else                       (next) (Comment:line k))))
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
             (Number (list ch (char "#")) k))
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
              ;; TODO:
              ;((char=? ch "u")  ;; \u[bodx]...;
               ;)
              ;((member ch (string->list "bBoOdDxX"))
               ;)
              ((char=? ch "\n") (loop (next) acc))
              ((char=? ch "\r") (define ch (next))
                                (loop (if (char=? ch "\n") (next) ch) acc))
              (else (k:fail ch))))
      (cond ((char=? ch "\"") (k (rlist->string acc)))
            ((char=? ch "\\") (escape (next)))
            (else             (loop (next) (cons ch acc))))))

  (define (Number acc k)
    (let loop ((acc acc))
      (cond ((member (peek) separators)
             (define s (rlist->string acc))
             (define num (string->number s))
             (k (if num num (k:error (list "invalid number" s)))))
            (else (loop (cons (next) acc))))))
  (define (Number/Symbol acc k)
    (let loop ((acc acc))
      (define ch (peek))
      (cond ((member ch separators) (define s (rlist->string acc))
                                    (define num (string->number s))
                                    (k (if num num (string->symbol s))))
            ;; TODO: \ and || escapes
            (else                   (loop (cons (next) acc))))))

  (Datum k:fail (lambda (delim) (if delim (k:fail delim) (k:eof)))
         (lambda (d) d)))

(define (rlist->string cs) (list->string (reverse cs)))

;; TODO: string->number
