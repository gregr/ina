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
          (lambda (x) (error "read failure:" x)) (lambda () eof)
          (lambda (d) d)))

(define (read/k peek next k:fail k:eof k)
  (define (Datum ch k:dot k:delim k)
    (define (=? . cs)             (ormap (lambda (c) (char=? ch c)) cs))
    (define (in-range? start end) (<= (char start) ch (char end)))
    (define (Comment:line ch k)
      ;; TODO: allow \NL \CRNL \CR to elide linebreak and continue comment.
      (cond ((member ch linebreaks) (Datum ch k:dot k:delim k))
            (else                   (Comment:line (next) k))))
    (define (Comment:line/eof targets)
      (if (null? targets) (k:delim #f)
        (let ((ch (next)))
          (cond ((char=? ch (car targets)) (Comment:line/eof (cdr targets)))
                (else                      (Comment:line ch k))))))
    (define (Comment:datum ch k)
      (Datum ch k:fail k:fail (lambda (_) (Datum (next) k:dot k:delim k))))
    (define (Comment:block ch k)
      (let loop ((ch ch) (level 1))
        (define (=2? c1 c2) (and (char=? ch c1) (char=? (next) c2)))
        (cond ((= level 0)   (Datum ch k:dot k:delim k))
              ((not ch)      (k:eof))
              ((=2? "#" "|") (loop (next) (+ level 1)))
              ((=2? "|" "#") (loop (next) (- level 1)))
              (else          (loop (next)    level)))))
    (define (Dot dot k)
      (cond ((member (peek) separators) (k:dot dot))
            (else                       (Number/Symbol (list dot) k))))

    (cond ((member ch spaces) (Datum (next) k:dot k:delim k))
          ((=? ";")  (Comment:line                    (next) k))
          ((=? "#")  (Hash                            (next) k))
          ((=? "(")  (List ")"                        (next) k))
          ((=? "[")  (List "]"                        (next) k))
          ((=? "{")  (List "}"                        (next) k))
          ((=? "'")  (Tag  'quote                     (next) k))
          ((=? "`")  (Tag  'quasiquote                (next) k))
          ((=? ",")  (Tag@ 'unquote 'unquote-splicing (next) k))
          ((=? "\"") (String                          (next) k))
          ((=? ".")  (Dot                             ch     k))
          ((or (not ch) (=? ")" "]" "}")) (k:delim ch))
          (else                           (Number/Symbol (list ch) k))))

  (define (Hash ch k)
    (define (=? . cs) (ormap (lambda (c) (char=? ch c)) cs))
    ;; TODO: #<<EOS
    ;; ...string...
    ;; EOS
    (cond ((=? "t" "T") (k #t))
          ((=? "f" "F") (k #f))
          ((=? "!") (Comment:line/eof '("e" "o" "f")))
          ((=? ";") (Comment:datum                       (next) k))
          ((=? "|") (Comment:block                       (next) k))
          ((=? "(") (Vector ")"                          (next) k))
          ((=? "[") (Vector "]"                          (next) k))
          ((=? "{") (Vector "}"                          (next) k))
          ((=? "'") (Tag    'syntax                      (next) k))
          ((=? "`") (Tag    'quasisyntax                 (next) k))
          ((=? ",") (Tag@   'unsyntax 'unsyntax-splicing (next) k))
          ((=? "i" "I" "e" "E" "b" "B" "d" "D" "o" "O" "x" "X")
            (Number (list ch (char "#")) k))
          (else (k:fail ch))))
  (define (Sequence dot-allowed? delim ch k)
    (let loop ((ch ch) (data '()))
      (define (Delimiter last ch)
        (if (char=? ch delim) (k (foldl cons last data)) (k:fail ch)))
      (define (k:dot dot)
        (cond (dot-allowed? (Datum (next) k:fail k:fail
                                   (lambda (datum) (Delimiter datum (next)))))
              (else         (k:fail dot))))
      (Datum ch k:dot (lambda (ch) (Delimiter '() ch))
             (lambda (datum) (loop (next) (cons datum data))))))
  (define (List   delim ch k) (Sequence #t delim ch k))
  (define (Vector delim ch k) (Sequence #f delim ch
                                        (lambda (ds) (k (list->vector ds)))))
  (define (Tag tag ch k)
    (Datum ch k:fail k:fail (lambda (d) (k (list tag d)))))
  (define (Tag@ tag tag@ ch k)
    (cond ((char=? ch "@") (Tag tag@ (next) k))
          (else            (Tag tag  ch     k))))

  (define (String ch k)
    (let loop ((ch ch) (acc '()))
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
             (k (if num num (k:fail (list "invalid number" s)))))
            (else (loop (cons (next) acc))))))
  (define (Number/Symbol acc k)
    (let loop ((acc acc))
      (define ch (peek))
      (cond ((member ch separators) (define s (rlist->string acc))
                                    (define num (string->number s))
                                    (k (if num num (string->symbol s))))
            ;; TODO: \ and || escapes
            (else                   (loop (cons (next) acc))))))

  (Datum (next) k:fail (lambda (delim) (if delim (k:fail delim) (k:eof)))
         (lambda (d) d)))

(define (rlist->string cs) (list->string (reverse cs)))

;; TODO: string->number
