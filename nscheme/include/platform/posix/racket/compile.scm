;;; This file sets up a full compiler for the posix/racket platform.  This compiler can be used for
;;; cross-compilation, and when already running on posix/racket, also runtime compilation.

;primitive-env.runtime           is symbol-name => parser producing (E:quote primitive-value)  ; defined in primitive.rkt
;primitive-env.cross-compiling   is symbol-name => parser producing (E:ref address)            ; defined here
;global-addr=>id                 is address => symbol-name                                     ; also defined here

(define (name*->addr-package    name*) (cons name* (map (lambda (n) (make-address n #f)) name*)))
(define (addr-package->addr=>id pkg)   (map cons (cdr pkg) (car pkg)))

(define addr-package.posix/racket.primitive
  (name*->addr-package
    '(
      panic apply values
      eq? eqv? null? boolean? procedure? symbol? string? rational? integer?
      pair? vector? mvector? bytevector? mbytevector?
      string->symbol symbol->string
      cons car cdr
      vector vector-length vector-ref
      make-mvector mvector->vector mvector-slice mvector-length mvector-ref mvector-set!
      bytevector bytevector-length bytevector-ref
      make-mbytevector mbytevector->bytevector mbytevector-slice
      mbytevector-length mbytevector-ref mbytevector-set!
      bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
      bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length integer-floor-divmod
      numerator denominator = <= >= < > + - * /)))

(define addr-package.posix/racket.primitive.privileged
  (name*->addr-package
    '(
      panic-handler native-signal-handler
      procedure-metadata
      record? record record-type-descriptor record-ref
      string->bytevector bytevector->string)))

;; TODO: include platform-specific primitives
(define primitive-addr=>id.posix/racket
  (addr-package->addr=>id
    (package-append addr-package.primitive addr-package.primitive.privileged)))

(define env.posix/racket.primitive            (addr-package->env addr-package.primitive))
(define env.posix/racket.primitive.privileged (addr-package->env addr-package.primitive.privileged))


;; We only want two hand-written .rkt files in this project:
;; - bootstrap.rkt
;;   - builds a fresh posix/racket system without needing an existing nscheme system
;;   - this will contain only the Racket primitive implementations needed for bootstrap
;;     - the complete primitive implementations are only defined right here, in platform/posix/racket.scm
;;     - so, only a small amount of duplication
;; - test.rkt
;;   - validates just enough to have confidence in the bootstrapping process
;;   - other testing can happen elsewhere


;;; OLD, SEE ABOVE INSTEAD
;; Ultimately, we only want three hand-written .rkt files in this project:
;; - primitive.rkt
;;   - containing the necessary parts currently found in nscheme.rkt
;; - bootstrap.rkt  (maybe we should call this something else, depending on what it REALLY does (nscheme.rkt ?))
;;   - orchestrates the bootstrap build process for a fresh system
;;   - or maybe it should just started up the equivalent of posix/nscheme.rkt, and let the
;;     user decide what to do with it
;;     - leaning towards just doing the build for a fresh posix/racket system, and from there the user can do
;;       what they want
;; - test.rkt
;;   - validates just enough to have confidence in the bootstrapping process
;;   - other testing can happen elsewhere
;; We need to be able to carry the text in primitive.rkt forward for building new instances of posix/racket
;; - this is actually true of all the text files, and needs to be solved in a platform-specific way
;;   - e.g., posix understands directories and files, so one option is to expect such a system to read those directly
;;   - a www system won't have that option, so we could package up the text files as some data structure
;;   - and a "livingnet" system is different from www, but will solve this problem in a similar way
;; - though primitive.rkt specifically is required for cross-compiling to racket, so its text should be
;;   internalized on all platforms whenever posix/racket.scm is used ... chicken and egg problem unless we maintain
;;   two separate copies: one here, and one in bootstrap/
;;   - or, we could somehow include the copy from here during the bootstrap process, if we can
;;     minimize the dependencies we need for doing so... how is this possible though, since nscheme just won't
;;     work without its primitives?
;;   - maybe it's even a good idea to maintain two copies
;;     - it makes it possible to change how posix/racket.scm works without having to change the current system
;;       - no need to redo the entire bootstrap
;;     - also good for security of the bootstrap (trusting trust)
;; - so maybe no primitive.rkt at all, since the duplicate copy can live in bootstrap.rkt
;;   - and we'll just have the text for the would-be primitive.rkt embedded in a string/bytevector here
;;   - we don't even have to provide all the primitives in bootstrap.rkt, just enough for
;;     the bootstrap itself to succeed


;; provide global names for all of these too
;; - maybe env.global is a better name than env.primitive ?  not a big deal though
- Racket platform stdio, file, and tcp streams, compatible with text/stream.scm interface
  - implementation for these platform-specific streams should be generated as literal Racket code, not nScheme code
    - we manually link to it during bootstrap, but also automatically link to it when compiling to posix Racket
  - expose only a limited set of IO primitives to nScheme
    - e.g., just a high-level interface for using filesystem, network, console
    - no need for lower level primitives, since high-level controllers can produce other encapsulated controllers



we can also support nscheme syntax object annotations, converting to srcloc?
- have to recognize specially supported annotations for this to work, but could be helpful for debugging


;; NOTE: if making changes here, handle any overlap in: bootstrap/nscheme.rkt
(define primitive.rkt-text
  "

  stdio (via a console)
  filesystem
  network ; tcp udp
  subprocess
  shell-environment
  command-line-arguments
  - and system paths (bootstrapped from find-systme-path in Racket)
    - (find-system-path 'orig-dir) (find-system-path 'run-file) (find-system-path 'exec-file)
  tty (stty via subprocess? in which case it's not primitive, so we deal with it in include/posix/ instead)

  one
  two
  three
  ")

;; TODO: can we get away without using text for primitive.rkt?  keywords may still be necessary for open-input-file and friends
;; - remember, we can use Racket itself to write us a properly-escaped string that we can copy/paste into here
;; - or we can write some portions in text and others in s-exprs, using `write` on
;;   the s-exprs to turn them into text that we string-append

`(
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Shared with bootstrap ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require
    ;racket/control racket/file racket/flonum racket/list racket/match racket/port racket/pretty
    ;racket/string racket/struct racket/system racket/tcp racket/udp racket/vector
    (prefix-in rkt: racket/base))

  (define (eqv? a b)
    (or (rkt:eqv? a b)
        (if (string? a)
            (and (string? b) (string=? a b))
            (and (bytevector? a) (bytevector? b) (bytes=? a b)))))

  (define (rational? x) (and (rkt:rational? x) (exact? x)))
  (define (integer?  x) (rkt:exact-integer? x))

  (define (bitwise-arithmetic-shift-left  n k) (rkt:arithmetic-shift n    k))
  (define (bitwise-arithmetic-shift-right n k) (rkt:arithmetic-shift n (- k)))
  (define (bitwise-length                 n)   (integer-length n))

  (define (integer-floor-divmod dividend divisor)
    (assert (integer? dividend) (integer? divisor))
    (let ((q (rkt:floor (/ dividend divisor))))
      (values q (- dividend (* q divisor)))))

  ;; NOTE: these use keyword args, so embed these as rkt-text instead
  (struct mbytevector (bv) #:name mbytevector-struct #:constructor-name mbytevector:new #:prefab)
  (struct mvector (v) #:name mvector-struct #:constructor-name mvector:new #:prefab)
  (struct record (type-descriptor field*) #:name record-struct #:constructor-name record:new #:prefab)

  (define (record rtd . x*)
    (unless (and (vector? rtd) (< 0 (vector-length rtd)))
      (error "record-type-descriptor is not a non-empty vector" rtd))
    (let ((field-count (vector-ref rtd 0)))
      (unless (fixnum? field-count) (error "not a field count" field-count))
      (unless (= (length x*) field-count) (error "incorrect record argument count" field-count x*))
      (record:new rtd (list->vector x*))))

  (define (record-ref x i)
    (unless (record? x) (error "not a record" x))
    (vector-ref (record-field* x) i))

  (define (make-mvector    len x)  (mvector:new   (make-vector len x)))
  (define (mvector-length  mv)     (vector-length (mvector-v mv)))
  (define (mvector-ref     mv i)   (vector-ref    (mvector-v mv) i))
  (define (mvector-set!    mv i x) (vector-set!   (mvector-v mv) i x))
  (define (mvector-slice mv start count)
    (mvector:new (vector-copy (mvector-v mv) start (+ start count))))
  (define mvector->vector
    (case-lambda
      ((mv)             (vector-copy (mvector-v mv)))
      ((mv start count) (vector-copy (mvector-v mv) start (+ start count)))))

  (define (bytevector->string bv) (bytes->string/utf-8 bv))
  (define (string->bytevector bv) (string->bytes/utf-8 bv))

  (define (bytevector        . x*) (apply bytes x*))
  (define (bytevector?       x)    (bytes?       x))
  (define (bytevector-length bv)   (bytes-length bv))
  (define (bytevector-ref    bv i) (bytes-ref bv i))

  (define (make-mbytevector        len n)   (mbytevector:new (make-bytes len n)))
  (define (mbytevector-length      mbv)     (bytevector-length (mbytevector-bv mbv)))
  (define (mbytevector-ref         mbv i)   (bytevector-ref    (mbytevector-bv mbv) i))
  (define (mbytevector-set!        mbv i n) (bytes-set!        (mbytevector-bv mbv) i n))
  (define (mbytevector-slice mbv start count)
    (mbytevector:new (subbytes (mbytevector-bv mbv) start (+ start count))))
  (define mbytevector->bytevector
    (case-lambda
      ((mbv)             (bytes-copy (mbytevector-bv mbv)))
      ((mbv start count) (subbytes   (mbytevector-bv mbv) start (+ start count)))))

  (define (make-parameter default)
    (let ((rkt-param (rkt:make-parameter default)))
      (case-lambda
        (()                (rkt-param))
        ((new-value thunk) (parameterize ((rkt-param new-value)) (thunk))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Not shared with bootstrap ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (rkt-eval rkt) (eval rkt (make-base-namespace)))
  (define (rkt-text-eval rkt-text)
    ((cond ((bytes? rkt-text) call-with-input-bytes)
           ((string? rkt-text) call-with-input-string)
           (else (error "not rkt-text" rkt-text)))
     rkt-text
     (lambda (in)
       (let ((rkt (read in)))
         (when (eof-object? rkt) (error "empty rkt-text" rkt-text))
         (unless (eof-object? (read in)) (error "rkt-text contains more than one form" rkt-text))
         (rkt-eval rkt)))))
  )
