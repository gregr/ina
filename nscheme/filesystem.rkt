#lang racket/base
(provide
  local-path
  library-path
  s->ns
  ns->s
  read/file
  write/file
  )

(require
  racket/path
  racket/vector
  )

(define (local-path rpath)
  (build-path (path-only (path->complete-path (find-system-path 'run-file)))
              rpath))
(define (library-path library-name module-name)
  (local-path
    (build-path "lib" (symbol->string library-name)
                (string-append (symbol->string module-name) ".scm"))))

(define (s->ns d)
  (cond ((symbol? d) (symbol->string d))
        ((pair? d) (cons (s->ns (car d)) (s->ns (cdr d))))
        ((vector? d) (vector-map s->ns d))
        (else d)))

(define (ns->s d)
  (cond ((string? d) (string->symbol d))
        ((pair? d) (cons (ns->s (car d)) (ns->s (cdr d))))
        ((vector? d) (vector-map ns->s d))
        (else d)))

(define (ns->s/write d)
  (cond ((string? d) (string->symbol d))
        ((pair? d) (cons (ns->s (car d)) (ns->s (cdr d))))
        ((vector? d) (vector-map ns->s d))
        ((or (boolean? d) (null? d) (number? d)) d)
        (else (error "cannot write:" d))))

(define (read/file path)
  (call-with-input-file
    path (lambda (in) (let loop ((rbody '()))
                        (define datum (ns->s (read in)))
                        (if (eof-object? datum) (reverse rbody)
                          (loop (cons datum rbody)))))))
(define (write/file path d)
  (call-with-output-file path (lambda (out) (write (ns->s/write d) out))))
