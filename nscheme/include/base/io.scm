(define current-input-port  (make-parameter (thread-safe-iport standard-input-port)))
(define current-output-port (make-parameter (thread-safe-oport standard-output-port)))
(define current-error-port  (make-parameter (thread-safe-oport standard-error-port)))

(define newline
  (let ((go (lambda (port) (oport-write-byte port 10))))
    (case-lambda
      (()     (go (current-output-port)))
      ((port) (go port)))))

(splicing-local
  ((define (make-write printer->layout newline?)
     (define (go x port) (let ((p (printer:port port)))
                           (notate (writer:layout (printer->layout p)) x)
                           (when newline? (printer-newline p))))
     (case-lambda
       ((x)      (go x (current-output-port)))
       ((x port) (go x port))))
   (define (make-write/notate printer->layout newline?)
     (define (go notate x port) (let ((p (printer:port port)))
                                  (notate (writer:layout (printer->layout p)) x)
                                  (when newline? (printer-newline p))))
     (case-lambda
       ((notate x)      (go notate x (current-output-port)))
       ((notate x port) (go notate x port)))))
  (define write                (make-write        layout:single-line                 #f))
  (define writeln              (make-write        layout:single-line                 #t))
  (define pretty-write         (make-write        (lambda (p) (layout:pretty  p 78)) #t))
  (define compact-write        (make-write        (lambda (p) (layout:compact p 78)) #t))
  (define write/notate         (make-write/notate layout:single-line                 #f))
  (define writeln/notate       (make-write/notate layout:single-line                 #t))
  (define pretty-write/notate  (make-write/notate (lambda (p) (layout:pretty  p 78)) #t))
  (define compact-write/notate (make-write/notate (lambda (p) (layout:compact p 78)) #t)))
