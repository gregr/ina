(define newline
  (let ((go (lambda (port) (oport-write-byte port 10))))
    (case-lambda
      (()     (go (current-output-port)))
      ((port) (go port)))))

(splicing-local
  ((define ((make-write/notate printer->layout newline?) notate)
     (define (go x port) (let ((p (printer:port port)))
                           (notate x (writer:layout (printer->layout p)))
                           (when newline? (printer-newline p))))
     (case-lambda
       ((x)      (go x (current-output-port)))
       ((x port) (go x port)))))
  (define write/notate         (make-write/notate layout:single-line                 #f))
  (define writeln/notate       (make-write/notate layout:single-line                 #t))
  (define pretty-write/notate  (make-write/notate (lambda (p) (layout:pretty  p 78)) #t))
  (define compact-write/notate (make-write/notate (lambda (p) (layout:compact p 78)) #t))
  (define write                (write/notate         notate))
  (define writeln              (writeln/notate       notate))
  (define pretty-write         (pretty-write/notate  notate))
  (define compact-write        (compact-write/notate notate)))

(define display
  (let ((go (lambda (x port)
              (if (text? x)
                  (oport-write-bytevector port (text->bytevector x))
                  (write x port)))))
    (case-lambda
      ((x)      (go x (current-output-port)))
      ((x port) (go x port)))))
(define displayln
  (let ((go (lambda (x port) (display x port) (newline port))))
    (case-lambda
      ((x)      (go x (current-output-port)))
      ((x port) (go x port)))))

(define (read/reader:data reader:data)
  (let ((go (lambda (port) ((read/reader:data/k reader:data) port raise values values))))
    (case-lambda
      (()     (go (current-input-port)))
      ((port) (go port)))))
(define (read*/reader:data reader:data)
  (let ((go (lambda (port) ((read*/reader:data/k reader:data) port raise values))))
    (case-lambda
      (()     (go (current-input-port)))
      ((port) (go port)))))
(define read (read/reader:data reader:data))
(define read* (read*/reader:data reader:data))
