(define (option-attribute* o) (filter (lambda (entry) (not (procedure? entry))) o))

(define (options->description o*)
  (map (lambda (o)
         (let* ((o    (option-attribute* o))
                (args (bytevector-join* #" " (map text->bytevector (alist-ref o 'arguments '())))))
           (bytevector-append
             (bytevector-join* #", " (map (lambda (flag) (bytevector-append flag #" " args))
                                          (map text->bytevector (alist-ref o 'flags '()))))
             #"\n  " (bytevector-join* #"\n" (map text->bytevector (alist-ref o 'description '()))))))
       o*))

(define (options->dispatch o*)
  (let ((flag=>handle
          (append*
            (map (lambda (o)
                   (let* ((attr*  (option-attribute* o))
                          (flag*  (alist-ref 'flags attr*))
                          (arg*   (alist-ref 'arguments attr*))
                          (argc   (length arg*))
                          (handle (or (memp procedure? o)
                                      (mistake 'options->dispatch "option has no handler procedure" o)))
                          (handle (car handle))
                          (handle (lambda (arg*)
                                    (let ((x* (take argc arg*))))
                                    (unless (= (length x*) argc)
                                      (raise-error (list 'options->dispatch "not enough arguments for option" o arg*)))
                                    (apply handle (append x*) (list (drop argc arg*))))))
                     (map (lambda (flag) (cons flag handle)) flag*)))
                 o*))))
    (lambda (arg* kempty)
      (if (null? arg*)
          (kempty)
          ((alist-ref flag=>handle (car arg*) (lambda (_) (kempty))) (cdr arg*))))))

;; TODO: better command-line argument parsing
;(define-values
;  (interact? program-path)
;  (let ((arg* (cdr cli-arg*)))
;    (values
;      (not (not (or (memv #"-i" arg*) (memv #"--interact" arg*))))
;      (let ((arg* (filter (lambda (x) (not (memv x '(#"-i" #"--interact")))) arg*)))
;        (and (pair? arg*)
;             (car arg*))))))

;; may include multiple uses of -c to generate code for multiple targets
;-c --compile TARGET-BACKEND FILE   ; file may be - to write to stdout
