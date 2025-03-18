(define (option-attribute* o) (filter (lambda (entry) (not (procedure? entry))) o))
(define (options->description o*)
  (map (lambda (o)
         (let* ((o    (option-attribute* o))
                (arg* (alist-ref/default o 'arguments '()))
                (args (bytevector-join* #" " (map text->bytevector arg*))))
           (bytevector-append
             #"  "
             (bytevector-join*
               #", " (map (lambda (flag) (if (null? arg*) flag (bytevector-append flag #" " args)))
                          (map text->bytevector (alist-ref o 'flags))))
             #"\n    "
             (bytevector-join* #"\n" (map text->bytevector (alist-ref/default o 'description '()))))))
       o*))
(define (options->dispatch o*)
  (let ((flag=>handle
          (append*
            (map (lambda (o)
                   (let* ((attr*  (option-attribute* o))
                          (flag*  (map text->bytevector (alist-ref attr* 'flags)))
                          (arg*   (alist-ref/default attr* 'arguments '()))
                          (argc   (length arg*))
                          (handle (or (memp procedure? o)
                                      (mistake 'options->dispatch
                                               "option has no handler procedure" o)))
                          (handle (car handle))
                          (handle
                            (lambda (arg*)
                              (let ((x* (take argc arg*)))
                                (unless (= (length x*) argc)
                                  (raise-error
                                    (list 'options->dispatch "not enough arguments for option" o
                                          'arguments arg*)))
                                (apply handle (append x* (list (drop argc arg*))))))))
                     (map (lambda (flag) (cons flag handle)) flag*)))
                 o*))))
    (lambda (arg* kempty)
      (if (null? arg*)
          (kempty)
          ((alist-ref/default flag=>handle (car arg*) (lambda (_) (kempty))) (cdr arg*))))))
