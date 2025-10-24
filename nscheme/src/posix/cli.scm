(define usage-description
  (let ((go (lambda (path.program arg-description options)
              (bytes-join*
                #"\n"
                (cons (bytes-append #"usage: " (path->bytes path.program) #" "
                                    (text->bytes arg-description))
                      (if options
                          (cons #"options:" (options->description options))
                          '()))))))
    (case-lambda
      ((path.program arg-description)         (go path.program arg-description #f))
      ((path.program arg-description options) (go path.program arg-description options)))))

(define (option-attribute* o) (filter (lambda (entry) (not (procedure? entry))) o))
(define (options->description o*)
  (map (lambda (o)
         (let* ((o    (option-attribute* o))
                (arg* (alist-ref/default o 'arguments '()))
                (args (bytes-join* #" " (map text->bytes arg*))))
           (bytes-append
             #"  "
             (bytes-join*
               #", " (map (lambda (flag) (if (null? arg*) flag (bytes-append flag #" " args)))
                          (map text->bytes (alist-ref o 'flags))))
             #"\n    "
             (bytes-join* #"\n    " (map text->bytes (alist-ref/default o 'description '()))))))
       o*))
(define (options->dispatch o*)
  (let ((flag=>handle
          (append*
            (map (lambda (o)
                   (let* ((attr*  (option-attribute* o))
                          (flag*  (map text->bytes (alist-ref attr* 'flags)))
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
