((require program-path printf file-exists? write/file))

(define out-path
  (reverse (cons 'nscheme.scm.rkt (cdr (reverse program-path)))))

(define out-path-string
  (apply string-append
         (cons (car out-path) (map (lambda (s) (string-append '"/" s))
                                   (cdr out-path)))))

(when (file-exists? out-path)
  (printf '"~s already exists; remove it to rebuild it.\n" out-path-string))

(unless (file-exists? out-path)
  (write/file out-path 'TODO:generate-this)
  (printf '"Finished building: ~s\n" out-path-string))
