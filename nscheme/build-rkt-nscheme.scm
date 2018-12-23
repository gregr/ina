(require program-path write/file)

(define out-path
  (reverse (cons 'nscheme.scm.rkt (cdr (reverse program-path)))))

(write/file out-path 'TODO:generate-this)
