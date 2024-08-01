#lang racket/base

;;; It seems that POSIX signals are only sent to the main thread.

(define ((work/breaks name latency))
  (parameterize-break
   #f
   (let loop ()
     (with-handlers ((exn:break?
                      (lambda (exn)
                        (displayln `(,name caught a ,exn)))))
       (displayln name)
       (parameterize-break #t (sleep latency)))
     (loop))))

(thread (work/breaks 't1 3))
(thread (work/breaks 't2 4))

(parameterize-break
 #f
 (let loop ()
   (unless (eof-object?
            (with-handlers ((exn:break?
                             (lambda (exn)
                               (displayln `(main caught a ,exn)))))
              (parameterize-break #t (read))))
     (loop))))
