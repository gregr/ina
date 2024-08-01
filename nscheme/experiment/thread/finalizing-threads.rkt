#lang racket/base

(define (with-local-custodian ^body)
  (let ((cust (make-custodian)))
    (parameterize ((current-custodian cust))
      (let ((t (current-thread)))
        (thread (lambda () (thread-wait t) (displayln 'here) (custodian-shutdown-all cust))))
      (dynamic-wind
       (lambda () (void))
       ^body
       (lambda ()
         (displayln 'here???)
         (custodian-shutdown-all cust))))))

(define (with-cancel-group proc)
  (with-local-custodian
   (lambda ()
     (let* ((result* (list (void)))
            (t (thread (lambda ()
                         (let ((t (current-thread)))
                           (parameterize ((current-thread-group (make-thread-group)))
                             (set! result* (call-with-values (lambda () (proc (lambda () (displayln 'killing) (kill-thread t) (displayln 'killed)))) list))))))))
       (thread-wait t)
       (apply values result*)))))

(define (make-a-mess)
  (with-cancel-group
   (lambda (cancel)
     (thread (lambda () (let loop () (displayln 'mess-1) (sleep 5) (loop))))
     (thread (lambda () (let loop () (displayln 'mess-2) (sleep 6) (loop))))
     (thread (lambda () (let loop () (displayln 'mess-3) (sleep 10) (displayln 'cancelling) (cancel))))
     (let loop ()
       (displayln 'maintaining-mess)
       (sleep 7)
       (loop)))))

;(define mess (thread make-a-mess))
;(sleep 20)
;(kill-thread mess)
;(displayln 'killed-mess)
;(sleep 15)

(define mess (thread make-a-mess))

;(sleep 8)
;(displayln 'killing-mess)
;(kill-thread mess)
(thread-wait mess)

(let loop ()
  (displayln 'done)
  (sleep 5)
  (loop))
