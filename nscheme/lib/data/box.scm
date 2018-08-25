(provide
  box
  unbox
  set-box!)

(define (box d)         (make-mvector 1 d))
(define (unbox mv)      (mvector-ref mv 0))
(define (set-box! mv d) (mvector-set! mv 0 d))
