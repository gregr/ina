(define (with-panic-finalizer finalize thunk)
  (let-values ((x* (current-panic-handler (lambda _ (finalize)) thunk)))
    (finalize)
    (apply values x*)))
(define (with-stty-fresh thunk)
  (let ((settings (stty-ref)))
    (display (bytevector-append csi:cursor-save
                                csi:display-save
                                csi:display-clear-screen
                                (csi:cursor-move 1 1)))
    (with-panic-finalizer
     (lambda ()
       (display (bytevector-append csi:display-restore csi:cursor-restore))
       (stty-set settings))
     thunk)))
(define (with-cursor-hidden thunk)
  (display csi:cursor-hide)
  (with-panic-finalizer (lambda () (display csi:cursor-show)) thunk))

(define (display/sgr   sgr s) (display sgr) (display s) (display sgr:reset))
(define (displayln/sgr sgr s) (display/sgr sgr s) (display "\r\n"))
(with-stty-fresh
 (lambda ()
   (with-cursor-hidden
    (lambda ()
      (let ((sgr.0 (make-sgr))
            (sgr.1 (make-sgr sgra:color-simple-fg:magenta
                             sgra:color-simple-bg:yellow
                             sgra:bold+
                             sgra:blink+))
            (sgr.2 (make-sgr sgra:color-simple-fg:green
                             sgra:color-simple-bg:red
                             sgra:underline+
                             sgra:invert+))
            (sgr.3 (make-sgr (sgra:color-6cube-fg 3 4 5)
                             (sgra:color-24gray-bg 7)))
            (sgr.4 (make-sgr (sgra:color-6cube-fg 0 0 3)
                             (sgra:color-6cube-bg 2 2 0)
                             sgra:invert+
                             sgra:blink+
                             sgra:underline+)))
        (stty-raw)
        (display "\r\n")
        (displayln/sgr sgr.0 "testing (1 2 3)")
        (displayln/sgr sgr.1 "testing (1 2 3)")
        (displayln/sgr sgr.2 "testing (1 2 3)")
        (displayln/sgr sgr.3 "testing (1 2 3)")
        (displayln/sgr sgr.4 "testing (1 2 3)")
        (displayln/sgr sgr.0 "testing (1 2 3)")
        (displayln/sgr sgr.0 "\r\nHit any key...")
        (iport-read-byte (current-input-port)))))))
