(splicing-local
  ((define (command* path x*)
     (call/oport:bytevector
       (lambda (out)
         (let ((in (iport:file "/dev/tty")))
           (posix-process-wait (posix-process in out out (find-file path) x*))
           (iport-close in)))))
   (define (command path . x*) (command* path x*)))
  (define (tput        x)     (command #"tput" x))
  (define (tput-number x)     (utf8->number (bytevector-rtrim1 (tput x) 10)))
  (define (terminal-lines)    (tput-number #"lines"))
  (define (terminal-columns)  (tput-number #"cols"))
  (define (stty . x*)         (command* #"stty" x*))
  (define (stty-ref)          (stty #"-g"))
  (define (stty-set settings) (stty settings))
  (define (stty-raw)          (stty #"raw")))
