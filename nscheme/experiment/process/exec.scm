;; gcc -fPIC -shared -o exec.so exec.c && chez --script exec.scm && rm exec.so
(load-shared-object #f)
(load-shared-object "exec.so")
(define show-error
  (let ((fp0 (foreign-procedure "get_errno" () int))
        (fp1 (foreign-procedure "get_error" () string)))
    (lambda ()
      (write `(errno: ,(fp0)))
      (newline)
      (write (fp1))
      (newline))))
(define fork (foreign-procedure "fork" () int))
;; exec-ls works fine because the variadic call is on the other side of the C boundary.
(define exec-ls (foreign-procedure "exec_ls" () int))
(define exec1 (foreign-procedure "exec1" (string) int))
(define exec2 (foreign-procedure "exec2" (string string) int))
;; Regardless of whether we fork or disable-interrupts, all of these execl variants greater than 1
;; fail with either:
;; - errno: 14, "Bad Argument"
;; - or with some form of memory error that loses debug context
;; Since the non-variadic execv seems to work fine, I suspect it's an issue with variadic arguments.
;; Maybe the variadic argument calling convention on Mac ARM is broken.
(define execl1
  (let ((fp (foreign-procedure "execl" (string void*) int)))
    (lambda (a) (fp a 0))))
(define execl2
  (let ((fp (foreign-procedure "execl" (string string void*) int)))
    (lambda (a b) (fp a b 0))))
(define execl3
  (let ((fp (foreign-procedure "execl" (string string string void*) int)))
    (lambda (a b c) (fp a b c 0))))
(define execl4
  (let ((fp (foreign-procedure "execl" (string string string string void*) int)))
    (lambda (a b c d) (fp a b c d 0))))
(define execl1x
  (let ((fp (foreign-procedure "execl" (void* void*) int)))
  ;(let ((fp (foreign-procedure "execl" ((* integer-8) void*) int)))
    (lambda (a) (fp a 0))))
(define execl2x
  (let ((fp (foreign-procedure "execl" (void* void* void*) int)))
  ;(let ((fp (foreign-procedure "execl" ((* integer-8) (* integer-8) void*) int)))
    (lambda (a b) (fp a b 0))))
(define execv
  (let ((fp (foreign-procedure "execv" (string void*) int)))
    (lambda (path . x*)
      (let* ((x*  (map string->utf8 x*))
             (len (length x*))
             (size.iptr  (foreign-sizeof 'iptr))
             (size.array (* size.iptr (+ len 1)))
             (size.bytes (+ (fold-left + 0 (map bytevector-length x*)) len))
             (size       (+ size.array size.bytes))
             (mem        (foreign-alloc size)))
        ;(write (list len size.array size.bytes size)) (newline)
        (foreign-set! 'iptr mem (* (foreign-sizeof 'iptr) len) 0)  ; probably not necessary
        (let loop ((i 0) (j size.array) (x* x*))
          (unless (null? x*)
            (let* ((x   (car x*))
                   (len (bytevector-length x)))
              (foreign-set! 'iptr mem (* size.iptr i) (+ mem j))
              (foreign-set! 'unsigned-8 mem (+ j len) 0)  ; probably not necessary
              (let loop ((i 0))
                (when (< i len)
                  (foreign-set! 'unsigned-8 mem (+ j i) (bytevector-u8-ref x i))
                  (loop (+ i 1))))
              (loop (+ i 1) (+ j len 1) (cdr x*)))))
        (fp path mem)))))

;(write (foreign-sizeof  'iptr))       (newline)
;(write (foreign-alignof 'iptr))       (newline)
;(write (foreign-sizeof  'unsigned-8)) (newline)
;(write (foreign-alignof 'unsigned-8)) (newline)

(let ()
;(disable-interrupts)
;(write (execl1 "/bin/ls")) (show-error) (newline)
(let ((pid
        0
        ;(fork)
        ))
  (if (= pid 0)
      (begin
        ;(let ((d (foreign-alloc 8)))
        ;  (foreign-set! 'integer-8 d 0 47)  ; /
        ;  (foreign-set! 'integer-8 d 1 98)  ; b
        ;  (foreign-set! 'integer-8 d 2 105) ; i
        ;  (foreign-set! 'integer-8 d 3 110) ; n
        ;  (foreign-set! 'integer-8 d 4 47)  ; /
        ;  (foreign-set! 'integer-8 d 5 108) ; l
        ;  (foreign-set! 'integer-8 d 6 115) ; s
        ;  (foreign-set! 'integer-8 d 7 0)
        ;  (write (execl1x d))
        ;  ;(write (execl2x d d))
        ;  )
        ;(write (execv "/usr/bin/printenv" "/usr/bin/printenv"))
        ;(write (execv "/bin/ls"))
        ;(write (execv "/bin/ls" "/bin/ls"))
        (write (execv "/bin/ls" "/bin/ls" "-l" "-h"))
        ;(write (exec-ls))
        ;(write (exec1 "/bin/ls"))
        ;(write (exec2 "/bin/ls" "-l"))
        ;(write (execl1 "/bin/ls"))
        ;(write (execl2 "/bin/ls" "/bin/ls"))
        ;(write (execl4 "/bin/sh" "/bin/sh" "-c" "echo test"))
        (newline)
        (show-error))
      (begin
        (write `(forked: ,pid))
        (newline))))
;(write (execl3 "/bin/ls" "/bin/ls" "/")) (show-error)
;(enable-interrupts)
)
