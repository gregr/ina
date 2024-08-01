#lang racket/base
(require racket/engine racket/match)

(struct thread-manager (t.handler ch.command ch.off ch.on))

(define (make-thread-manager)
  (let ((ch.command (make-channel)) (ch.off (make-channel)) (ch.on (make-channel)))
    (define (command cmd) (if cmd (on) (off)))
    (define (on)
      (displayln "manager is on")
      (sync (handle-evt (channel-put-evt ch.on (void))
                        (lambda (_) (on)))
            (handle-evt ch.command command)))
    (define (off)
      (displayln "manager is off")
      (sync (handle-evt (channel-put-evt ch.off (void))
                        (lambda (_) (off)))
            (handle-evt ch.command command)))
    (thread-manager (thread/suspend-to-kill on) ch.command ch.off ch.on)))

(define (thread-manager-off tm)
  (thread-resume (thread-manager-t.handler tm) (current-thread))
  (channel-put (thread-manager-ch.command tm) #f))

(define (thread-manager-on tm)
  (thread-resume (thread-manager-t.handler tm) (current-thread))
  (channel-put (thread-manager-ch.command tm) #t))

(define (make-thread tm proc)
  (match-define (thread-manager t.handler _ ch.off ch.on) tm)
  (let ((eng (engine (lambda (_) (proc)))))
    (thread
     (lambda ()
       (let ((self (current-thread)))
         (thread-resume t.handler self)
         (let loop ()
           (sync ch.on)
           ;; If we do not wrap the engine-run call with call-in-nested-thread
           ;; (or some other mediating thread), and the outer thread is
           ;; suspended, the call to engine-run will itself be affected by the
           ;; suspend, and will no longer be watching for the stop event.  This
           ;; means the engine will keep running when it should be suspended.
           (unless (call-in-nested-thread
                    ;; NOTE: we don't have to use an engine.  It is possible to instead use a
                    ;; thread that we explicitly suspend whenever
                    ;; (choice-evt (thread-suspend-evt self) ch.off) occurs.
                    (lambda ()
                      (engine-run (choice-evt (thread-suspend-evt self) ch.off)
                                  eng)))
             (loop))))))))

(define ((work name latency))
  (let loop ()
    (displayln name)
    (sleep latency)
    (loop)))

(define tm (make-thread-manager))
(define t1 (make-thread tm (work 't1 1)))
(define t2 (make-thread tm (work 't2 2)))
(define t3 (make-thread tm (work 't3 3)))

(void
 (thread
  (lambda ()
    (let loop ()
      (displayln "will suspend t3 soon")
      (sleep 10)
      (thread-suspend t3)
      (displayln "suspended t3")
      (displayln "will resume t3 soon")
      (sleep 10)
      (displayln "resuming t3")
      (thread-resume t3)
      (loop)))))

(displayln "listening for on/off commands")
(let loop ()
  (match (read)
    ((? eof-object?) (void))
    ('off
     (displayln "turning manager off")
     (thread-manager-off tm)
     (loop))
    ('on
     (displayln "turning manager on")
     (thread-manager-on tm)
     (loop))))
