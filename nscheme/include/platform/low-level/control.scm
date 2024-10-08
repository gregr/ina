;; These primitives will be used to define high level control facilities on platforms that do not
;; provide such facilities directly.
(define package.low-level:control
  (cons
    '(
      native-thread-local-value with-raw-escape-prompt raw-escape-to-prompt
      current-raw-coroutine make-raw-coroutine
      timer-interrupt-handler set-timer enable-interrupts disable-interrupts)
    (list
      native-thread-local-value with-raw-escape-prompt raw-escape-to-prompt
      current-raw-coroutine make-raw-coroutine
      timer-interrupt-handler set-timer enable-interrupts disable-interrupts)))
