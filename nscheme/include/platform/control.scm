(define package.control
  (cons
    '(
      make-parameter current-custodian make-custodian custodian-shutdown-all
      current-thread thread thread/suspend-to-kill call-in-nested-thread
      thread-suspend thread-resume thread-kill thread-wait
      sleep
      make-channel channel-get channel-put
      )
    (list
      make-parameter current-custodian make-custodian custodian-shutdown-all
      current-thread thread thread/suspend-to-kill call-in-nested-thread
      thread-suspend thread-resume thread-kill thread-wait
      sleep
      make-channel channel-get channel-put
      )))
