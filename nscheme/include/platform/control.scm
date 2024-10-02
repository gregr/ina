(define package.control
  (cons
    '(
      make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
      current-thread-group make-thread-group current-thread thread thread/suspend-to-kill
      thread-resume thread-wait thread-resume-evt thread-suspend-evt thread-dead-evt
      make-channel channel-get channel-put channel-get-evt channel-put-evt
      make-semaphore semaphore-post semaphore-wait semaphore-try-wait? semaphore-peek-evt
      sync sync/timeout handle-evt choice-evt guard-evt nack-guard-evt replace-evt always-evt never-evt
      current-time/type sleep-seconds-nanoseconds alarm-evt)
    (list
      make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
      current-thread-group make-thread-group current-thread thread thread/suspend-to-kill
      thread-resume thread-wait thread-resume-evt thread-suspend-evt thread-dead-evt
      make-channel channel-get channel-put channel-get-evt channel-put-evt
      make-semaphore semaphore-post semaphore-wait semaphore-try-wait? semaphore-peek-evt
      sync sync/timeout handle-evt choice-evt guard-evt nack-guard-evt replace-evt always-evt never-evt
      current-time/type sleep-seconds-nanoseconds alarm-evt)))
