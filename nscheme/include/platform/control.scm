(define package.control
  (cons
    '(
      make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
      current-thread-group make-thread-group current-thread thread thread-wait thread-dead-evt
      sync sync/default handle-evt choice-evt guard-evt nack-guard-evt replace-evt never-evt
      make-channel channel-get channel-put channel-put-evt
      make-semaphore semaphore-post semaphore-wait
      current-time/type sleep-seconds-nanoseconds)
    (list
      make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
      current-thread-group make-thread-group current-thread thread thread-wait thread-dead-evt
      sync sync/default handle-evt choice-evt guard-evt nack-guard-evt replace-evt never-evt
      make-channel channel-get channel-put channel-put-evt
      make-semaphore semaphore-post semaphore-wait
      current-time/type sleep-seconds-nanoseconds)))
