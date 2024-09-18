(define package.control
  (cons
    '(
      make-parameter panic-handler current-custodian make-custodian custodian-shutdown-all
      current-thread-group make-thread-group current-thread thread thread/suspend-to-kill
      thread-resume thread-suspend thread-kill thread-wait
      thread-resume-evt thread-suspend-evt thread-dead-evt
      make-channel channel-get channel-put channel-get-evt channel-put-evt
      make-semaphore semaphore-post semaphore-wait semaphore-try-wait? semaphore-peek-evt
      call-with-semaphore sleep alarm-evt always-evt never-evt
      sync sync/timeout handle-evt choice-evt guard-evt nack-guard-evt replace-evt)
    (list
      make-parameter panic-handler current-custodian make-custodian custodian-shutdown-all
      current-thread-group make-thread-group current-thread thread thread/suspend-to-kill
      thread-resume thread-suspend thread-kill thread-wait
      thread-resume-evt thread-suspend-evt thread-dead-evt
      make-channel channel-get channel-put channel-get-evt channel-put-evt
      make-semaphore semaphore-post semaphore-wait semaphore-try-wait? semaphore-peek-evt
      call-with-semaphore sleep alarm-evt always-evt never-evt
      sync sync/timeout handle-evt choice-evt guard-evt nack-guard-evt replace-evt)))
