(define package.posix.privileged
  (cons
    '(
      standard-input-stream standard-output-stream standard-error-stream
      change-directory directory-file* make-symbolic-link make-directory
      delete-directory delete-file move-file open-file-istream open-file-ostream
      file-type file-size file-permissions file-modified-seconds
      set-file-permissions! set-file-modified-seconds!
      filesystem-change-evt filesystem-change-evt-cancel)
    (list
      standard-input-stream standard-output-stream standard-error-stream
      change-directory directory-file* make-symbolic-link make-directory
      delete-directory delete-file move-file open-file-istream open-file-ostream
      file-type file-size file-permissions file-modified-seconds
      set-file-permissions! set-file-modified-seconds!
      filesystem-change-evt filesystem-change-evt-cancel)))
