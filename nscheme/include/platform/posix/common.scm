(define package.posix.common
  (cons
    '(
      host-pid host-environment raw-host-process
      change-directory command-line-argument*
      standard-input-stream standard-output-stream standard-error-stream
      filesystem-change-evt filesystem-change-evt-cancel
      directory-file*/k make-symbolic-link/k make-directory/k
      delete-directory/k delete-file/k move-file/k open-file-istream/k open-file-ostream/k
      file-type/k file-size/k file-permissions/k file-modified-seconds/k
      set-file-permissions!/k set-file-modified-seconds!/k
      )
    (list
      host-pid host-environment raw-host-process
      change-directory command-line-argument*
      standard-input-stream standard-output-stream standard-error-stream
      filesystem-change-evt filesystem-change-evt-cancel
      directory-file*/k make-symbolic-link/k make-directory/k
      delete-directory/k delete-file/k move-file/k open-file-istream/k open-file-ostream/k
      file-type/k file-size/k file-permissions/k file-modified-seconds/k
      set-file-permissions!/k set-file-modified-seconds!/k)))
