(define package.posix.common
  (cons
    '(
      host-pid host-argument* host-environment raw-host-process/k
      change-directory filesystem-change-evt filesystem-change-evt-cancel
      directory-file*/k make-symbolic-link/k make-directory/k
      delete-directory/k delete-file/k move-file/k open-input-file/k open-output-file/k
      file-type/k file-size/k file-permissions/k file-modified-seconds/k
      set-file-permissions!/k set-file-modified-seconds!/k
      gethostname open-tcp-listener/k open-tcp-connection/k open-udp-socket/k)
    (list
      host-pid host-argument* host-environment raw-host-process/k
      change-directory filesystem-change-evt filesystem-change-evt-cancel
      directory-file*/k make-symbolic-link/k make-directory/k
      delete-directory/k delete-file/k move-file/k open-input-file/k open-output-file/k
      file-type/k file-size/k file-permissions/k file-modified-seconds/k
      set-file-permissions!/k set-file-modified-seconds!/k
      gethostname open-tcp-listener/k open-tcp-connection/k open-udp-socket/k)))
