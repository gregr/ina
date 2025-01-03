(define package.posix.common
  (cons
    '(
      host-pid host-argument* host-environment raw-host-process/k
      change-directory filesystem-change-evt filesystem-change-evt-cancel
      directory-file*/k make-symbolic-link/k make-directory/k
      delete-directory/k delete-file/k move-file/k imemory:file/k omemory:file/k
      file-type/k file-size/k file-permissions/k file-modified-seconds/k
      set-file-permissions!/k set-file-modified-seconds!/k
      gethostname tcp-listen/k tcp-connect/k udp-open/k)
    (list
      host-pid host-argument* host-environment raw-host-process/k
      change-directory filesystem-change-evt filesystem-change-evt-cancel
      directory-file*/k make-symbolic-link/k make-directory/k
      delete-directory/k delete-file/k move-file/k imemory:file/k omemory:file/k
      file-type/k file-size/k file-permissions/k file-modified-seconds/k
      set-file-permissions!/k set-file-modified-seconds!/k
      gethostname tcp-listen/k tcp-connect/k udp-open/k)))
