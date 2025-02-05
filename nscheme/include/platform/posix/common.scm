(define package.posix.common
  (cons
    '(
      host-argument* host-environment host-make-raw-process/k
      filesystem-change-evt filesystem-change-evt-cancel
      change-directory/k directory-file*/k make-symbolic-link/k make-directory/k
      delete-directory/k delete-file/k move-file/k imemory:file/k omemory:file/k
      file-type/k file-size/k file-permissions/k file-modified-seconds/k
      set-file-permissions!/k set-file-modified-seconds!/k
      tcp-listen/k tcp-connect/k udp-open/k)
    (list
      host-argument* host-environment host-make-raw-process/k
      filesystem-change-evt filesystem-change-evt-cancel
      change-directory/k directory-file*/k make-symbolic-link/k make-directory/k
      delete-directory/k delete-file/k move-file/k imemory:file/k omemory:file/k
      file-type/k file-size/k file-permissions/k file-modified-seconds/k
      set-file-permissions!/k set-file-modified-seconds!/k
      tcp-listen/k tcp-connect/k udp-open/k)))
