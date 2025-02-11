(define (imemory:file/k               path      kf k) ((current-filesystem) 'open-imemory          path      kf k))
(define (omemory:file/k               path mod  kf k) ((current-filesystem) 'open-omemory          path mod  kf k))
(define (file-change-evt/k            path      kf k) ((current-filesystem) 'change-evt            path      kf k))
(define (change-directory/k           path      kf k) ((current-filesystem) 'change-directory      path      kf k))
(define (directory-file*/k            path      kf k) ((current-filesystem) 'list                  path      kf k))
(define (make-symbolic-link/k         to path   kf k) ((current-filesystem) 'make-symbolic-link    to path   kf k))
(define (make-directory/k             path      kf k) ((current-filesystem) 'make-directory        path      kf k))
(define (move-file/k                  old new   kf k) ((current-filesystem) 'move                  old new   kf k))
(define (delete-file/k                path      kf k) ((current-filesystem) 'delete-file           path      kf k))
(define (delete-directory/k           path      kf k) ((current-filesystem) 'delete-directory      path      kf k))
(define (file-type/k                  path      kf k) ((current-filesystem) 'type                  path      kf k))
(define (file-size/k                  path      kf k) ((current-filesystem) 'size                  path      kf k))
(define (file-permissions/k           path      kf k) ((current-filesystem) 'permissions           path      kf k))
(define (file-modified-seconds/k      path      kf k) ((current-filesystem) 'modified-seconds      path      kf k))
(define (set-file-permissions!/k      path perm kf k) ((current-filesystem) 'set-permissions!      path perm kf k))
(define (set-file-modified-seconds!/k path sec  kf k) ((current-filesystem) 'set-modified-seconds! path sec  kf k))

(define (imemory:file path)     (imemory:file/k path     raise-io-error values))
(define (omemory:file path mod) (omemory:file/k path mod raise-io-error values))
(define (iport:file/k path kf k) (imemory:file/k path kf (lambda (im) (k (iport:memory im 0 #t)))))
(define (oport:file/k path mod kf k)
  (let ((k (lambda (om) (omemory-size/k om kf (lambda (size) (k (oport:memory om size #t)))))))
    (omemory:file/k path mod kf k)))
(define (iport:file path)     (iport:file/k path     raise-io-error values))
(define (oport:file path mod) (oport:file/k path mod raise-io-error values))

(define (file-change-evt       path) (file-change-evt/k       path  raise-io-error values))
(define (change-directory      path) (change-directory/k      path raise-io-error values))
(define (directory-file*       path) (directory-file*/k       path raise-io-error values))
(define (make-symbolic-link to path) (make-symbolic-link/k to path raise-io-error values))
(define (make-directory        path) (make-directory/k        path raise-io-error values))
(define (move-file          old new) (move-file/k          old new raise-io-error values))
(define (delete-file           path) (delete-file/k           path raise-io-error values))
(define (delete-directory      path) (delete-directory/k      path raise-io-error values))
(define (file-type             path) (file-type/k             path raise-io-error values))
(define (file-size             path) (file-size/k             path raise-io-error values))
(define (file-permissions      path) (file-permissions/k      path raise-io-error values))
(define (file-modified-seconds path) (file-modified-seconds/k path raise-io-error values))
(define (set-file-permissions! path permissions)
  (set-file-permissions!/k path permissions raise-io-error values))
(define (set-file-modified-seconds! path seconds)
  (set-file-modified-seconds!/k path seconds raise-io-error values))

(define (find-file name) (find-file/env (current-host-environment) name))
(define (find-file/env env name)
  (find-file/PATH (let ((kv (assv #"PATH" env))) (if kv (cdr kv) #"")) name))
(define (find-file/PATH PATH name)
  (find-file/directory* (bytevector-split PATH (bytevector-ref #":" 0)) name))
(define (find-file/directory* dir* name)
  (define (element name) (cond ((string? name) (string->bytevector name))
                                 ((symbol? name) (symbol->bytevector name))
                                 (else           name)))
  (let ((name (element name)))
    (if (and (< 0 (bytevector-length name))
             (let ((b0 (bytevector-ref name 0)))
               (or (= b0 (bytevector-ref #"." 0)) (= b0 (bytevector-ref #"/" 0)))))
        name
        (ormap (lambda (dir) (let ((path (bytevector-join #"/" dir name)))
                               (and (file-type/k path (lambda _ #f) values) path)))
               (map element dir*)))))

(splicing-let ((byte:/ (bytevector-ref #"/" 0)))
  (define (path->bytevector path) (cond ((string? path) (string->bytevector path))
                                        ((symbol? path) (symbol->bytevector path))
                                        (else           path)))
  (define (path-split path) (bytevector-split (path->bytevector path) byte:/))
  (define (path-directory path)
    (let* ((path (path->bytevector path)) (count ((bytevector-index-end/byte byte:/) path)))
      (if (< 0 count) (bytevector-copy path 0 count) #".")))
  (define (path-append* p*)
    (bytevector-join* #"/" (map (lambda (p) (bytevector-rtrim1 (path->bytevector p) byte:/)) p*)))
  (define (path-append . path*) (path-append* path*)))
