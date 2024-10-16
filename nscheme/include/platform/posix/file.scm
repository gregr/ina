(define (open-input-file path) (open-input-file/k path raise-io-error values))
(define (open-output-file path restriction)
  (open-output-file/k path restriction raise-io-error values))

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

(define (find-file/env env name)
  (find-file/PATH (let ((kv (assv #"PATH" env))) (if kv (cdr kv) #"")) name))
(define (find-file/PATH PATH name)
  (find-file/directory* (bytevector-split PATH (bytevector-ref #":" 0)) name))
(define (find-file/directory* dir* name)
  (let ((name (if (string? name) (string->utf8 name) name)))
    (if (and (< 0 (bytevector-length name))
             (let ((b0 (bytevector-ref name 0)))
               (or (= b0 (bytevector-ref #"." 0)) (= b0 (bytevector-ref #"/" 0)))))
        (utf8->string name)
        (ormap (lambda (dir)
                 (let ((path (utf8->string (bytevector-join #"/" dir name))))
                   (and (file-type/k path (lambda _ #f) values)
                        path)))
               (map (lambda (dir) (if (string? dir) (string->utf8 dir) dir)) dir*)))))
