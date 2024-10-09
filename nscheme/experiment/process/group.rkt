#lang racket/base
;(require )
;(current-subprocess-custodian-mode 'kill)
(current-subprocess-custodian-mode #f)

;; Passing -m to bash allows sleep 1400 to start in a new process group.
(subprocess #f #f (current-error-port) #f (find-executable-path "bash") "-mc" "sleep 1400")

;(subprocess #f #f (current-error-port) #f (find-executable-path "bash") "-ic" "set -m; sleep 1400")
;(subprocess #f #f (current-error-port) #f (find-executable-path "bash") "-mc" "exec sleep 1400")
;(subprocess #f #f (current-error-port) #f (find-executable-path "bash") "-mc" "sleep 1400 &")
;(subprocess #f #f (current-error-port) #f (find-executable-path "bash") "-c" "(set -m; exec cat)")
;(subprocess #f #f #f #f (find-executable-path "cat"))
;(subprocess #f #f #f #f (find-executable-path "sleep") "1700")
;(subprocess #f #f #f #f (find-executable-path "bash") "-c" "sleep 1700")
;(subprocess #f #f #f #f (find-executable-path "bash") "-c" "nohup sleep 1700 &")

(sleep 900)
