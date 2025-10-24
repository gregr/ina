;;; Operating System Command (OSC) escapes
(define (osc:set-window-and-icon-name name) (bytes-append #"\e]0;" name #"\e\\"))
(define (osc:set-icon-name            name) (bytes-append #"\e]1;" name #"\e\\"))
(define (osc:set-window-name          name) (bytes-append #"\e]2;" name #"\e\\"))
