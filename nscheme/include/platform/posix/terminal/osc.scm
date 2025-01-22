;;; Operating System Command (OSC) escapes
(define (osc:set-window-and-icon-name name) (bytevector-append #"\e]0;" name #"\e\\"))
(define (osc:set-icon-name            name) (bytevector-append #"\e]1;" name #"\e\\"))
(define (osc:set-window-name          name) (bytevector-append #"\e]2;" name #"\e\\"))
