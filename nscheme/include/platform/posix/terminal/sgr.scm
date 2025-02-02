;;; Select Graphic Rendition (SGR) escapes and attributes (SGR is a type of CSI)
(define (make-sgr . sgra*) (sgra*->sgr sgra*))
(define (sgra*->sgr sgra*) (if (null? sgra*) #"" (bytevector-append
                                                   #"\e[" (bytevector-join* #";" sgra*) #"m")))

(define sgra:reset      (number->utf8  0))
(define sgra:bold+      (number->utf8  1))
(define sgra:underline+ (number->utf8  4))
(define sgra:blink+     (number->utf8  5))
(define sgra:invert+    (number->utf8  7))
(define sgra:strike+    (number->utf8  9))
(define sgra:bold-      (number->utf8 22))
(define sgra:underline- (number->utf8 24))
(define sgra:blink-     (number->utf8 25))
(define sgra:invert-    (number->utf8 27))
(define sgra:strike-    (number->utf8 29))
(define (sgra:bold      ?) (if ? sgra:bold+      sgra:bold-))
(define (sgra:underline ?) (if ? sgra:underline+ sgra:underline-))
(define (sgra:blink     ?) (if ? sgra:blink+     sgra:blink-))
(define (sgra:invert    ?) (if ? sgra:invert+    sgra:invert-))
(define (sgra:strike    ?) (if ? sgra:strike+    sgra:strike-))

(define sgr:reset (sgra*->sgr (list sgra:reset)))

;;;;;;;;;;;;;
;;; Color ;;;
;;;;;;;;;;;;;
(define sgr-color-simple:default 9)
(define sgr-color-simple:black   0)
(define sgr-color-simple:red     1)
(define sgr-color-simple:green   2)
(define sgr-color-simple:yellow  3)
(define sgr-color-simple:blue    4)
(define sgr-color-simple:magenta 5)
(define sgr-color-simple:cyan    6)
(define sgr-color-simple:white   7)

(define (sgra:color-simple-fg c) (number->utf8 (+ 30 c)))
(define (sgra:color-simple-bg c) (number->utf8 (+ 40 c)))
(define sgra:color-simple-fg:default (sgra:color-simple-fg sgr-color-simple:default))
(define sgra:color-simple-fg:black   (sgra:color-simple-fg sgr-color-simple:black))
(define sgra:color-simple-fg:red     (sgra:color-simple-fg sgr-color-simple:red))
(define sgra:color-simple-fg:green   (sgra:color-simple-fg sgr-color-simple:green))
(define sgra:color-simple-fg:yellow  (sgra:color-simple-fg sgr-color-simple:yellow))
(define sgra:color-simple-fg:blue    (sgra:color-simple-fg sgr-color-simple:blue))
(define sgra:color-simple-fg:magenta (sgra:color-simple-fg sgr-color-simple:magenta))
(define sgra:color-simple-fg:cyan    (sgra:color-simple-fg sgr-color-simple:cyan))
(define sgra:color-simple-fg:white   (sgra:color-simple-fg sgr-color-simple:white))
(define sgra:color-simple-bg:default (sgra:color-simple-bg sgr-color-simple:default))
(define sgra:color-simple-bg:black   (sgra:color-simple-bg sgr-color-simple:black))
(define sgra:color-simple-bg:red     (sgra:color-simple-bg sgr-color-simple:red))
(define sgra:color-simple-bg:green   (sgra:color-simple-bg sgr-color-simple:green))
(define sgra:color-simple-bg:yellow  (sgra:color-simple-bg sgr-color-simple:yellow))
(define sgra:color-simple-bg:blue    (sgra:color-simple-bg sgr-color-simple:blue))
(define sgra:color-simple-bg:magenta (sgra:color-simple-bg sgr-color-simple:magenta))
(define sgra:color-simple-bg:cyan    (sgra:color-simple-bg sgr-color-simple:cyan))
(define sgra:color-simple-bg:white   (sgra:color-simple-bg sgr-color-simple:white))

(define (sgra:color-simple-bright-fg c) (number->utf8 (+  90 c)))
(define (sgra:color-simple-bright-bg c) (number->utf8 (+ 100 c)))
(define sgra:color-simple-bright-fg:gray    (sgra:color-simple-bright-fg sgr-color-simple:black))
(define sgra:color-simple-bright-fg:red     (sgra:color-simple-bright-fg sgr-color-simple:red))
(define sgra:color-simple-bright-fg:green   (sgra:color-simple-bright-fg sgr-color-simple:green))
(define sgra:color-simple-bright-fg:yellow  (sgra:color-simple-bright-fg sgr-color-simple:yellow))
(define sgra:color-simple-bright-fg:blue    (sgra:color-simple-bright-fg sgr-color-simple:blue))
(define sgra:color-simple-bright-fg:magenta (sgra:color-simple-bright-fg sgr-color-simple:magenta))
(define sgra:color-simple-bright-fg:cyan    (sgra:color-simple-bright-fg sgr-color-simple:cyan))
(define sgra:color-simple-bright-fg:white   (sgra:color-simple-bright-fg sgr-color-simple:white))
(define sgra:color-simple-bright-bg:gray    (sgra:color-simple-bright-bg sgr-color-simple:black))
(define sgra:color-simple-bright-bg:red     (sgra:color-simple-bright-bg sgr-color-simple:red))
(define sgra:color-simple-bright-bg:green   (sgra:color-simple-bright-bg sgr-color-simple:green))
(define sgra:color-simple-bright-bg:yellow  (sgra:color-simple-bright-bg sgr-color-simple:yellow))
(define sgra:color-simple-bright-bg:blue    (sgra:color-simple-bright-bg sgr-color-simple:blue))
(define sgra:color-simple-bright-bg:magenta (sgra:color-simple-bright-bg sgr-color-simple:magenta))
(define sgra:color-simple-bright-bg:cyan    (sgra:color-simple-bright-bg sgr-color-simple:cyan))
(define sgra:color-simple-bright-bg:white   (sgra:color-simple-bright-bg sgr-color-simple:white))

(define (sgra:color-256-fg c) (bytevector-append #"38;5;" (number->utf8 c)))
(define (sgra:color-256-bg c) (bytevector-append #"48;5;" (number->utf8 c)))
;; r, g, b: 0 through 5
(define (sgra:color-6cube-fg r g b) (sgra:color-256-fg (+ 16 (* 36 r) (* 6 g) b)))
(define (sgra:color-6cube-bg r g b) (sgra:color-256-bg (+ 16 (* 36 r) (* 6 g) b)))
;; i: intensity 0 through 23
(define (sgra:color-24gray-fg i) (sgra:color-256-fg (+ 232 i)))
(define (sgra:color-24gray-bg i) (sgra:color-256-bg (+ 232 i)))
;; r, g, b: 0 through 255
(define (sgra:color-24bit-fg r g b)
  (bytevector-append #"38;2;" (number->utf8 r) #";" (number->utf8 g) #";" (number->utf8 b)))
