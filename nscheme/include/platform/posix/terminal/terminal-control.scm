;;; Control Sequence Introducer (CSI) codes

;; Title control (title position 0, 1, or 2); some terminals use \e\\ instead of \a ?
(define (csi:title-set title position)
  (string-append "\e]" (number->string position) ";" title "\a"))

;; Display control
(define csi:display-save    "\e[?47h")
(define csi:display-restore "\e[?47l")
;; Report display <lines> <cols> on stdin as: \e[8;<lines>;<cols>t
(define csi:display-size "\e[18t")
(define csi:display-clear-after       "\e[0J")
(define csi:display-clear-before      "\e[1J")
(define csi:display-clear-full        "\e[2J")
(define csi:display-clear-scrollback  "\e[3J")
(define csi:display-clear-line-after  "\e[0K")
(define csi:display-clear-line-before "\e[1K")
(define csi:display-clear-line-full   "\e[2K")

;; Cursor control
(define csi:cursor-save    "\e[s")
(define csi:cursor-restore "\e[u")
(define csi:cursor-show    "\e[?25h")
(define csi:cursor-hide    "\e[?25l")
;; Report cursor position <y-digits> <x-digits> on stdin as: \e[<y-digits>;<x-digits>R
(define csi:cursor-report-position "\e[6n")
(define (csi:cursor-move-to     x y) (string-append
                                       "\e["
                                       ;; translate 0-based x and y coordinates to 1-based
                                       (number->string (+ y 1)) ";"
                                       (number->string (+ x 1)) "H"))
(define (csi:cursor-move-up       n) (string-append "\e[" (number->string n) "A"))
(define (csi:cursor-move-down     n) (string-append "\e[" (number->string n) "B"))
(define (csi:cursor-move-forward  n) (string-append "\e[" (number->string n) "C"))
(define (csi:cursor-move-backward n) (string-append "\e[" (number->string n) "D"))

;; NOTE: these are less standard
;\e[nE beginning of next line
;\e[nF beginning of previous line
;\e[nG move to column on same line
;\e[nS scroll up
;\e[nT scroll down

;; Mouse input reporting
(define csi:mouse-button-on      "\e[?1000h")
(define csi:mouse-button-off     "\e[?1000l")
(define csi:mouse-all-events-on  "\e[?1003h")
(define csi:mouse-all-events-off "\e[?1003l")
;; NOTE: terminal support varies, but one of these reporting modes will probably work
;; xterm style
;; button press:         \e[<0;column;rowM
;; button release:       \e[<0;column;rowm
;; wheel up:             \e[<64;column;rowM
;; wheel down:           \e[<65;column;rowM
;; wheel button press:   \e[<1;column;rowm
;; wheel button release: \e[<1;column;rowM
;; move:                 \e[<35;column;rowM
(define csi:mouse-report-1-on  "\e[?1006h")
(define csi:mouse-report-1-off "\e[?1006l")
;; urxvt style
(define csi:mouse-report-2-on  "\e[?1015h")
(define csi:mouse-report-2-off "\e[?1015l")

;; Select Graphic Rendition (SGR) codes
(define (sgr:color-fg c) (list (+ 30 c)))
(define (sgr:color-bg c) (list (+ 40 c)))

(define sgr:color:default 9)
(define sgr:color:black   0)
(define sgr:color:red     1)
(define sgr:color:green   2)
(define sgr:color:yellow  3)
(define sgr:color:blue    4)
(define sgr:color:magenta 5)
(define sgr:color:cyan    6)
(define sgr:color:white   7)

(define sgr:color-fg:default (sgr:color-fg sgr:color:default))
(define sgr:color-fg:black   (sgr:color-fg sgr:color:black))
(define sgr:color-fg:red     (sgr:color-fg sgr:color:red))
(define sgr:color-fg:green   (sgr:color-fg sgr:color:green))
(define sgr:color-fg:yellow  (sgr:color-fg sgr:color:yellow))
(define sgr:color-fg:blue    (sgr:color-fg sgr:color:blue))
(define sgr:color-fg:magenta (sgr:color-fg sgr:color:magenta))
(define sgr:color-fg:cyan    (sgr:color-fg sgr:color:cyan))
(define sgr:color-fg:white   (sgr:color-fg sgr:color:white))

(define sgr:color-bg:default (sgr:color-bg sgr:color:default))
(define sgr:color-bg:black   (sgr:color-bg sgr:color:black))
(define sgr:color-bg:red     (sgr:color-bg sgr:color:red))
(define sgr:color-bg:green   (sgr:color-bg sgr:color:green))
(define sgr:color-bg:yellow  (sgr:color-bg sgr:color:yellow))
(define sgr:color-bg:blue    (sgr:color-bg sgr:color:blue))
(define sgr:color-bg:magenta (sgr:color-bg sgr:color:magenta))
(define sgr:color-bg:cyan    (sgr:color-bg sgr:color:cyan))
(define sgr:color-bg:white   (sgr:color-bg sgr:color:white))

(define (sgr:color-bright-fg c) (list (+  90 c)))
(define (sgr:color-bright-bg c) (list (+ 100 c)))

(define sgr:color-bright-fg:gray    (sgr:color-bright-fg sgr:color:black))
(define sgr:color-bright-fg:red     (sgr:color-bright-fg sgr:color:red))
(define sgr:color-bright-fg:green   (sgr:color-bright-fg sgr:color:green))
(define sgr:color-bright-fg:yellow  (sgr:color-bright-fg sgr:color:yellow))
(define sgr:color-bright-fg:blue    (sgr:color-bright-fg sgr:color:blue))
(define sgr:color-bright-fg:magenta (sgr:color-bright-fg sgr:color:magenta))
(define sgr:color-bright-fg:cyan    (sgr:color-bright-fg sgr:color:cyan))
(define sgr:color-bright-fg:white   (sgr:color-bright-fg sgr:color:white))

(define sgr:color-bright-bg:gray    (sgr:color-bright-bg sgr:color:black))
(define sgr:color-bright-bg:red     (sgr:color-bright-bg sgr:color:red))
(define sgr:color-bright-bg:green   (sgr:color-bright-bg sgr:color:green))
(define sgr:color-bright-bg:yellow  (sgr:color-bright-bg sgr:color:yellow))
(define sgr:color-bright-bg:blue    (sgr:color-bright-bg sgr:color:blue))
(define sgr:color-bright-bg:magenta (sgr:color-bright-bg sgr:color:magenta))
(define sgr:color-bright-bg:cyan    (sgr:color-bright-bg sgr:color:cyan))
(define sgr:color-bright-bg:white   (sgr:color-bright-bg sgr:color:white))

(define (sgr:color-256-fg c) (list 38 5 c))
(define (sgr:color-256-bg c) (list 48 5 c))

(define sgr:color-256-fg:gray    (sgr:color-256-fg (+ 8 sgr:color:black)))
(define sgr:color-256-fg:red     (sgr:color-256-fg (+ 8 sgr:color:red)))
(define sgr:color-256-fg:green   (sgr:color-256-fg (+ 8 sgr:color:green)))
(define sgr:color-256-fg:yellow  (sgr:color-256-fg (+ 8 sgr:color:yellow)))
(define sgr:color-256-fg:blue    (sgr:color-256-fg (+ 8 sgr:color:blue)))
(define sgr:color-256-fg:magenta (sgr:color-256-fg (+ 8 sgr:color:magenta)))
(define sgr:color-256-fg:cyan    (sgr:color-256-fg (+ 8 sgr:color:cyan)))
(define sgr:color-256-fg:white   (sgr:color-256-fg (+ 8 sgr:color:white)))

(define sgr:color-256-bg:gray    (sgr:color-256-bg (+ 8 sgr:color:black)))
(define sgr:color-256-bg:red     (sgr:color-256-bg (+ 8 sgr:color:red)))
(define sgr:color-256-bg:green   (sgr:color-256-bg (+ 8 sgr:color:green)))
(define sgr:color-256-bg:yellow  (sgr:color-256-bg (+ 8 sgr:color:yellow)))
(define sgr:color-256-bg:blue    (sgr:color-256-bg (+ 8 sgr:color:blue)))
(define sgr:color-256-bg:magenta (sgr:color-256-bg (+ 8 sgr:color:magenta)))
(define sgr:color-256-bg:cyan    (sgr:color-256-bg (+ 8 sgr:color:cyan)))
(define sgr:color-256-bg:white   (sgr:color-256-bg (+ 8 sgr:color:white)))

;; r, g, b: 0 through 5
(define (sgr:color-rgb6-fg r g b) (sgr:color-256-fg (+ 16 (* 36 r) (* 6 g) b)))
(define (sgr:color-rgb6-bg r g b) (sgr:color-256-bg (+ 16 (* 36 r) (* 6 g) b)))
;; i: intensity 0 through 23
(define (sgr:color-gray-fg i)     (sgr:color-256-fg (+ 232 i)))
(define (sgr:color-gray-bg i)     (sgr:color-256-bg (+ 232 i)))

(define sgr:reset      (list  0))
(define sgr:bold+      (list  1))
(define sgr:underline+ (list  4))
(define sgr:blink+     (list  5))
(define sgr:invert+    (list  7))
(define sgr:strike+    (list  9))
(define sgr:bold-      (list 22))
(define sgr:underline- (list 24))
(define sgr:blink-     (list 25))
(define sgr:invert-    (list 27))
(define sgr:strike-    (list 29))
(define (sgr:bold      ?) (if ? sgr:bold+      sgr:bold-))
(define (sgr:underline ?) (if ? sgr:underline+ sgr:underline-))
(define (sgr:blink     ?) (if ? sgr:blink+     sgr:blink-))
(define (sgr:invert    ?) (if ? sgr:invert+    sgr:invert-))
(define (sgr:strike    ?) (if ? sgr:strike+    sgr:strike-))

(define (sgr->csi sgr)
  (if (null? sgr) "" (string-append "\e[" (string-join* ";" (map number->string sgr)) "m")))
