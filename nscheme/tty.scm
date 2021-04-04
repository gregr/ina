;;; Control Sequence Introducer (CSI) codes

;; Title control (title position 0, 1, or 2); some terminals use \e\\ instead of \a ?
(define (ec:title-set title (position 0))
  (string-append "\e]" (number->string position) ";" title "\a"))

;; Display control
(define ec:display-save    "\e[?47h")
(define ec:display-restore "\e[?47l")
;; Report display <lines> <cols> on stdin as: \e[8;<lines>;<cols>t
(define ec:display-size "\e[18t")
(define ec:display-clear-after       "\e[0J")
(define ec:display-clear-before      "\e[1J")
(define ec:display-clear-full        "\e[2J")
(define ec:display-clear-scrollback  "\e[3J")
(define ec:display-clear-line-after  "\e[0K")
(define ec:display-clear-line-before "\e[1K")
(define ec:display-clear-line-full   "\e[2K")

;; Cursor control
(define ec:cursor-save    "\e[s")
(define ec:cursor-restore "\e[u")
(define ec:cursor-show    "\e[?25h")
(define ec:cursor-hide    "\e[?25l")
;; Report cursor position <y-digits> <x-digits> on stdin as: \e[<y-digits>;<x-digits>R
(define ec:cursor-report-position "\e[6n")
(define (ec:cursor-move-to     x y) (string-append
                                      "\e["
                                      ;; translate 0-based x and y coordinates to 1-based
                                      (number->string (+ y 1)) ";"
                                      (number->string (+ x 1)) "H"))
(define (ec:cursor-move-up       n) (string-append "\e[" (number->string n) "A"))
(define (ec:cursor-move-down     n) (string-append "\e[" (number->string n) "B"))
(define (ec:cursor-move-forward  n) (string-append "\e[" (number->string n) "C"))
(define (ec:cursor-move-backward n) (string-append "\e[" (number->string n) "D"))

;; NOTE: these are less standard
;\e[nE beginning of next line
;\e[nF beginning of previous line
;\e[nG move to column on same line
;\e[nS scroll up
;\e[nT scroll down

;; Mouse input reporting
(define ec:mouse-button-on      "\e[?1000h")
(define ec:mouse-button-off     "\e[?1000l")
(define ec:mouse-all-events-on  "\e[?1003h")
(define ec:mouse-all-events-off "\e[?1003l")
;; NOTE: terminal support varies, but one of these reporting modes will probably work
;; xterm style
;; button press:         \e[<0;column;rowM
;; button release:       \e[<0;column;rowm
;; wheel up:             \e[<64;column;rowM
;; wheel down:           \e[<65;column;rowM
;; wheel button press:   \e[<1;column;rowm
;; wheel button release: \e[<1;column;rowM
;; move:                 \e[<35;column;rowM
(define ec:mouse-report-1-on  "\e[?1006h")
(define ec:mouse-report-1-off "\e[?1006l")
;; urxvt style
(define ec:mouse-report-2-on  "\e[?1015h")
(define ec:mouse-report-2-off "\e[?1015l")

;; Select Graphic Rendition (SGR) codes
(define (sgr:color-fg c) (+ 30 c))
(define (sgr:color-bg c) (+ 40 c))

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

(define sgr:reset       0)
(define sgr:bold+       1)
(define sgr:underline+  4)
(define sgr:blink+      5)
(define sgr:invert+     7)
(define sgr:bold-      22)
(define sgr:underline- 24)
(define sgr:blink-     25)
(define sgr:invert-    27)
(define (sgr:bold      ?) (if ? sgr:bold+      sgr:bold-))
(define (sgr:underline ?) (if ? sgr:underline+ sgr:underline-))
(define (sgr:blink     ?) (if ? sgr:blink+     sgr:blink-))
(define (sgr:invert    ?) (if ? sgr:invert+    sgr:invert-))

(define (sgr*->ec sgr*)
  (if (null? sgr*) ""
    (string-append "\e[" (string-join (map number->string sgr*) ";") "m")))
