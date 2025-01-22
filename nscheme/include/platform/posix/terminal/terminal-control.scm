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
