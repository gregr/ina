;;; Control Sequence Introducer (CSI) codes

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
(define ec:cursor-show "\e[?25h")
(define ec:cursor-hide "\e[?25l")
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
(define sgr:colors
  (map cons
       '(default black red green yellow blue magenta cyan white)
       (cons 9 (range 8))))
(define (sgr:fg-color i) (+ 30 i))
(define (sgr:bg-color i) (+ 40 i))

(define sgr:reset                   0)
(define (sgr:bold      ?) (if ? 1 22))
(define (sgr:underline ?) (if ? 4 24))
(define (sgr:blink     ?) (if ? 5 25))
(define (sgr:invert    ?) (if ? 7 27))

(define (sgr:codes->string codes)
  (if (null? codes) ""
    (string-append "\e[" (string-join (map number->string codes) ";") "m")))

(define sgr:style.default
  (plist->alist
    (list 'fg-color  (sgr:fg-color (alist-ref sgr:colors 'default))
          'bg-color  (sgr:bg-color (alist-ref sgr:colors 'default))
          'bold      (sgr:bold      #f)
          'underline (sgr:underline #f)
          'blink     (sgr:blink     #f)
          'invert    (sgr:invert    #f))))

(define (sgr:style-diff-codes s1 s2)
  (foldl (lambda (key codes)
           (define v.1 (alist-ref s1 key))
           (define v.2 (alist-ref s2 key))
           (if (equal? v.1 v.2) codes (cons v.1 codes)))
         '()
         '(invert blink underline bold bg-color fg-color)))
