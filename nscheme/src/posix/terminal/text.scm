(define (printer-decorate/sgr p)
  (printer-map-text/attribute p (lambda (t sgr) (if sgr (bytes-append sgr t #"\e[0m") t))))
(define (printer-sgr-default p sgr.default)
  (printer-map-attribute p (lambda (sgr) (or sgr sgr.default))))
(define (printer-sgr-add p sgr.add)
  (printer-map-attribute p (lambda (sgr) (if sgr (bytes-append sgr sgr.add) sgr.add))))

(define (writer-decorate/sgr w sgr.prefix sgr.dot sgr.bracket atom->sgr)
  (define (decorate attr sgr) (or attr sgr))
  (make-writer
    (lambda (t attr x) (writer-atom          w t (decorate attr (atom->sgr x)) x))
    (lambda (t attr x) (writer-prefix        w t (decorate attr sgr.prefix)    x))
    (lambda (t attr)   (writer-dot           w t (decorate attr sgr.dot)))
    (lambda (t attr x) (writer-left-bracket  w t (decorate attr sgr.bracket)   x))
    (lambda (t attr)   (writer-right-bracket w t (decorate attr sgr.bracket)))))

(define (writer:layout/sgr l sgr.prefix sgr.dot sgr.bracket datum->sgr)
  (writer-decorate/sgr (writer:layout l) sgr.prefix sgr.dot sgr.bracket datum->sgr))
