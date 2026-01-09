;; racket bootstrap/run-file.rkt src/run-cli.scm test/c.scm
(include "../src/codegen/c.scm")

(for-each
  (lambda (S)
    (pretty-write S)
    (displayln '==>)
    (displayln (c-flatten (c:stmt S)))
    (newline))
  '((fn int main ((: int argc) (: char (deref (deref argv))))
        (: int (:= cube (* argc argc argc)))
        (return (+ cube 1 2)))
    (fn int main ((: int argc) (: char (deref (deref argv))))
        (local
          (: int (:= cube (* argc argc argc)))
          (unless (== cube 5)
            (return -1))
          (return (+ cube 1 2))))
    (fn int main ((: int argc) (: char (deref (deref argv))))
        (local
          (: int (:= cube (* argc argc argc)))
          (cond ((== cube 5) (return -1))
                ((== cube 6) (return -2))
                (else (break)))
          (return (+ cube 1 2))))
    (fn int main ((: int argc) (: char (deref (deref argv))))
        (local
          (: int (:= cube (* argc argc argc)))
          (+= cube (cond ((== cube 5) -1)
                         ((== cube 6) -2)
                         (else 7)))
          (return (+ cube 1 2))))
    (case 5
      ((1 2 3) (return first))
      ((4 5)   (return second))
      (else    (return third)))
    (switch 5
            ((1 2 3) (return first))
            ((4 5)   (return second))
            (else    (return third)))
    (typedef struct ;tagged
             ((: u64 header)
              (union VARIANTS
                     ((enum COLOR (RED (:= GREEN 5) BLUE) color)
                      (struct POINT ((: float x) (: float y)) point)
                      (: s64 integer)
                      (: u8  boolean)
                      (struct PAIR ((: (struct tagged) *a *b)) pair)
                      (: f64 inexact))
                     payload))
             tv *tp)
    (%error "one two" "three four" five six)
    (%line 123 "four.five")))

(for-each
  (lambda (S*)
    (pretty-write S*)
    (displayln '==>)
    (displayln (c-flatten (map (lambda (c) (list c "\n\n")) (map c:stmt S*)))))
  '(((%include "foo.h" "bar.h" stdio.h)
     (%cond ((call defined ONE)                           (: (static int) (:= x 1)))
            ((or (call defined TWO) (call defined THREE)) (: (static int) (:= x 5)))
            (else                                         (: (static int) (:= x 6)))))))
