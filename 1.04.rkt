#lang sicp

;; exercise 1.4
(display "exercise 1.4")
(newline)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 5)
(a-plus-abs-b 10 -5)
(a-plus-abs-b -10 5)
(a-plus-abs-b -10 -5)

(display "Add positive b, subtract negative b => add absolute b")
(newline)
