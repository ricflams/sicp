#lang sicp
(#%require "util.rkt")

;; exercise 1.4
(display "exercise 1.4\n")


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(print-eval (a-plus-abs-b 10 5))
(print-eval (a-plus-abs-b 10 -5))
(print-eval (a-plus-abs-b -10 5))
(print-eval (a-plus-abs-b -10 -5))

(display "Add positive b, subtract negative b => add absolute b")
(newline)
