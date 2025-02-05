#lang sicp
(#%require "util.rkt")

;; exercise 1.2
(display "exercise 1.2\n")


(print-eval (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))
(print-eval (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4.0 5))))) (* 3 (- 6 2) (- 2 7))))

