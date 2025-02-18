#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.43
(display "exercise 1.43\n")


(define (repeated f n)
  (lambda (x)
    (if (= n 0)
        x
        (f ((repeated f (- n 1)) x)))))

(print-eval-verify ((repeated square 2) 5) 625)
