#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.42
(display "exercise 1.42\n")

(define (compose f g)
  (lambda (x)
    (f (g x))))

(print-eval-verify ((compose square inc) 6) 49)
