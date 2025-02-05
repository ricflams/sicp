#lang sicp
(#%require "util.rkt")

;; exercise 1.3
(display "exercise 1.3\n")


(define (sum-square a b c)
  (define (square x) (* x x))
  (define (sum-of-squares x y) (+ (square x) (square y)))
  (cond
   ((and (> a c) (> b c)) (sum-of-squares a b))
   ((and (> a b) (> c b)) (sum-of-squares a c))
   (else (sum-of-squares b c))))

(print-eval (sum-square 3 4 5))
(print-eval (sum-square 4 3 5))
(print-eval (sum-square 3 5 4))
(print-eval (sum-square 5 3 4))
(print-eval (sum-square 5 4 3))
(print-eval (sum-square 4 5 4))

