#lang sicp
(#%require "util.rkt")

;; exercise 1.11
(display "exercise 1.11\n")


(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(print-eval (f 1))
(print-eval (f 2))
(print-eval (f 3))
(print-eval (f 4))
(print-eval (f 5))
(print-eval (f 6))
(print-eval (f 7))

(define (f-iterative  n)
  (define (iter a b c n)
    (if (= n 0)
	a
	(iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (if (< n 3)
      n
      (iter 2 1 0 (- n 2))))

(print-eval (f-iterative 1))
(print-eval (f-iterative 2))
(print-eval (f-iterative 3))
(print-eval (f-iterative 4))
(print-eval (f-iterative 5))
(print-eval (f-iterative 6))
(print-eval (f-iterative 7))
