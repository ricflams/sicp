#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.39
(display "exercise 1.39\n")

(define (tan-cf x k)
  (define (iter i)
    (if (> i k)
        1.0
        (- (- (* 2 i) 1) ;; Di=2i-1; for i=1,2,3,4,... gives 1,3,5,7,...
           (/ (square x)
              (iter (inc i))))))
  (/ x (iter 1)))

(define (tan-real x)
  (tan x))
  

(print-eval (tan-cf 1 3))
(print-eval (tan-cf 1 5))
(print-eval (tan-cf 1 9))
(print-eval (tan-real 1))

(print-eval (tan-cf 2 3))
(print-eval (tan-cf 2 5))
(print-eval (tan-cf 2 9))
(print-eval (tan-real 2))

(print-eval (tan-cf 5 3))
(print-eval (tan-cf 5 5))
(print-eval (tan-cf 5 9))
(print-eval (tan-real 5))
