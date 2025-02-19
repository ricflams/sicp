#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.46
(display "exercise 1.46\n")

(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improve guess))))
    (iter initial-guess)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- x (* guess guess))) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(print-eval (sqrt 2))
(print-eval (sqrt 9))
(print-eval (sqrt 10))


(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))

(print-eval (fixed-point cos 1))

