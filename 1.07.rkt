#lang sicp
(#%require "util.rkt")

;; exercise 1.7
(display "exercise 1.7\n")


(#%require "math.rkt")


(define (average x y)
  (/ (+ x y ) 2))

(define (sqrt x)
  (define (sqrt-iter prev-guess guess)
    (if (good-enough? prev-guess guess)
	guess
	(sqrt-iter guess (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? prev-guess guess)
    (or (= prev-guess guess)
	(< (abs (- (/ prev-guess guess) 1)) 0.000001)))
  (sqrt-iter 0.0 1.0))


(print-eval (sqrt 0.0001))
(print-eval (sqrt 0.01))
(print-eval (sqrt 1.2))
(print-eval (sqrt 9))
(print-eval (sqrt 1000000))
(print-eval (sqrt 100000000))
(print-eval (sqrt 1e26))

(print-eval (square (sqrt 0.001)))
(print-eval (square (sqrt 100)))
(print-eval (square (sqrt 1000)))
(print-eval (square (sqrt 10000)))
(print-eval (square (sqrt 100000)))
