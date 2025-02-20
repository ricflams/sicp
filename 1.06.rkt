#lang sicp
(#%require "util.rkt")

;; exercise 1.6
(display "exercise 1.6\n")

(#%require "math.rkt")


(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- x (* guess guess))) 0.001))
  (sqrt-iter 1.0))


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


;; Alternatives, using a raise function

(define (raise x n)
  (if (= n 0)
      1
      (* x (raise x (- n 1)))))

(define (root x power improve)
  (define (iter guess)
    (if (good-enough? guess)
	guess
	(iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (- x (raise guess power))) (/ guess 100000000)))
  (iter 1.0))

(define (root-2 x)
  (define (improve guess)
    (average guess (/ x guess)))
  (root x 2 improve))

(print-eval (root-2 100))

(define (root-3 x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (root x 3 improve))

(print-eval (root-3 27))


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter-2 guess x)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- x (* guess guess))) 0.001))
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter-2 (improve guess x) x)))

(print-eval (new-if (= 2 3) 0 5))
(print-eval (new-if (= 1 1) 0 5))

;; (sqrt-iter-2 1.0 9) ;; would loop forever in calling itself
