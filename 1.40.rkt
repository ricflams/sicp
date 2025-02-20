#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")
(#%require "fixed-point.rkt")

;; exercise 1.40
(display "exercise 1.40\n")

(define dx 0.00001)

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


;; Give it a spin by calculating sqrt using newtons
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
(print-eval (sqrt 2))
(print-eval (sqrt 9))
(print-eval (sqrt 10))


;; First let's solve it using the cumbersome way of
;; deducing the x=f(x) function that fixed-point can solve
;;
;;     x³ + ax² + bx + c = 0
;; =>  x³ + ax² + bx = -c
;; =>  x(x² + ax + b) = -c
;; =>  x = -c / (x² + ax + b)

(define (cubic-by-fixed-point a b c)
  (lambda (x)
    (/ (- 0 c)
       (+ (square x)
          (* a x)
          b))))

(define (solve-cubic-by-fixed-point a b c guess)
  (fixed-point (cubic-by-fixed-point a b c) guess))

(print-eval (solve-cubic-by-fixed-point -6.0 11.0 -6.0 1.2))
(print-eval (solve-cubic-by-fixed-point -6.0 11.0 -6.0 2.2))
(print-eval (solve-cubic-by-fixed-point -6.0 11.0 -6.0 3.2))

;; Interestingly it can't find the x=2 root no matter how close the guess is
(print-eval (solve-cubic-by-fixed-point -6.0 11.0 -6.0 1.99))
(print-eval (solve-cubic-by-fixed-point -6.0 11.0 -6.0 2.01))


;; Then solve it using the much easier method of just
;; defining the cubic equation and solve using newtons:
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (solve-cubic a b c guess)
  (newtons-method (cubic a b c) guess))

(print-eval (solve-cubic -6.0 11.0 -6.0 1.2))
(print-eval (solve-cubic -6.0 11.0 -6.0 2.2))
(print-eval (solve-cubic -6.0 11.0 -6.0 3.2))
  
  
