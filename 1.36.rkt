#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")
(#%require racket/trace)

;; exercise 1.36
(display "exercise 1.36\n")

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (trace try)
  (try first-guess))

(display "Without average damping:\n")

(define (x-th-root x)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))
(print-eval (x-th-root 1000))

(display "With average damping:\n")

(define (x-th-root-with-damping x)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               2.0))
(print-eval (x-th-root-with-damping 1000))
