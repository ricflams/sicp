#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")
(#%require "sqrt.rkt")

;; exercise 1.37
(display "exercise 1.37\n")

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (iter (inc i))))))
  (iter 1))

;; The "real" phi value, (1+√5)/2
(define (phi-the-value)
  (/ (+ 1 (sqrt 5)) 2))

;; The cont-frac will find 1/phi
(define (phi-approx k)
  (/ 1.0
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))

(print-eval (phi-the-value))
(print-eval (phi-approx 13)) ;; 4 correct decimals at 13 iterations
(print-eval (phi-approx 12))
(print-eval (phi-approx 14))
