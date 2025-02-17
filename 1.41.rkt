#lang sicp
(#%require "util.rkt")

;; exercise 1.41
(display "exercise 1.41\n")

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x) (+ x 1))

(print-eval-verify ((double inc) 3) 5)

;; (double f) = f^2(x)
;; (double double) = f^2^2(x)
;; (double (double double)) = f^2^2^2(x)
;; 2^2^2 = 4^2 = 16; ie for f=inc this means increment by 16
(define (ddd-inc x)
  (+ x 16))

(print-eval-verify (((double (double double)) inc) 5) (ddd-inc 5))
(print-eval-verify (((double (double double)) inc) 3) (ddd-inc 3))
