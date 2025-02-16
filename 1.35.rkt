#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.35
(display "exercise 1.35\n")

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(print-eval (fixed-point cos 1))

;; (define (sqrt x)
;;   (fixed-point (lambda (y) (/ x y))
;;                1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(print-eval (sqrt 2))
(print-eval (sqrt 9))
(print-eval (sqrt 10))


; golden ratio phi:
;     phi² := 1 + phi
; =>  phi = (1 + phi) / phi
; =>  phi = 1 + 1/phi

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))
(print-eval phi)

;; (define phi2
;;  (fixed-point (lambda (x) (- (square x) 1))
;;               1.0))
;; loops forever, hmm
