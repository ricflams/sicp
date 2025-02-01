#lang sicp

;; exercise 1.15
(display "exercise 1.15")
(newline)

(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; for checking:
;;(#%require racket/trace)
;;(trace p)
;;(sine 12.5)

;; Every call to p will call sine with the angle divided by 3.
;; Therefoe p will be called n times until angle/3^n <= 0.1
;;
;;      angle / 3^n <= 0.1
;;  =>  angle / 0.1 <= 3^n
;;  =>  3^n >= 10 angle n
;;  =>  n => cuberoot(10 angle)
;;
;; For angle=12.15 then n=cuberoot(121.5) which is just below 5 (5^3 = 125)
(display "p is called 5 times")
(newline)
