#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.15
(display "exercise 1.15\n")


(define (sine angle)
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
;;  =>  3^n >= 10 angle
;;  =>  n log(3) >= log(10 angle)
;;  =>  n => log(10 angle)/log(3)
;;
;; For angle=12.15 then n >= log(121.5)/log(3) = 4.37
(display "p is called 5 times\n")
(display "order is O(log(angle))\n")
