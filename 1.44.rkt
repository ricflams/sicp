#lang sicp
(#%require "util.rkt")

;; exercise 1.44
(display "exercise 1.44\n")

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(print-eval ((smooth sin) 0)) ;; exactly 0 because sin(dx) = -sin(-dx) so they cancel out
(print-eval ((smooth cos) 0)) ;; is <1 because cos(dx) and cos(-dx) are < 1 so avg is bust


(define (repeated f n)
  (lambda (x)
    (if (= n 0)
        x
        (f ((repeated f (- n 1)) x)))))

(define (smooth-n f n)
  (repeated (smooth f) n))

(print-eval ((smooth-n sin 10) 0)) ;; stay solid 0
(print-eval ((smooth-n cos 10) 0)) ;; stray further and further away from 1
