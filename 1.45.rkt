#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")
(#%require "expt.rkt")
(#%require "fixed-point.rkt")

;; exercise 1.45
(display "exercise 1.45\n")

;; An avg damper for the nth root, y -> x/y^(n-1)
;; Eg
;;    n = 2  =>  y -> x/y
;;    n = 3  =>  y -> x/y²
;;    n = 4  =>  y -> x/y³
(define (average-damp-root-n n)
  (lambda (x)
    (lambda (y)
      (average y (/ x (expt y (- n 1)))))))

(define (sqrt x)
  (fixed-point ((average-damp-root-n 2) x) 1.0))

(print-eval (sqrt 2))
(print-eval (sqrt 9))
(print-eval (sqrt 10))


(define (cube-root x)
  (fixed-point ((average-damp-root-n 3) x) 1.0))

(print-eval (cube-root 8))
(print-eval (cube-root 27))
(print-eval (cube-root 125))

;; This does not converge
;; (define (fourth-root-bad x)
;;   (fixed-point (average-damp-root-n x 4) 1.0))
;; (print-eval (fourth-root-bad 8))

;; Bring in procedure (repeated f n) from 1.44
(define (repeated f n)
  (lambda (x)
    (if (= n 0)
        x
        (f ((repeated f (- n 1)) x)))))

(define (root-4 x)
  (fixed-point (repeated ((average-damp-root-n 4) x) 2) 1.0))

(print-eval (root-4 16))
(print-eval (root-4 81))
(print-eval (root-4 625))

;; This repeats will converge: 0, 2, 4, 6, 8, ...
;; So apparently all the even repeats will converge
(define (root-5 x)
  (fixed-point (repeated ((average-damp-root-n 5) x) 4) 1.0))
(print-eval (root-5 32))

;; This repeats will converge: 0, 1, 2, 3, 4, 5, ...
;; So apparently all repeats will converge
(define (root-6 x)
  (fixed-point (repeated ((average-damp-root-n 6) x) 5) 1.0))
(print-eval (root-6 64))

;; This repeats will converge: 0, 1, 2, 3, 4, 5, ...
;; So apparently all repeats will converge
(define (root-7 x)
  (fixed-point (repeated ((average-damp-root-n 7) x) 5) 1.0))
(print-eval (root-7 128))


;; Define n-th root, based on the empirical findings above
(define (root-n n x)
  (let ((repeats (cond ((= n 1) 0) ;; x itself, no damping
                       ((= n 2) 1) ;; 1 damp needed
                       ((= n 3) 1) ;; 1 damp needed
                       ((= n 4) 2) ;; 2 damps needed
                       ((= n 5) 2) ;; an even number of damps needed
                       (else 1)))) ;; pick an
    (fixed-point (repeated ((average-damp-root-n n) x) repeats) 1.0)))

(print-eval-compare (root-n 1 64) (expt (root-n 1 64) 1))
(print-eval-compare (root-n 2 64) (expt (root-n 2 64) 2))
(print-eval-compare (root-n 3 64) (expt (root-n 3 64) 3))
(print-eval-compare (root-n 4 64) (expt (root-n 4 64) 4))
(print-eval-compare (root-n 5 64) (expt (root-n 5 64) 5)) ;; is way off, no matter what?!
(print-eval-compare (root-n 6 64) (expt (root-n 6 64) 6))
(print-eval-compare (root-n 7 64) (expt (root-n 7 64) 7))
(print-eval-compare (root-n 8 64) (expt (root-n 8 64) 8))
(print-eval-compare (root-n 9 64) (expt (root-n 9 64) 9))



