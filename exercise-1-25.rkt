#lang sicp

;; 1.25
(display "exercise 1.25\n")


;;(#%require "expmod.rkt") -- added inline here
(#%require "math.rkt")
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m)) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fast-expt base exp)
  (cond ((= exp 0) 1)
	((even? exp) (square (fast-expt base (/ exp 2))))
	(else (* base (fast-expt base (- exp 1))))))


;; Q: why not have expmod use the fast-expt helper?
(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))
;; A: the shorter version works too

  ;; (remainder (fast-expt base exp) m))   =>
  ;; (remainder
  ;;   (cond ((= exp 0) 1)
  ;;          ((even? exp) (square (fast-expt base (/ exp 2))))
  ;; 	   (else (* base (fast-expt base (- exp 1))))))
  ;;    m) =>
  ;; (cond ((= exp 0) (remainder 1 m))
  ;;          ((even? exp) (remainder (square (fast-expt base (/ exp 2))) m))
  ;; 	  (else (remainder (* base (fast-expt base (- exp 1))) m))  =>
  ;;  the expmod

(expmod 2 16 7777)
(expmod2 2 16 7777)
(expmod 2 15 5555)
(expmod2 2 15 5555)


