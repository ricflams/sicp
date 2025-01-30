#lang sicp

(#%require "math.rkt")

(define (expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (expt b (/ n 2))))
	(else (* b (expt b (- n 1))))))
