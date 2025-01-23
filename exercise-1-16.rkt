#lang sicp

;; 1.16

(define (square x)
  (* x x))
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(fast-expt 2 16)
(fast-expt 2 31)
(fast-expt 2 32)
(fast-expt 2 1000)

(define (fast-expt2 b n)
  (define (iterate n a)
    (cond ((= n 0) a)
	  ((even? n) (iterate (/ n 2) (iterate (/ n 2) a)))
	  (else (iterate (- n 1) (* b a)))))
  (iterate n 1))

(fast-expt2 2 16)
(fast-expt2 2 31)
(fast-expt2 2 32)
(fast-expt2 2 1000)

