#lang sicp

(define (average x y)
	(/ (+ x y ) 2))

(define (sqrt x)
	(define (sqrt-iter guess)
		(if (good-enough? guess)
			guess
			(sqrt-iter (improve guess))
		))
	(define (improve guess)
		(average guess (/ x guess)))
	(define (good-enough? guess)
		;(< (abs (- x (* guess guess))) 0.001))
		(< (abs (- x (* guess guess))) (/ guess 1000000000000000)))
	(sqrt-iter 1.0))

"sqrt"
(sqrt 9)
(sqrt 1000000)
(sqrt 10000000000)
(sqrt 1000000000000000000000000000000000)
(sqrt 1.2)
(sqrt 0.001)

(define (raise x n)
	(if (= n 0)
		1
		(* x (raise x (- n 1)))))
(raise 3 0)
(raise 3 1)
(raise 3 2)
(raise 3 3)
(raise 3 4)

(define (root x power improve)
	(define (iter guess)
		(if (good-enough? guess)
			guess
			(iter (improve guess))
		))
	(define (good-enough? guess)
		(< (abs (- x (raise guess power))) (/ guess 100000000)))
	(iter 1.0))

(define (root-2 x)
	(define (improve guess)
		(average guess (/ x guess)))
	(root x 2 improve))

"root-2"
(root-2 9)
(root-2 1000000)
(root-2 10000000000)
(root-2 10000000000000000000000000)
(root-2 1.2)
(root-2 0.001)


(define (root-3 x)
	(define (improve guess)
		(/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
	(root x 3 improve))

"root-3"
(root-3 27)
(root-3 1000000)
(root-3 10000000000)	
