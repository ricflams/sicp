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

(define (cube-root x)
	(define (iter guess)
		(if (good-enough? guess)
			guess
			(iter (improve guess))
		))
	(define (improve guess)
		(/
			(+ (/ x (* guess guess)) (* 2 guess))
			3))
	(define (good-enough? guess)
		;(< (abs (- x (* guess guess))) 0.001))
		(< (abs (- x (* guess guess guess))) (/ guess 100000000)))
	(iter 1.0))

"cube-root"
(cube-root 27)
(cube-root 1000000)
(cube-root 10000000000)
; (cube-root 100000000000000000)
; (cube-root 1.2)
; (cube-root 0.001)