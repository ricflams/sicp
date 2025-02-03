#lang sicp

; 10
; (+ 5 4 3)
; (- 9 1)
; (/ 6 2)
; (+ (* 2 4) (- 4 6))

;;;;;; 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4.0 5))))) (* 3 (- 6 2) (- 2 7)))

;;;;;;;;; 1.3

(define (sum-of-squares-of-two-larger-numbers a b c)
	(define (square x) (* x x))
	(define (sum-squares x y) (+ (square x) (square y)))
	(define (sum-squares-from-list x) (sum-squares (car x) (car (cdr x))))
	(define (two-largest)
		(cond
			((and (> a c) (> b c)) (list a b))
		 	((and (> a b) (> c b)) (list a c))
			(else (list b c))
		))
	;(apply sum-squares (two-largest))
	(sum-squares-from-list (two-largest))
	)
(sum-of-squares-of-two-larger-numbers 5 6 3)

;;;;;; 1.5
(define (p) (p))
(define (test-a x y)
	(if (= x 0)
		0
		(y)))
(test-a 0 p) ; doesn't evaluate (p)
(define (test-b x y)
	(if (= x 0)
		0
		y))
;(test-b 0 (p)) ; does evaluate (p) and will loop forever


