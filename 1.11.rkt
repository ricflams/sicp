#lang sicp

;; exercise 1.11
(display "exercise 1.11")
(newline)

(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(display "f recursive:\n")
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)

(define (f2  n)
  (define (iter a b c n)
    (if (= n 0)
	a
	(iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (if (< n 3)
      n
      (iter 2 1 0 (- n 2))))

(display "f iterative:\n")
(f2 1)
(f2 2)
(f2 3)
(f2 4)
(f2 5)
(f2 6)
(f2 7)
