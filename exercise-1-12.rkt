#lang sicp

;; 1.12

(define (tri row col)
  ;; let both row and col start at 1
  (if (= row 1)
      1
      (cond ((= 1 col) 1)
	    ((= row col) 1)
	    (else (+ (tri (- row 1) (- col 1))
		     (tri (- row 1) col))))))

(define (print-odd a b c d e)
  (define (p x)
    (display x) (display " "))
  (p a) (p b) (p c) (p d) (p e) (newline))
(define (print-eve a b c d e)
  (display " ")
  (print-odd a b c d e))

(print-odd " " " " (tri 1 1) "" "")
(print-eve " " (tri 2 1) (tri 2 2) "" "")
(print-odd " " (tri 3 1) (tri 3 2) (tri 3 3) "")
(print-eve (tri 4 1) (tri 4 2) (tri 4 3) (tri 4 4) "")
(print-odd (tri 5 1) (tri 5 2) (tri 5 3) (tri 5 4) (tri 5 5))
