#lang sicp

;; 1.10



;; 1.11



;; 1.12

(define (pascal-triangle-element row col)
  ;; let both row and col start at 1
  (if (= row 1)
      1
      (cond ((= 1 col) 1)
	    ((= row col) 1)
	    (else (+ (pascal-triangle-element (- row 1) (- col 1))
		     (pascal-triangle-element (- row 1) col))))))
		     
"-"
(pascal-triangle-element 1 1)
"-"
(pascal-triangle-element 2 1)
(pascal-triangle-element 2 2)
"-"
(pascal-triangle-element 3 1)
(pascal-triangle-element 3 2)
(pascal-triangle-element 3 3)
"-"
(pascal-triangle-element 4 1)
(pascal-triangle-element 4 2)
(pascal-triangle-element 4 3)
(pascal-triangle-element 4 4)
"-"
(pascal-triangle-element 5 1)
(pascal-triangle-element 5 2)
(pascal-triangle-element 5 3)
(pascal-triangle-element 5 4)
(pascal-triangle-element 5 5)
"-"
