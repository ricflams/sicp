#lang racket

(#%provide print)
(define (print . x)
  (for-each display x)
  (newline))

(define (print-with-parenthesis thunk)
  (define use-curly (print-pair-curly-braces))
  (print-pair-curly-braces #f)
  (thunk)
  (print-pair-curly-braces use-curly))
  
(#%provide print-eval)
(define-syntax print-eval
  (syntax-rules ()
    [(print-eval expr)
     (begin
       (print-with-parenthesis
	(lambda ()
	  (print 'expr " -> " expr))))]))

