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
     (print-with-parenthesis
      (lambda ()
	(print 'expr " -> " expr)))]))

(#%provide print-eval-verify)
(define-syntax print-eval-verify
  (syntax-rules ()
    [(print-eval-verify expr expected)
     (print-with-parenthesis
      (lambda ()
	(let ((actual expr))
	  (define (are-equal a b)
	    (if (boolean? a)
		(or (and a b) (and (not a) (not b)))
		(= a b)))
	  (if (are-equal actual expected)
	      (print 'expr " -> " actual " 💚")
	      (print "\n" 'expr " -> " actual " IS NOT EQUAL TO " expected " ❌\n")))))]))

(#%provide print-eval-compare)
(define-syntax print-eval-compare
  (syntax-rules ()
    [(print-eval-compare expr1 expr2)
     (print-with-parenthesis
      (lambda ()
        (print 'expr1 " -> " expr1 "    " 'expr2 " -> " expr2)))]))

(#%provide to-string)
(define (to-string . items)
  (string-join
   (map (lambda (item)
          (cond
            [(number? item) (number->string item)]
            [(symbol? item) (symbol->string item)]
            [(string? item) item]
            [(boolean? item) (if item "true" "false")]
            [(char? item) (string item)]
            [else (format "~a" item)]))
        items)
   ""))

(#%provide with-precision)
(define (with-precision value decimals)
  (~r value #:precision decimals))
