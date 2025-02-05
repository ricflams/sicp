#lang sicp

(#%provide print)
(define (print . x)
  (for-each display x)
  (newline))

(#%provide print-eval)
(define (print-eval expr ns)
  (display expr)
  (display " = ")
  (display (eval expr ns));;(scheme-report-environment 5)))
  (newline))

;; ;;(define my-namespace (make-base-namespace))
;; ;;(namespace-attach-myself my-namespace)

;; ;;(make-base-empty-namespace)
;; (define sq (lambda (x) (* x x)))
;; (define my-env (scheme-report-environment 5))
;; (define (add-to-env env name value)
;;   (eval '(define name value) env) (scheme-report-environment 5))
;; (add-to-env my-env 'root-2 root-2)

;; (eval '(sq 9) my-env)
;; (eval '(root-2 9) my-env)
;; (eval '(root-2 9) )
;; (print-eval '(root-2 9) (interaction-environment))
;; ;; (print-eval '(root-2 1000000) my-namespace)

