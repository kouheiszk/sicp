;;; ex4.2

(load "../global.scm")
(load "./ch4-mceval.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values #?=(operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

;; a

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(define x 3)
;> ERROR

;; define という operator が見つからないから動かない

;; b

(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(call + 1 3)
;> 4

