;;; ex4.1

(load "../global.scm")
(load "./ch4-mceval.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
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
        ((application? exp)
         (apply (eval (operator exp) env)
                ;(list-of-values (operands exp) env))) ; original
                ;(list-of-values-left (operands exp) env))) ; left 
                (list-of-values-right (operands exp) env))) ; right
        (else
          (error "Unknown expression type -- EVAL" exp))))


;; original

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(define value 10)
;> ok
(begin (set! value (+ value 2)) value)
;> 12 
(begin (set! value (* value 2)) value)
;> 24
(define value 10)
;> ok
(+ (begin (set! value (+ value 2)) value) (begin (set! value (* value 2)) value))
;> 36


;; left

(define (list-of-values-left exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first
              (list-of-values-left (rest-operands exps) env)))))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(define value 10)
;> ok
(+ (begin (set! value (+ value 2)) value) (begin (set! value (* value 2)) value))
;> 36


;; right

(define (list-of-values-right exps env)
  (if (no-operands? #?=exps)
      '()
      (let ((rest (list-of-values-right (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest))))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(define value 10)
;> ok
(+ (begin (set! value (+ value 2)) value) (begin (set! value (* value 2)) value))
;> 42

