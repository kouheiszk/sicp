;;; ex4.7

(load "../global.scm")
(load "./ch4-mceval.scm")
(load "./ex4.6.scm")

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
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-assignment exp) (cadr exp))
(define (let*-body exp) (cddr exp))

(define (let*->nested-lets exp)
  (transform-let* (let*-assignment exp) (let*-body exp)))
(define (transform-let* assignment body)
  (if (null? (cdr assignment))
      (cons 'let (cons assignment body))
      (list 'let (list (car assignment))
            (transform-let* (cdr assignment) body))))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))
;> 39

