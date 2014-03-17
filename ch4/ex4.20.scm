;;; ex4.20

(load "../global.scm")
(load "./ch4-mceval.scm")
; (load "./ex4.6.scm")
; (load "./ex4.8.scm")
(load "./ex4.16.scm")


;; a

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
        ((letrec? exp) (eval (letrec->let exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


(define (letrec? exp) (tagged-list? exp 'letrec))
(define (make-unassigned-letrec vars)
  (if (null? vars)
      '()
      (cons (list (car vars) ''*unassigned*) 
            (make-unassigned-letrec (cdr vars)))))
(define (make-set-letrec vars exps)
  (if (null? vars)
      '()
      (cons (list 'set! (car vars) (car exps))
            (make-set-letrec (cdr vars) (cdr exps)))))
(define (letrec->let exp)
  (let* ((assignment (let-assignment exp))
         (vars (let-var assignment))
         (exps (let-exp assignment)))
    (cons 'let (cons (make-unassigned-letrec vars)
                     (append (make-set-letrec vars exps)
                             (let-body exp))))))


; (letrec->let '(letrec ((fact
;                          (lambda (n)
;                            (if (= n 1)
;                                1
;                                (* n (fact (- n 1)))))))
;                 (fact 10)))


(letrec ((fact
           (lambda (n)
             (if (= n 1)
                 1
                 (* n (fact (- n 1)))))))
  (fact 10))
;> 3628800
(* 1 2 3 4 5 6 7 8 9 10)
;> 3628800
(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(letrec ((fact
           (lambda (n)
             (if (= n 1)
                 1
                 (* n (fact (- n 1)))))))
  (fact 10))
;> 20


;; b

;; @figure
