;;; ex4.4

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
        ;((and? exp) (eval-and exp env))
        ;((or? exp) (eval-or exp env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


;; and

(define (and? exp)
  (tagged-list? exp 'and))
(define (eval-and exp env)
  (let check ((operands (cdr exp)))
    (cond ((null? operands) true)
          ((true? (car operands)) (check (cdr operands)))
          (else false))))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(and 1 2 3 4)
;> #t
(and 1 2 3 #f 4)
;> #f


;; or

(define (or? exp)
  (tagged-list? exp 'or))
(define (eval-or exp env)
  (let check ((operands (cdr exp)))
    (cond ((null? operands) false)
          ((true? (car operands)) true)
          (else (check (cdr operands))))))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(or 1 2 3 4)
;> #t
(or 1 2 3 #f 4)
;> #t
(or #f #f #f)
;> #f


;; 導出された式として...
;; and や or を if に直す

(define (and-clauses exp) (cdr exp))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (car clauses)
               (expand-and-clauses (cdr clauses))
               'false)))
(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (or-clauses exp) (cdr exp))
(define (expand-or-cluases clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-or-cluases (cdr clauses)))))
(define (or->if exp)
  (expand-or-cluases (or-clauses exp)))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(and 1 2 3 4)
;> #t
(and 1 2 3 false 4)
;> #f
(or 1 2 3 4)
;> #t
(or 1 2 3 false 4)
;> #t
(or false false)
;> #f

