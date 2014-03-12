;; schemeの上にschemeを作る？
;; AMB
;;
;; 4.1.5 の問題まで
;; ただし問題4.3は除く。問題4.9の問題はスキップしていいよ。
;; 問題4.10はやらないでいいよね...。
;;

;; 超循環評価器
;; 

;; 4.1 超言語的抽象

(load "../global.scm")
(load "./ch4-mceval.scm")

;; ex4.6

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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

;; (let ((x 100)) (+ x 1))
;; '('let '( '('(x 100)) '(+ x 1) ))
(define (let? exp) (tagged-list? exp 'let))
(define (let-assignment exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-var assignment)
  (if (null? assignment)
      '()
      (cons (car (car assignment))
            (let-var (cdr assignment)))))
(define (let-exp assignment)
  (if (null? assignment)
      '()
      (cons (cadr (car assignment))
            (let-exp (cdr assignment)))))
  
(define (let->combination exp)
  (transform-let (let-assignment exp) (let-body exp)))
(define (transform-let assignment body)
  (cons (make-lambda (let-var assignment) body)
        (let-exp assignment)))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(let ((x 100)) (+ x 1))
;> 101

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))
;> ok
(append '(a b c) '(d e f))
;> (a b c d e f)

;; 4.15

;; 背理法で頑張る

;; 次の問題と問題4.24まで全部
