;;; 4.1.6 内部定義

;;; 問題4.16

(load "./global.scm")
(load "./ch4-mceval.scm") 

(define the-global-environment (setup-environment))
(driver-loop)
;;; M-Eval value:
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))
;> ok
(append '(a b c) '(d e f))
;> (a b c d e f)
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
;> ok
(fib 5)

;; ex4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-assignment exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-exp assignment)
  (if (null? assignment)
      '()
      (cons (cadr (car assignment))
            (let-exp (cdr assignment)))))
(define (let-var assignment)
  (if (null? assignment)
      '()
      (cons (car (car assignment))
            (let-var (cdr assignment)))))
(define (let->combination exp)
  (transform-let (let-assignment exp) (let-body exp)))
(define (transform-let assignment body)
  (cons (make-lambda (let-var assignment) body)
        (let-exp assignment)))







(define let-exp
  '(let ((a 1)
         (b (list 1 2)))
     (display a)
     (map - b))))

(let->combination let-exp)

(let ()
  (define (let? exp) (tagged-list? exp 'let))
  (define (let-bindings exp) (cadr exp))
  (define (let-body exp) (cddr exp))

  (define (let-binding-var binding) (car binding))
  (define (let-binding-val binding) (cadr binding))

  (define (let->combination exp)
    (let ((bindings (let-bindings exp))
    (body    (let-body     exp)))
      (let ((vars (map let-binding-var bindings))
      (vals (map let-binding-val bindings)))
  (cons (make-lambda vars body)
        vals))))

  ;; (put 'eval 'let  (lambda (exp env)
  ;;         (eval (let->combination exp) env)))

  ;; test
  (define let-exp
    '(let ((a 1)
     (b (list 1 2)))
       (display a)
       (map - b)))

  (let->combination let-exp))

