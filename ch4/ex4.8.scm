;;; ex4.8

(load "../global.scm")
(load "./ch4-mceval.scm")
(load "./ex4.6.scm")

;; 名前付きlet をdefineの入れ子の形に直す

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
;> fib
(fib 10)
;> 55
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
;> fib
(fib 10)
;> 55


(define (named-let? exp)
  (if (variable? (cadr exp)) ; リストだったら普通のlet
      true
      false))
(define (named-let-name exp) (cadr exp))
(define (named-let-assignment exp) (caddr exp))
(define (named-let-body exp)
  (cdr (cdr (cdr exp))))
(define (let->combination exp)
  (if (named-let? exp)
      (transform-named-let (named-let-name exp)
                           (named-let-assignment exp)
                           (named-let-body exp))
      (transform-let (let-assignment exp) (let-body exp))))
(define (transform-named-let name assignment body)
  (make-begin 
   (list (cons 'define 
               (cons (cons name (let-var assignment))
                     body))
         (cons name (let-exp assignment)))))

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
;> ok
(fib 10)
;> 55

