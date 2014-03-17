;;; ex4.21

(load "../global.scm")
(load "./ch4-mceval.scm")

;; a

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
;> 3628800
(* 1 2 3 4 5 6 7 8 9 10)
;> 3628800
(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
;> 3628800


;; lambdaを展開してみる

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;; ↓

((lambda (fact)
   (fact fact 10))
 (lambda (ft k) ;; 手続きft と 引数k をとる
   (if (= k 1)
       1
       (* k (ft ft (- k 1))))))


;; fibを作る

(define (fib n)
  (let fib-iter ((k n))
    (cond ((= k 0) 0)
          ((= k 1) 1)
          (else (+ (fib-iter (- k 1))
                   (fib-iter (- k 2)))))))
(fib 10)

;; ↓

((lambda (n)
   (let fib-iter ((k n))
     (cond ((= k 0) 0)
           ((= k 1) 1)
           (else (+ (fib-iter (- k 1))
                    (fib-iter (- k 2)))))))
 10)

;; ↓

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (ft k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (ft ft (- k 1))
                     (ft ft (- k 2))))))))
 10)

;; b

; (define (f x)
;   ((lambda (even? odd?)
;      (even? even? odd? x))
;    (lambda (ev? od? n)
;      (if (= n 0) true (od? ⟨??⟩ ⟨??⟩ ⟨??⟩)))
;    (lambda (ev? od? n)
;      (if (= n 0) false (ev? ⟨??⟩ ⟨??⟩ ⟨??⟩)))))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))


