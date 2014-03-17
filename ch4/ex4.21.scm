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
 (lambda (ft k)
   (if (= k 1)
       1
       (* k (ft ft (- k 1))))))

;; ↓

((lambda (ft k)
   (if (= k 1) 
       1
       (* k (ft ft (- k 1)))))
 (lambda (ft k)
   (if (= k 1)
       1
       (* k (ft ft (- k 1)))))
 10)

;; ↓

(if (= 10 1)
    1
    (* 10 ((lambda (ft k)
             (if (= k 1)
                 1
                 (* k (ft ft (- k 1)))))
           (lambda (ft k)
             (if (= k 1)
                 1
                 (* k (ft ft (- k 1)))))
           9)))



;; b



