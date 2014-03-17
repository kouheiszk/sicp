;;; ex4.24

(load "../global.scm")
(load "./ch4-mceval.scm")
(load "./ch4-analyzingmceval.scm")

;; time で計測できるそうです
;; http://www.serendip.ws/archives/2140

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (time (eval input the-global-environment))))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
;> ok
(fib 100)
;> 354224848179261915075
q

;; ...違いが出ない...
;; analyzeを使わない方が評価のたびに構文解析を繰り返しているため遅くなるはず...
;;
