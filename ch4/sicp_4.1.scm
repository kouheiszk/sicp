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

(load "./global.scm")
(load "./ch4-mceval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))
;> ok
(append '(a b c) '(d e f))
;> 



;; 4.15

;; 背理法で頑張る

;; 次の問題と問題4.24まで全部
