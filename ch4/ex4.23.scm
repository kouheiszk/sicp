;;; ex4.23

(load "../global.scm")
(load "./ch4-mceval.scm")
(load "./ch4-analyzingmceval.scm")

;; 本文's

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


;; Alyssa's

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))


(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(+ 1 1)
;> 3
(begin (+ 1 1) (+ 2 2))
;> 4
q

;; 答え見た
;; @see http://www.serendip.ws/archives/2122

;; 本文:
;; (+ 1 1)の場合、rest-procがnullなので(+ 1 1)がそのまま実行される
;; 2つ以上の場合、lambdaが作られる
;;
;; Alyssa:
;; (+ 1 1)の場合、(+ 1 1)が含まれたlamdaが作られる
;; 2つ以上の場合、((+ 1 1) ...)のようなリストを本文に持つlambdaが作られる
;;





