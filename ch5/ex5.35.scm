;;; ex5.35

(load "../global.scm")
(load "./ch5-compiler.scm")

  (assign val (op make-compiled-procedure) (label entry16) (reg env))
  (goto (label after-lambda15))
entry16
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env
          (op extend-environment) (const (x)) (reg argl) (reg env)) ; xを引数にとる
  (assign proc (op lookup-variable-value) (const +) (reg env))
;; 手続きの始まり
  (save continue)
;; +を退避
  (save proc)
  (save env)
;; g
  (assign proc (op lookup-variable-value) (const g) (reg env))
;; gを退避
  (save proc)
;; (+ x 2)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)) ; procは+
  (branch (label primitive-branch19))
compiled-branch18
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17 ; valには(+ x 2)の結果が入っている
  (assign argl (op list) (reg val))
;; gを復活
  (restore proc)
  (test (op primitive-procedure?) (reg proc)) ; procはg
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20)) 
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20 ; valには (g (+ x 2))の結果が入っている
;; (x (g (+ x 2))) という被演算引数をつくる
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
;; +を復活
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc)) ; procは+
  (branch (label primitive-branch25))
compiled-branch24
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch25
;; (+ (x (g (+ x 2))))
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue)) ; 終了
after-call23
after-lambda15
;; (define f ,label-entry16)
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))


;; こんな感じの手続き

(define (f x) 
  (+ (x (g (+ x 2)))))

;; compile
;(compile '(define (f x) (+ (x (g (+ x 2))))) 'val 'next)

