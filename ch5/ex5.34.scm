;;; ex5.34

(load "../global.scm")
(load "./ch5-compiler.scm")

(compile
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
     (iter 1 1))
  'val
  'next)


((env) 
 (val) 
 ((assign val (op make-compiled-procedure) (label entry45) (reg env)) 
  (goto (label after-lambda46)) 
entry45 ; factorialの呼出しスタート
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 
  (assign val (op make-compiled-procedure) (label entry47) (reg env)) 
  (goto (label after-lambda48)) 
entry47 
  (assign env (op compiled-procedure-env) (reg proc)) 
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env)) 
  (save continue) 
  (save env) 
  (assign proc (op lookup-variable-value) (const >) (reg env)) 
  (assign val (op lookup-variable-value) (const n) (reg env)) 
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch52)) 
compiled-branch53 
  (assign continue (label after-call54)) 
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch52 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call54 
  (restore env) 
  (restore continue) 
  (test (op false?) (reg val)) 
  (branch (label false-branch50)) 
true-branch49 
  (assign val (op lookup-variable-value) (const product) (reg env)) 
  (goto (reg continue))
  false-branch50 
  (assign proc (op lookup-variable-value) (const iter) (reg env)) 
  (save continue) 
  (save proc) 
  (save env) 
  (assign proc (op lookup-variable-value) (const +) (reg env)) 
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const counter) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch58)) 
compiled-branch59 
  (assign continue (label after-call60)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch58 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call60 
  (assign argl (op list) (reg val)) 
  (restore env) 
  (save argl) 
  (assign proc (op lookup-variable-value) (const *) (reg env)) 
  (assign val (op lookup-variable-value) (const product) (reg env)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const counter) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch55)) 
compiled-branch56 
  (assign continue (label after-call57)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) primitive-branch55 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call57 
  (restore argl) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (restore proc) 
  (restore continue) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch61)) 
compiled-branch62 
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch61 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (goto (reg continue)) 
after-call63 
after-if51 
after-lambda48
;; (define (iter product counter)) の初期化部分
  (perform (op define-variable!) (const iter) (reg val) (reg env)) 
  (assign val (const ok)) 
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  ;; (iter 1 1) のiterに渡す引数部分を作ってる
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (const 1)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch64)) 
compiled-branch65 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch64 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (goto (reg continue)) 
after-call66 
after-lambda46 
  (perform (op define-variable!) (const factorial) (reg val) (reg env)) 
  (assign val (const ok))))
