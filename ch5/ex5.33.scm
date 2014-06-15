;;; ex5.33

(load "../global.scm")
(load "./ch5-compiler.scm")

(compile 
  '(define (factorial-alt n)
     (if (= n 1)
         1
         (* n (factorial-alt (- n 1)))))
  'val 
  'next)

((env) 
 (val)
;; 手続きを構成し手続き本体のコードを飛び越す
 ((assign val (op make-compiled-procedure) (label entry154) (reg env)) 
  (goto (label after-lambda155)) 
entry154 
  (assign env (op compiled-procedure-env) (reg proc)) 
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) 
;; 手続き本体の開始
  (save continue) 
  (save env) 
;; (= n 1)の計算
  (assign proc (op lookup-variable-value) (const =) (reg env)) 
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const n) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch159)) 
compiled-branch160 
  (assign continue (label after-call161)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch159 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call161 ; valには(= n 1)の結果がある
  (restore env) 
  (restore continue) 
  (test (op false?) (reg val)) 
  (branch (label false-branch157)) 
true-branch156 ; 1を返す
  (assign val (const 1)) 
  (goto (reg continue)) 
false-branch157 
;; (* n (factorial (- n 1)))を計算し返す
  (assign proc (op lookup-variable-value) (const *) (reg env)) 
  (save continue) 
  (save proc) 
  (save env)
;; 引数(factorial-alt (- n 1))の計算
  (assign proc (op lookup-variable-value) (const factorial-alt) (reg env)) 
  (save proc) ; factorial-alt手続きを退避
;; factorialの引数(- n 1)の計算
  (assign proc (op lookup-variable-value) (const -) (reg env)) 
  (assign val (const 1)) 
  (assign argl (op list) (reg val)) 
  (assign val (op lookup-variable-value) (const n) (reg env)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch162)) 
compiled-branch163 
  (assign continue (label after-call164)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch162 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call164 
  (assign argl (op list) (reg val)) 
  (restore proc) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch165)) 
compiled-branch166 
  (assign continue (label after-call167)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch165 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call167 ; valには(factorial-alt (- n 1))の結果がある
  (assign argl (op list) (reg val)) 
  (restore env) 
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl)) 
  (restore proc) ; *掛け算になるはず 
  (restore continue) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch168)) 
compiled-branch169 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch168 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (goto (reg continue)) 
after-call170 
after-if158 
after-lambda155 
  (perform (op define-variable!) (const factorial-alt) (reg val) (reg env)) 
  (assign val (const ok)))) 


;; 再帰呼び出しの部分で、
;; factorial-altを評価するコードが先に作られている部分が異なっている。
;; save、restoreの数が変わらないので、効率は同じ?
;; コードはこちらの方が追いやすかった気がする...。

