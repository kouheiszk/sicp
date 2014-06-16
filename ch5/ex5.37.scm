;;; ex5.37

(load "../global.scm")
(load "./ch5-compiler.scm")


(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                          (list-union (list first-reg)
                                      (registers-needed seq1))
                          (list-difference (registers-modified seq1)
                                           (list first-reg))
                          (append `((save ,first-reg))
                                  (statements seq1)
                                  `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))


(compile '(+ 1 (* 2 3)) 'val 'next)

((env) 
 (env proc argl continue val) 
 ((assign proc (op lookup-variable-value) (const +) (reg env)) 
  (save proc) ; ここでsave
  (assign proc (op lookup-variable-value) (const *) (reg env)) 
  (assign val (const 3)) 
  (assign argl (op list) (reg val)) 
  (assign val (const 2)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch16)) 
compiled-branch17 
  (assign continue (label after-call18)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch16 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call18 
  (assign argl (op list) (reg val)) 
  (assign val (const 1)) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (restore proc) ; ここでrestore
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch19)) 
compiled-branch20 
  (assign continue (label after-call21)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch19 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
after-call21))


;; needs-register?と
;; modifies-register?の比較部分を消して、
;; 常にsaveとrestoreを行うようにした

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (preserving (cdr regs)
                    (make-instruction-sequence
                      (list-union (list first-reg)
                                  (registers-needed seq1))
                      (list-difference (registers-modified seq1)
                                       (list first-reg))
                      (append `((save ,first-reg))
                              (statements seq1)
                              `((restore ,first-reg))))
                    seq2))))


(compile '(+ 1 (* 2 3)) 'val 'next)

((env continue) 
 (env proc argl continue val) 
 ((save continue) 
  (save env) 
  (save continue) 
  (assign proc (op lookup-variable-value) (const +) (reg env)) 
  (restore continue) 
  (restore env) 
  (restore continue) 
  (save continue) 
  (save proc) 
  (save env) 
  (save continue) 
  (save env) 
  (save continue) 
  (assign proc (op lookup-variable-value) (const *) (reg env)) 
  (restore continue) 
  (restore env) 
  (restore continue) 
  (save continue) 
  (save proc) 
  (save env) 
  (save continue) 
  (assign val (const 3)) 
  (restore continue) 
  (assign argl (op list) (reg val)) 
  (restore env) 
  (save argl) 
  (save continue) 
  (assign val (const 2)) 
  (restore continue) 
  (restore argl) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (restore proc) 
  (restore continue) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch34)) 
compiled-branch35 
  (assign continue (label after-call36)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch34 
  (save continue) 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (restore continue) 
after-call36 
  (assign argl (op list) (reg val)) 
  (restore env) 
  (save argl) 
  (save continue) 
  (assign val (const 1)) 
  (restore continue) 
  (restore argl) 
  (assign argl (op cons) (reg val) (reg argl)) 
  (restore proc) 
  (restore continue) 
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch37)) 
compiled-branch38 
  (assign continue (label after-call39)) 
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) 
primitive-branch37 
  (save continue) 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
  (restore continue) after-call39))

