;;; ex5.7

(load "./ch5-regsim.scm")

;;; 問題5.4で設計した計算機をテストせよ

(define expt-machine
  (make-machine
    '(b n val continue)
    (list (list '= =) (list '* *) (list '- -))
    '((assign continue (label expt-done))
      expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-expt))
        (goto (label expt-loop))
      after-expt
        (restore n)
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      expt-done)))

(set-register-contents! expt-machine 'b 2)
;> done
(set-register-contents! expt-machine 'n 8)
;> done
(start expt-machine)
;> done
(get-register-contents expt-machine 'val)
;> 256

