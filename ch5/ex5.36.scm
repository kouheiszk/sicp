;;; ex5.36

(load "../global.scm")
(load "./ch5-compiler.scm")


(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env) ; envを退避させながら連結
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env) ; envを退避させながら連結
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(construct-arglist
  (map (lambda (operand) (compile operand 'val 'next)) '(1 3)))
;> (() 
;>  (val argl) 
;>  ((assign val (const 3)) 
;>   (assign argl (op list) (reg val)) 
;>   (assign val (const 1)) 
;>   (assign argl (op cons) (reg val) (reg argl))))

;; 被演算子の右から左への評価が行われている

(define (construct-arglist operand-codes)
  (let ((operand-codes operand-codes))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env) ; envを退避させながら連結
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))


(construct-arglist
  (map (lambda (operand) (compile operand 'val 'next)) '(1 3)))
;> (() 
;>  (val argl)     
;>  ((assign val (const 1)) 
;>   (assign argl (op list) (reg val)) 
;>   (assign val (const 3)) 
;>   (assign argl (op cons) (reg val) (reg argl))))

;; reverseが無い分、効率的ではある

