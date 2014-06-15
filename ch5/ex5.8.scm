;;; ex5.7

(load "./ch5-regsim.scm")

(define test-machine
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
      here
       (assign a (const 3))
       (goto (label there))
      here
       (assign a (const 4))
       (goto (label there))
      there)))
;> test-machine
(set-register-contents! test-machine 'a 0)
;> done
(start test-machine)
;> done
(get-register-contents test-machine 'a)
;> 3

;; 2つめのhereにはジャンプしない


;;; 同じラベル名が二つの異る場所を指すように使われたら, エラーとする

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels) ; next-instが存在するか？
                   (error "Label has already defined: " next-inst)
                   (receive insts
                            (cons (make-label-entry next-inst
                                                    insts)
                                  labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))


(define test-machine
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
      here
       (assign a (const 3))
       (goto (label there))
      here
       (assign a (const 4))
       (goto (label there))
      there)))
;> ERROR


