;;; ex4.16

(load "../global.scm")
(load "./ch4-mceval.scm")
(load "./ex4.8.scm")

;; a

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (equal? (car vals) "*unassigned*")
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;; b

;; 答え見た
;; @see http://sioramen.sub.jp/blog/2008/02/sicp-416.html

; (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10))
; ;> 20
; (define the-global-environment (setup-environment))
; (driver-loop)
; ;> ;;; M-Eval input:
; (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10))
; ;> 16 ; 20になって欲しい

(define (scan-out-defines exp)
  (let ((lets '()) ; (a '*unassigned*) のリスト
        (sets '()) ; (set! a hogehoge) のリスト
        (bodys '())) ; 本文のリスト
    (let scan-iter ((b exp))
      (cond ((null? b)
             '())
            ((definition? (car b))
             (let ((var (definition-variable (car b)))
                   (val (definition-value (car b))))
               (set! lets (cons (list var ''*unassigned*) lets))
               (set! sets (cons (cons 'set! (list var val)) sets))))
            (else (set! bodys (append bodys (list (car b))))))
      (if (not (null? b))
          (scan-iter (cdr b))))
    (if (null? lets)
        exp
        (list (append (list 'let lets) (append sets bodys))))))

;(scan-out-defines '((define b (+ a x))
;                    (define a 5)
;                    (+ a b))))
;; これに対して動くようにする

;; c

;; make-procedureはprocedure作るときに1度だけしか呼ばれない
;; procedure-bodyは複数回呼ばれる可能性があるので、
;; make-procedureでscan-out-definesしたほうが良い

;(define (procedure-body p) (scan-out-defines (caddr p)))
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


; (define the-global-environment (setup-environment))
; (driver-loop)
; ;> ;;; M-Eval input:
; (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10))
; ;> 20
; 
