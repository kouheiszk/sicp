;;; 4.1 超循環評価器

(load "../global.scm")
(load "./ch4-mceval.scm")

(define the-global-environment (setup-environment))
(driver-loop)
;> ;;; M-Eval input:
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))
;> ok
(append '(a b c) '(d e f))
;> (a b c d e f)

