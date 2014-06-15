;;; ex5.23

(load "../global.scm")
(load "./load-eceval.scm")
(define the-global-environment (setup-environment))
(start eceval)

;;; EC-Eval input:
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))
;>(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
;>ok
;>
;;; EC-Eval input:
(append '(a b c) '(d e f))
;>(total-pushes = 118 maximum-depth = 17)
;;; EC-Eval value:
;> (a b c d e f)
;>
