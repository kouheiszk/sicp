;;; ex4.70

(load "../global.scm")
(load "./common4.4.4.scm")
(load "./ch4-query.scm")
(initialize-data-base microshaft-data-base)
;(initialize-data-base empty-data-base)

(find-assertions (query-syntax-process '(job ?x (computer programmer)))
                 (singleton-stream '()))

(query-syntax-process '(job ?x (computer programmer)))

(display-stream (find-assertions (query-syntax-process '(job ?x (computer programmer)))
                 (singleton-stream '())))



(unify-match 1 '(? x)
             (singleton-stream '()))
 
(unify-match '(? x) 1
             (singleton-stream '()))
 
(unify-match '(? x) '(? x)
             (singleton-stream '()))
 
;(query-syntax-process '(?x ?x))
;(?x ?x) (?y ?y)
(unify-match '((? x) (? x)) '((? y) (? y))
             (singleton-stream '()))
 
(unify-match '((? y) (? x)) '((? x) (? y))
             (singleton-stream '()))
 
(unify-match '(? x) 1
             (extend '(? x) 2
                     (singleton-stream '())))
 
(unify-match '(? x) 2
             (extend '(? x) 2
                     (singleton-stream '())))
 
(unify-match '((? x) (? y)) '((? y) 1)
             (extend '(? x) 2
                     (singleton-stream '())))
 
(depends-on? '((? x) (? x)) '(? x)
             (singleton-stream '()))
(depends-on? '(? x) '(? x)
             (singleton-stream '()))
(depends-on? '(? x) '(? y)
             (singleton-stream '()))



(fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '()))
 
(apply-rules (query-syntax-process '(wheel ?p)) (singleton-stream '()))


(apply-a-rule (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '())))
              (query-syntax-process '(wheel ?p)) (singleton-stream '()))
 
(display-stream
  (stream-map
    (lambda (frame)
      (instantiate
        (query-syntax-process '(wheel ?p))
        frame
        (lambda (v f) (v))))
    (apply-a-rule (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '())))
                  (query-syntax-process '(wheel ?p)) (singleton-stream '()))))
 
(rename-variables-in
  (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '()))))






;; 手続きadd-assertion!とadd-rule!のlet束縛の目的は何か. 
;; add-assertion!の次の実装は何が悪いか. 
;; ヒント: 3.5.2節の一たちの無限ストリームの定義: (define ones (cons-stream 1 ones))を思い出そう.


(define ones (cons-stream 1 ones))



(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)

;; この場合、THE-ASSERTIONSはassertionとTHE-ASSERTIONSを評価するペアとなる。
;; carがassertionで、
;; cdrはassertionとTHE-ASSERTIONSを評価する約束のペアとなる。
;; この場合cdrをたどっても以前にストリームに入っていたassetionを取り出すことができなくなる


