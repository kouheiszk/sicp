;;; ex4.70

(load "../global.scm")
(load "./common4.4.4.scm")
(load "./ch4-query.scm")


;; 手続きadd-assertion!とadd-rule!のlet束縛の目的は何か. 
;; add-assertion!の次の実装は何が悪いか. 
;; ヒント: 3.5.2節の一たちの無限ストリームの定義: (define ones (cons-stream 1 ones))を思い出そう.

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)

;; この場合、THE-ASSERTIONSはassertionとTHE-ASSERTIONSを評価するペアとなる。
;; carがassertionで、
;; cdrはassertionとTHE-ASSERTIONSを評価する約束のペアとなる。
;; この場合cdrをたどっても以前にストリームに入っていたassetionを取り出すことができなくなる

