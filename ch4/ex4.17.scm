;;; ex4.17

;; @figure

;; letをlambdaに変換するために余計なフレームが生成される
;; letを使わなければ余計なフレームは増えない
;; letに変換していたものを
;;
;;(lambda ⟨vars⟩
;;  (let ((u '*unassigned*)
;;        (v '*unassigned*))
;;    (set! u ⟨e1⟩)
;;    (set! v ⟨e2⟩)
;;    ⟨e3⟩))
;; 
;; letを使わないようにdefineとset!に変換するようにする
;; 
;;(lambda <vars>
;;  (define u '*unassigned*)
;;  (define v '*unassigned*)
;;  (set! u <e1>)
;;  (set! v <e2>)
;;  <e3>)

