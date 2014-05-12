;;; ex4.72

(load "../global.scm")
(load "./common4.4.4.scm")
(load "./ch4-query.scm")
(initialize-data-base microshaft-data-base)
(initialize-data-base empty-data-base)

;; disjoinとstream-flatmapが単に連接しないで
;; ストリームを差込みにするのはなぜか。
;; 差込みがうまく働くのが分る例を示せ。
;; (ヒント: 3.5.3節でinterleaveを使ったのはなぜか。)

;; disjoin、stream-flatmapを用いたのは、4.71と同じくdisjoinで無限ループが発生するため

;; delayを使用するflatten-stream
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
        (stream-car stream)
        (delay (flatten-stream (stream-cdr stream))))))

;; delayを使用しないflatten-stream
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
        (stream-car stream)
        (flatten-stream (stream-cdr stream))))) ;; ここで無限ループ...

