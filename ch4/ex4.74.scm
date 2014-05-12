;;; ex4.74

(load "../global.scm")
(load "./common4.4.4.scm")
(load "./ch4-query.scm")
(initialize-data-base microshaft-data-base)
(initialize-data-base empty-data-base)
(query-driver-loop)
#q

;; Alyssa P. Hackerはnegate, lisp-valueとfind-assertionsでのstream-flatmapに,
;; より単純な版を使うことを提案した. 
;; 彼女は, これらの場合で, フレームのストリームにマップされるこの手続きは, 
;; 常に空ストリームか単一ストリームを生じ, ストリームを組み合せるのに差込みは必要ないと考えた. 


;; a. Alyssaのプログラムの欠けた式を補え.
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map <??>
              (stream-filter <??> stream)))


;;;
(job ?x (computer programmer))
(not (job ?x (computer programmer)))
(define q1 (query-syntax-process '(job ?x (computer programmer))))
(define q2 (query-syntax-process '(not (job ?x (computer programmer)))))
q1
q2
(qeval q1 (singleton-stream '()))
(qeval q2 (singleton-stream '()))
(get (type q2) 'qeval)
(contents q2)
(define q2proc (get (type q2) 'qeval))
(q2proc (contents q2) (singleton-stream '()))
(negated-query (contents q2))

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands) ;; frameを質問で拡張しようと試みる
                              (singleton-stream frame)))
         (singleton-stream frame) ;; 拡張できなかったものを返す
         the-empty-stream))
   frame-stream))








(computer wizard)


;;; Query input:
;;; Query results:
;> (job (Fect Cy D) (computer programmer))
;> (job (Hacker Alyssa P) (computer programmer))
;>
;;; Query input:


(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame) ;; 拡張フレームのストリームを作る
      (delay (apply-rules query-pattern frame)))) ;; 可能な規則をすべて作用させて拡張されたフレームのもう一つのストリームを作る
   frame-stream))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; b. このように変更すると, 質問システムの振舞いは変るか. 
