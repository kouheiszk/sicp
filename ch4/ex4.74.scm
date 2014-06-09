;;; ex4.74

(load "../global.scm")
(load "./common4.4.4.scm")
(load "./ch4-query.scm")
(initialize-data-base microshaft-data-base)
(initialize-data-base empty-data-base)
(query-driver-loop)

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


;; negateを基に考えてみる
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))


(query-driver-loop)
;;; Query input:
(and (job ?x (computer . ?type))
     (not (job ?x (computer programmer))))
;> 
;;; Query results:
;> (and (job (Reasoner Louis) (computer programmer trainee)) 
;>      (not (job (Reasoner Louis) (computer programmer))))
;> (and (job (Tweakit Lem E) (computer technician)) 
;>      (not (job (Tweakit Lem E) (computer programmer))))
;> (and (job (Bitdiddle Ben) (computer wizard)) 
;>      (not (job (Bitdiddle Ben) (computer programmer))))
;> 
;;; Query input:
(job ?x (computer . ?type))
;> 
;;; Query results:
;> (job (Reasoner Louis) (computer programmer trainee))
;> (job (Tweakit Lem E) (computer technician))
;> (job (Fect Cy D) (computer programmer)) ; empty-streamになる
;> (job (Hacker Alyssa P) (computer programmer))
;> (job (Bitdiddle Ben) (computer wizard)) ; empty-streamになる
;>
;;; Query input:
#q

;; simple-flattenに渡るストリームは、
;; 必要な要素と空の要素が入り混じった形式になっている
;; simple-flattenでは、空のストリームを取り除いてあげれば良い

(define (simple-flatten stream)
  (stream-map <??>
              (stream-filter <??> stream)))

;; ↓

(define (simple-flatten stream)
  (stream-map <??>
              (stream-filter (lambda (s) 
                               (not (stream-null? s))) 
                             stream)))

;; ↓

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) 
                               (not (stream-null? s))) 
                             stream)))

;; negateで使ってみます

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map (lambda (s) 
                (stream-car s))
              (stream-filter (lambda (s) 
                               (not (stream-null? s))) 
                             stream)))

(define (negate operands frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))


(query-driver-loop)
;;; query input:
(and (job ?x (computer . ?type))
     (not (job ?x (computer programmer))))
;>
;;; query results:
;> (and (job (reasoner louis) (computer programmer trainee)) 
;>      (not (job (reasoner louis) (computer programmer))))
;> (and (job (tweakit lem e) (computer technician)) 
;>      (not (job (tweakit lem e) (computer programmer))))
;> (and (job (bitdiddle ben) (computer wizard)) 
;>      (not (job (bitdiddle ben) (computer programmer))))
;> 
#q


;; list-valueとfind-assertionsの場合

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map (lambda (s) 
                (stream-car s))
              (stream-filter (lambda (s) 
                               (not (stream-null? s))) 
                             stream)))

(define (lisp-value call frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (find-assertions pattern frame)
  (simple-stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))


(query-driver-loop)
;;; Query input:
(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))
;> 
;;; Query results:
;> (and (salary (Scrooge Eben) 75000) (lisp-value > 75000 30000))
;> (and (salary (Warbucks Oliver) 150000) (lisp-value > 150000 30000))
;> (and (salary (Fect Cy D) 35000) (lisp-value > 35000 30000))
;> (and (salary (Hacker Alyssa P) 40000) (lisp-value > 40000 30000))
;> (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 30000))
;> 
;;; Query input:
#q


;; b. このように変更すると, 質問システムの振舞いは変るか. 

;; 変わらない


