;;; ex4.71

(load "../global.scm")
(load "./common4.4.4.scm")
(load "./ch4-query.scm")
(initialize-data-base microshaft-data-base)
(initialize-data-base empty-data-base)

;;; 質問システムが実装できていることを確認する　

(query-driver-loop)

;;; Query input:
(job ?x (computer programmer))
;;; Query results:
;> (job (Fect Cy D) (computer programmer))
;> (job (Hacker Alyssa P) (computer programmer))
;>
;;; Query input:
(job ?x (computer ?type))
;;; Query results:
;> (job (Tweakit Lem E) (computer technician))
;> (job (Fect Cy D) (computer programmer))
;> (job (Hacker Alyssa P) (computer programmer))
;> (job (Bitdiddle Ben) (computer wizard))
;>
;;; Query input:
(job ?x (computer . ?type))
;;; Query results:
;> (job (Reasoner Louis) (computer programmer trainee))
;> (job (Tweakit Lem E) (computer technician))
;> (job (Fect Cy D) (computer programmer))
;> (job (Hacker Alyssa P) (computer programmer))
;> (job (Bitdiddle Ben) (computer wizard))
#q

;; Louis Reasonerはsimple-queryとdisjoin手続き(4.4.4.2節)が次のようには定義せず
;; delay演算を陽に使って実装している理由が分らない:

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts) frame-stream))))

;; より単純な定義だと, 望ましくない振舞いになる質問の例が書けるか. 

(initialize-data-base empty-data-base)
(query-driver-loop)

;;; Query input:
(assert! (married Minnie Mickey))
;> Assertion added to data base.
;>
;;; Query input:
(married Minnie ?who)
;;; Query input:
(married Mickey ?who)
;;; Query results:
;>
;;; Query input:
(assert! (rule (married ?x ?y)
               (married ?y ?x)))
;> Assertion added to data base.
;>
;;; Query input:
(married Mickey ?who)
;;; Query results:
;> (married Minnie Mickey)
;> (married Minnie Mickey)
;> (married Minnie Mickey)
;> ...


;; simple-query は apply-rules を呼び、
;; apply-rules は apply-a-rule を読んでいる。
;; Minnie と Mickey のパターンでは unify された結果を得ることができず
;; 再び qeval が呼ばれ simple-query が呼ばれる。
;;
;; ここで無限ループが発生する 
;; -> delayがあると印字が行われるが、delayが無いとそうではない

