;;; ex4.75

(load "../global.scm")
(load "./common4.4.4.scm")
(load "./ch4-query.scm")
(initialize-data-base microshaft-data-base)
(initialize-data-base empty-data-base)

;; 質問言語にuniqueという新しい特殊形式を実装せよ. 
;; uniqueは指定した質問を満足する項目がデータベースに唯一つある時に成功する. 
;; 例えば
;; (unique (job ?x (computer wizard)))
;; は, Benは唯一人の計算機達人[computer wizard]なので, 一項目ストリーム
;; (unique (job (Bitdiddle Ben) (computer wizard)))
;; を印字する. 
;; また
;; (unique (job ?x (computer programmer)))
;; は, 一人を超える計算機プログラマがいるので, 空ストリームを印字する. 
;; 更に
;; (and (job ?x ?j) (unique (job ?anyone ?j)))
;; は, 一人だけが担当している仕事すべての担当者をリストする.
;; 
;; uniqueの実装には二つの部分がある. 
;; 第一はこの特殊形式を扱う手続きを書くこと, 第二はqevalにその手続きに振り分けさせることである.
;; qevalは振分けをデータ主導で行うので第二の部分は容易である. 
;; この手続きをuniquely-assertedと呼ぶなら, なすべきことは
;; 
;; (put 'unique 'qeval uniquely-asserted)
;; で, そうするとqevalはそのtype(car)が記号unique のすべての質問をこの手続きに振り分ける.
;;
;; 本当の問題は, 手続きuniquely-assertedを書くことである. 
;; これは入力としてunique質問のcontents (cdr)をフレームのストリームと共にとる. 
;; ストリームの各フレームについて, qevalを使い, 与えられた質問を満足するフレームへのすべての拡張ストリームを見つける. 
;; そこの唯一個の項目を持つものでないストリームは除去する. 
;; 残りのストリームは戻され, unique質問の結果である, 一つの大きなストリームに蓄積する. 
;; これはnot特殊形式の実装に類似している.
;; 
;; この実装を, 唯一人を監督する人すべてをリストする質問を形成してテストせよ. 

(define (uniquely-query exps) (car exps))

(define (stream-unique? s) 
  (and (not (stream-null? s)) 
       (stream-null? (stream-cdr s))))

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result (qeval (uniquely-query operands)
                           (singleton-stream frame)))) 
        (if (stream-unique? result)
            result
            the-empty-stream)))
    frame-stream))


(query-driver-loop)
;;; Query input:
(unique (job ?x (computer wizard)))
;>
;;; Query results:
;> (unique (job (Bitdiddle Ben) (computer wizard)))
;> 
;;; Query input:
(and (supervisor ?x ?s) (unique (supervisor ?anyone ?s)))
;> 
;;; Query results:
;> (and (supervisor (Cratchet Robert) (Scrooge Eben)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
;> (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
;> 
;;; Query input:
#q


;; 監督関係
;; 
;; (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
;; (supervisor (Fect Cy D) (Bitdiddle Ben))
;; (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;; (supervisor (Reasoner Louis) (Hacker Alyssa P))
;; (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;; (supervisor (Scrooge Eben) (Warbucks Oliver))
;; (supervisor (Cratchet Robert) (Scrooge Eben))
;; (supervisor (Aull DeWitt) (Warbucks Oliver))

