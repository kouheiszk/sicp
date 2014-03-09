;;; 3.5.1 ストリームは遅延リスト

;;; 普通のリストをストリームリストに変換する関数を作ると楽
;;; scheme stream take という streamの先頭を持ってくるものを取得する


(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

(sum-primes 0 10)
 
