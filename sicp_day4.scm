;; 1.3

;; 3乗は次のように表される

(define (cube x) (* x x x))

;; しかし、(* 3 3 3) と書いても実現できる。
;; 引数が数値に制限されると、抽象の能力は激しく狭められてしまう。手続きを引数に取れ、手続きを値として返す、手続きを扱う手続きを高階手続きという。


;; 1.3.1

;; a から b までの整数の和を計算

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

;; a の 3乗から b の3乗までの和を計算する

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

;; 級数の項の並びの和を計算する

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; これらには共通の型があり、それは抽象化が可能であることを表している。
;; 級数の総和という抽象から総和記号を発明した。

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

;; termは関数fを、nextはaを変化させる関数を表す

(define (inc n) (+ n 1)) ;; next
(define (sum-cubes a b)
  (sum cube a inc b)) ;; term

;; 1 から 10 までの整数の3乗の和

(sum-cubes 1 10)


;; シンプソンの公式で、2の倍数だと上手くいく理由は、2こづつ計算しているから。
;;
