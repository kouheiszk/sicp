;; 式

;; 整数は基本的な式

486

(+ 137 349)

(- 1000 334)

(* 5 9)

(/ 10 5)

(+ 2.7 10)

;; 前置記法は任意個の引数をとることができる

(+ 21 35 12 7)

(* 25 4 12)

;; 入れ子にもできる(原則、入れ子の深さや式の長さには制限はない)

(+ (* 3 5) (- 10 6))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

;; 見やすいように清書系で書く

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

;; 値はdefineで変数に入れられる
;; 代入ではなく、定義だそう
;; この違いは、後々の章でやるそうです
;; Haskellとか、再代入できないど、再定義はできるとかうんたらかんたら...

(define size 2)
size ;; 2が出力される

;; 円の半径を求める

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))

;; 円周を求める

(define circumference (* 2 pi radius))
circumference

;; 手続き定義

(define (square x) (* x x))

(square 21) ;; 21 * 21

(square (+ 2 5))

(square (square 3))

;; 作成した手続きを別の手続きの組立に利用することもできる

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4) ;; 3 * 3 + 4 + 4

;; さらに

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5) ;; (5 + 1) * (5 + 1) + (5 * 2) * (5 * 2) 

;; 解釈系はどう評価するか
;; 手続きの仮パラメータを引数に置き換えていく
;; 下は正規順序の例で、作用的順序は、引数を作用させてから仮パラメタに値を入れる
;; 間違った説明をしてしまってすいません><

(f 5)

(sum-of-squares (+ 5 1) (* 5 2))

(+ (square (+ 5 1) (* 5 2)) (square (* 5 2)))

(+ (* (+ 5 1) (+ 5 1))      (* ( * 5 2) (* 5 2)))

(+ (* 6       6)            (* 10       10))

(+ 36                       100)

136

;; 条件式
;; condで場合分けを記述できる

(define (abs x)
  (cond 
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))
  ))

(abs -20)
(abs 20)

;; condの節の"それ以外"はelseで表すことができる

(define (abs x)
  (cond 
    ((< x 0) (- x))
    (else x)
  ))

;; また、場合分けが2つの場合は、ifを用いることができる

(define (abs x)
  (if (< x 0)
    (- x)
    x))

;; 論理合成演算もある

;; 5 < x < 10

(and (> x 5) (< x 10))
 
;; x < 5 or 10 < x

(or (< x 5) (> x 10))

;; x != 5

(not (= x 5))

;; ある数がもう一つの数より大きいか、等しいかを調べる手続きが定義できる（以上の定義）

(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))


;;; #1.1

10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b))) ;; b > a and b < a * b
  b
  a)

(cond 
  ((= a 4) 6)
  ((= b 4) (+ 6 7 a))
  (else 25))

(+ 2 (if (> b a) b a))

(* (cond 
     ((> a b) a)
     ((< a b) b)
     (else -1)
  ))


;;; #1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))


;;; #1.3

(define (square x) (* x x))
(define (sum-of-max-squares x y z)
  (+ (square (cond
               ((>= x y) x)
               (else y))) 
     (square (cond
               ((and (>= x y) (>= y z)) y)
               ((and (>= x y) (< y z)) z)
               ((and (< x y) (>= x z)) x)
               ((and (< x y) (< x z)) z)))
     ))

(sum-of-max-squares 3 4 5) ;; 41
(sum-of-max-squares 3 5 4)
(sum-of-max-squares 4 3 5)
(sum-of-max-squares 4 5 3)
(sum-of-max-squares 5 3 4)
(sum-of-max-squares 5 4 3)
(sum-of-max-squares 3 3 3) ;; 18
(sum-of-max-squares 5 3 3) ;; 34
(sum-of-max-squares 3 5 3)
(sum-of-max-squares 3 3 5)
(sum-of-max-squares 5 5 3) ;; 50
(sum-of-max-squares 5 3 5)
(sum-of-max-squares 3 5 5)

;;; #1.3 別解

(define (square x) (* x x))
(define (sum-of-max-squares x y z)
  (cond
    ((and (>= x z) (>= y z)) (+ (square x) (square y)))
    ((and (>= x y) (>= z y)) (+ (square x) (square z)))
    ((and (>= y x) (>= z x)) (+ (square y) (square z)))
  ))

;;; #1.4

;; aと、bの絶対値を足す
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 3 4)
(a-plus-abs-b 3 -4)
(a-plus-abs-b -3 4)
(a-plus-abs-b -3 -4)


;;; #1.5

(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p)) ;; macがヒーターと化す素晴らしい式

;; 作用的順序だと
;; ずっとこの置き換えが続く？
(test 0 (p))
(test 0 (p))

;; 正規順序だと
;; pの値に関係なく0が帰る
(test 0 (p))
(if (= x 0) 0 y)
(if (= 0 0) 0 (p))
0


;; Newton法

;; 平方根の定義は
;; y : √x = y >= 0 and y^2 = x
;; これをLisp風に表すと

(define (sqrt x)
  (the y (and (>= y 0)
           (= (square y) x))))

;; 定義だけで、手続きが不明
;; Newton法で平方根を求める

;; 平方数
(define (square x) (* x x))

;; 絶対値
(define (abs x)
  (if (> x 0)
    x
    (- x)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))


;;; #1.6

(define (new-if predicate then-clause else-clause)
  (cond 
    (predicate then-clause)
    (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

;; 喜んで書きなおす

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;; 喜びもつかの間、ヒーター化する
;; condは作用的順序だと、評価されず式が再帰的に深くなっていく？
;; ifは帰結部と代替部が単一の式でなくてはいけないことから、
;; 作用的順序において式部分が評価されるため、再帰を抜けられる？
;; 何言ってるかよくわからない(´・ω・｀)

;; condは式を評価してしまうからダメという認識に落ち着いた...


;;; #1.7

;;; long 9,223,372,036,854,775,807
(sqrt 0.0000000000000000000000000000000000000009)
(sqrt 90000000000000000000000000000000000000000)
(sqrt 1e46)

;; もともとのgood-enough?

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; 書き換えたgood-enough?を用いたもの

;; 平方数
(define (square x) (* x x))

;; 絶対値
(define (abs x)
  (if (> x 0)
    x
    (- x)))

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
    guess
    (sqrt-iter (improve guess x)
               guess
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; (define (good-enough? guess prev-guess)
;;   (< (abs (- prev-guess guess)) (* guess 0.0001)))

(define (good-enough? guess prev-guess)
  (< (abs (- prev-guess guess)) (* guess 0.001))) 

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

(sqrt 0.0000000000000000000000000000000000000009)
(sqrt 90000000000000000000000000000000000000000)
(sqrt 1e46)



;;; #1.8

;; 平方数
(define (square x) (* x x))

;; 立方数
(define (cube x) (* x x x))

;; 絶対値
(define (abs x)
  (if (> x 0)
    x
    (- x)))

(define (curt-iter guess x)
  (if (good-enough-curt? guess x)
    guess
    (curt-iter (improve guess x)
               x)))

(define (improve guess x)
  (sum-and-three-division (* 2 guess) (/ x (square guess))))

(define (sum-and-three-division x y)
  (/ (+ x y) 3))

(define (good-enough-curt? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (curt x)
  (curt-iter 1.0 x))

(curt 27)
(curt (cube 13))
(curt 1000)


