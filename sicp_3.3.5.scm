;;; 準備

;; true and false

(define true #t)
(define false #f)
(define (square x) (* x x))

;;; 3.3.5制約の拡散

;; 変数a、b、c、dについて方程式 ab = cd という関係があったとする。
;; これは、一方向性ではなく、任意の3つの量の値が分かれば、残る1つを計算できる。
;; しかし、方程式を伝統的な計算機言語に翻訳しようとすると、
;; 他の3つを使って計算する量を1つ選ばなくてはいけない。
;; 
;; この節では関係そのもを使って仕事ができるような言語の設計をみてみる。
;; その基本要素は、量と量の間にはある関係が成り立つことを述べる基本制約である。
;; 例えば
;; (adder a b c) は a + b = c を
;; (multiplier x y z) は x * y = z を
;; (constant 3.14 x) は x が 3.14 でなくてはいけない
;; ということを表している。
;;
;; これらの基本制約を組み合わせ、更に複雑な関係を表す手段を提供する。
;; 制約は制約ネットワークを構成して組み合わせる。
;; 制約はコネクタで接続する。
;; コネクタは1つか複数の制約に関する値を保持するオブジェクトである。
;;
;; 例えば、カ氏の温度Fとセ氏の温度Cの間には
;; 9C = 5(F-32)
;; の関係がある。
;; これらは figure3.28 で表される。


;;; 制約システムの使い方

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)


;;; 制約システムの実装

;; コネクタの基本演算は次の通り
;;
;; (has-value? <connector>)
;; コネクタが値を持つかどうかを告げる
;;
;; (get-value <connector>)
;; コネクタの現在の値を返す
;;
;; (set-value! <connector> <new-value> <informant>)
;; 通知者はコネクタに新しい値を設定するよう要求していることを示す
;;
;; (forget-value! <connectro> <retractor>)
;; 撤回者がその値を忘れるように要求していることをコネクタに告げる
;;
;; (connect <connector> <new-constaint>)
;; コネクタに新しい制約に関るよう告げる

;; コネクタは、コネクタが値を持つことを制約に告げる手続き inform-about-value と、
;; コネクタは値を持たないと制約に告げる inform-about-no-value を使って制約と通信する。

;; adder は次のような手続きで実装される

(define (adder a1 a2 sum)
  ;; a1 + a2 = sum
  ;; sum - a1 = a2
  ;; sum - a2 = a1
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  ;; この加算器が設定したコネクタの値を失わせる
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- ADDER" request))))
  ;; コネクタと加算器をつなぐ
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;; 制約の局所手続きの振り分けに用いる構文インターフェース

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;; 同様に乗算器 multiplier は次のような手続きで実装される

(define (multiplier m1 m2 product)
  ;; product = 0 if m1 = 0
  ;; product = 0 if m2 = 0
  ;; product = m1 * m2
  ;; m2 = product / m1
  ;; m1 = product / m2
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  ;; この乗算器が設定したコネクタの値を失わせる
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  ;; コネクタと乗算器をつなぐ
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

;; constant 構成子は支持されたコネクタの値を設定するのみ

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;; プローブは支持されたコネクタの設定や設定解除についてメッセージを印字

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)


;;; コネクタの表現

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      ;; 現在コネクタが値を持っていなければ、
      ;; 1.値を設定する
      ;; 2.informant に setter をセットする
      ;; 3.setter 以外の制約に値が設定されたことを知らせる
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints)
             'ok)
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      ;; 値を忘れる要求が来た場合、setter と retractor が同じで会った場合
      ;; コネクタと関わりのある制約に値が無くなったことを知らせる
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      ;; 制約のリストに手続きが存在しなければ、そのリストに制約を追加する
      (if (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

;; for-each-except
;; procedure を exception 以外のリストの項目に作用させる

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

;; コネクタの局所手続きの振り分けに用いる構文インターフェース

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;;; セ氏とカ氏の変換を試してみる

(set-value! C 25 'user)
;> Probe: Celsius temp = 25
;> Probe: Fahrenheit temp = 77

(set-value! F 212 'user)
;> ERROR

(forget-value! C 'user)
;> Probe: Celsius temp = ?
;> Probe: Fahrenheit temp = ?

(set-value! F 212 'user)
;> Probe: Fahrenheit temp = 212
;> Probe: Celsius temp = 100


;;; 問題3.33

;; memo 図を書く

(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (multiplier c x y)
    (constant 2 x)
    (adder a b y)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)

(probe "Value A" A)
(probe "Value B" B)
(probe "Average" C)

(set-value! A 100 'user)
;> Probe: Value A = 100

(set-value! B 200 'user)
;> Probe: Value B = 200
;> Probe: Average = 150

(forget-value! A 'user)
;> Probe: Value A = ?
;> Probe: Average = ?

(set-value! C 300 'user)
;> Probe: Average = 300
;> Probe: Value A = 400


;;; 問題3.34

(define (squarer a b)
  (multiplier a a b))

(define A (make-connector))
(define B (make-connector))
(squarer A B)

(probe "Value A" A)
(probe "Value B" B)

(set-value! A 100 'user)
;> Probe: Value A = 100
;> Probe: Value B = 10000

(forget-value! A 'user)
;> Probe: Value A = ?
;> Probe: Value B = ?

(set-value! B 100 'user)
;> Probe: Value B = 100

;; 以上より、b の値を設定した際に a の値が設定されないのが問題


;;; 問題3.35

(define (squarer a b)
  ;; a * a = b
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (square (get-value a)) me))))
  (define (process-forget-value) 
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request) 
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define A (make-connector))
(define B (make-connector))
(squarer A B)

(probe "Value A" A)
(probe "Value B" B)

(set-value! A 100 'user)
;> Probe: Value A = 100
;> Probe: Value B = 10000

(forget-value! A 'user)
;> Probe: Value A = ?
;> Probe: Value B = ?

(set-value! B 100 'user)
;> Probe: Value B = 100
;> Probe: Value A = 10


;;; 問題3.36

;; 図を書く


;;; 問題3.37

;; 9 * C = 5 * (F - 32)
;; (9 / 5) * C = F - 32
;; F = (9 / 5) * C + 32
;; としたい

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))


(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
;> Probe: Celsius temp = 25
;> Probe: Fahrenheit temp = 77

(set-value! F 212 'user)
;> ERROR

(forget-value! C 'user)
;> Probe: Celsius temp = ?
;> Probe: Fahrenheit temp = ?

(set-value! F 212 'user)
;> Probe: Fahrenheit temp = 212
;> Probe: Celsius temp = 100

;; ファーレンハイト

