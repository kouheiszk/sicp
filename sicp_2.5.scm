;;; 2.5汎用演算のシステム

;; 複数の方法で表現されているシステムの設計方法で重要なことは、
;; データ演算を指定するプログラムを、汎用インターフェース手続きを使って
;; 複数の表現へ接続することである。
;;
;; この節では、異なる表現に対して汎用な演算を定義するだけでなく、
;; 異なる種類の引数に対する汎用な演算を定義する方法を学ぶ
;;
;; add、sub、mul、div をインターフェースとして
;; 有理数演算や複素数演算（直行座標、極座標）、通常演算にアクセスできるようにする

;;; 準備

;; true and false

(define true #t)
(define false #f)

;; 局所表 @see 3.3.3

(define (make-table) (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; attach-tag

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; apply-generic

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "no method for these types -- apply-generic"
            (list op type-tags))))))

;; install-rectangular-package 

(define (install-rectangular-package)
  ;; internal procedure
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

;; install-polar-package

(define (install-polar-package)
  ;; internal procedure
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)

;; square

(define (square x) (* x x ))


;;; 2.5.1汎用算術演算

;; それぞれの種類の数に、型を表すタグを付け
;; 型に応じたパッケージへ振り分ける

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

;; 通常の数を扱うパッケージ

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x)) 
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(install-scheme-number-package)
(define x (make-scheme-number 12))
(define y (make-scheme-number 4))

(add x y)
;> (scheme-number . 16)
(sub x y)
;> (scheme-number . 8)
(mul x y)
;> (scheme-number . 48)
(div x y)
;> (scheme-number . 3)


;; 有理数を扱うパッケージ

(define (install-rational-package)
  ;; internal procedure
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ?-rat x y)
    )
  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) 
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put '=zero? '(rational) 
       (lambda (x) (= 0 (numer x))))
  (put 'raise '(rational)
       (lambda (x) (make-complex-from-real-imag
                     (/ (numer x) (denom x)) 0)))
  (put 'project '(rational)
       (lambda (x)
         (make-scheme-number
           (round (/ (numer x) (denom x))))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(install-rational-package)
(define x (make-rational 7 12))
(define y (make-rational 6 7))

(add x y)
;> (rational 121 . 84)
(sub x y)
;> (rationa -23 . 84)
(mul x y)
;> (rational 1 . 2)
(div x y)
;> (rational 49 . 72)


;; 複素数を扱うパッケージ

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedure
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (and (= (real-part z1) (real-part z2))
              (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex)
       (lambda (z)
         (and (= 0 (real-part z))
              (= 0 (imag-part z)))))
  (put 'project '(complex)
       (lambda (x) (make-rational (real-part x) 1)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; 汎用選択肢 @see 2.4.3

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


(install-complex-package)
(define x (make-complex-from-real-imag 1 1.732))
(define y (make-complex-from-mag-ang 2 3.142))

(add x y)
;> (complex rectangular -0.9999998340689045 . 1.731185307202117)
(sub x y)
;> (complex rectangular 2.9999998340689045 . 1.732814692797883)
(mul x y)
;> (complex polar 3.9999119990319785 . 4.189184849024927)
(div x y)
;> (complex polar 0.9999779997579946 . -2.094815150975073)


;;; 問題2.77

(define z (make-complex-from-real-imag 3 4))
(magnitude z)
;> 5

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ; type-tags => (complex)
    (let ((proc (get op type-tags))) ; (get 'magnitude '(complex))
      (if proc
          (apply proc (map contents args)) 
          ; (magnitude ('rectanglar 3 . 4))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;; apply-genericは2回呼び出される
;; (install-rectangular-package)でもmagnitudeという手続きを使っているが、
;; それは(install-rectangular-package)内で定義されたmagnitudeを使うため、
;; apply-genericは2回しか呼ばれない


;;; 問題2.78

(install-scheme-number-package)
(define x (make-scheme-number 12))
(define y (make-scheme-number 4))

(add x y)
;> (scheme-number . 16)
(add x 4)
;> ERROR

;; これをちゃんと動作するようにする
;; tableには'add '(scheme-number scheme-number)で登録されている
;; apply-genericで呼ばれるtype-tagで、通常の数は記号scheme-numberを返さない
;; 通常の数はtype-tagで記号scheme-numberを返すようにする

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number) ; numberだったら記号scheme-numberを返す
        (else (error "Bad tagged datum -- TYPE-TAG" datum))
        ))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))
        ))


;;; 問題2.79

;; 各パッケージに追加する

(install-scheme-number-package)
(define x (make-scheme-number 12))
(equ? x 12)
;> #t
(equ? x 7)
;> #f

(install-rational-package)
(define x (make-rational 7 12))
(define y (make-rational 6 7))
(define z (make-rational 6 7))
(equ? x y)
;> #f
(equ? y z)
;> #t

(install-complex-package)
(define x (make-complex-from-real-imag 1 1.732))
(define y (make-complex-from-mag-ang 2 3.142))
(define z (make-complex-from-mag-ang 2 3.142))
(define a (make-complex-from-mag-ang 1 0))
(define b (make-complex-from-real-imag 1 0))
(equ? x y)
;> #f
(equ? y z)
;> #t
(equ? a b)
;> #t
(define c (make-rational 0 2))
(define d (make-rational 0 3))
(equ? c d)
;> #t

;;; 問題2.80

;; 各パッケージに追加する

(install-scheme-number-package)
(define x (make-scheme-number 1))
(define y (make-scheme-number 0))
(=zero? x)
;> #y
(=zero? y)
;> #t

(install-rational-package)
(define x (make-rational 7 12))
(define y (make-rational 0 7))
(=zero? x)
;> #y
(=zero? y)
;> #t

(install-complex-package)
(define x (make-complex-from-real-imag 1 0))
(define y (make-complex-from-real-imag 0 1))
(define z (make-complex-from-mag-ang 0 3.142))
(=zero? x)
;> #f
(=zero? y)
;> #f
(=zero? z)
;> #t


;; equ?のパッケージがあるといいかも


;;; 2.5.2異なる方のデータの結合

;; これまではパッケージごとに独立で、演算も同じ型どうしで行った。
;; これからは、型の境界を超える演算を定義していく。
;; 定義に置いて、型の間の演算は、これまでの部品の境界を
;; 大きく破壊しないようして導入してく。

;; 方法の一つに、演算が許されているの組み合わせ毎に手続きを設計する方法がある

;; 複素数パッケージに以下の手続きを組み込む
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

;; この方法は上手くいくが、煩わしい。
;; パッケージの責任が曖昧になるためである。
;; 複素数と通常の数の演算は複素数パッケージ？
;; 複素数と有理数の演算は何パッケージ？

;;; 強制型変換

;; 例えば、通常の数は、虚部が0の複素数と見ることができる。
;; これを強制型変換といい、
;; これにより、複素数のパッケージを用いて解くことのできるた問題となる。

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;; ここまでは前のapply-genericと同じ
          ;; 適切なprocがなかった場合は型変換を行う
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  ;; coercion
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define x (make-complex-from-real-imag 2 4))
;> (complex rectangular 2 . 4)
(define y (make-scheme-number 1))
;> (scheme-number . 1)
(add x y)
;> (complex rectangular 3 . 4)

;; しかしこのままだとnこの型があるとn^2の手続きを定義しなくてはいけない
;; そこで型の間の関係のさらなる構造を使ってこれを簡略化する
;; @see figure 2.25

;;; 型の階層構造

;; 整数は有理数に含まれ、有理数は実数に含まれ、実数は複素数に含まれる。
;; この階層構造を用いて、
;; 整数から有理数、有理数から実数、実数から複素数への変換を定義して、
;; よしなにこれらを作用させるような手続きを用意すれば、
;; 整数から複素数の変換をわざわざ用意しなくてもいいということ。

;;; 階層構造の不適切さ

;; ただし、幾何学図形の様に、ネットワークの関係を持つものについては
;; 上記手法を使うことができない
;; @see figure 2.26


;;; 問題2.81

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a

(define (exp x y) (apply-generic 'exp x y))

(define x (make-scheme-number 2))
(define y (make-scheme-number 3))
(exp x y)
;> (scheme-number . 8)
(define x (make-complex-from-real-imag 1 0))
(define y (make-complex-from-real-imag 0 1))
(exp x y)
;> 処理が返ってこない

;; apply-genericの(apply proc (map contents args))の行で
;; procが見つからないために再度型変換を行う処理に入って
;; 結果無限ループに陥る？

;; b

;; aの結果から正しくない

;; c

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;; ここまでは前のapply-genericと同じ
          ;; 適切なprocがなかった場合は型変換を行う
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                ;; typeが同じなのにprocが見つからない場合はerrorにする
                (if (eq? type1 type2)
                    (error "No method for same types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      ;; coercion
                      (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                              (apply-generic op a1 (t2->t1 a2)))
                            (else
                              (error "No method for these types"
                                     (list op type-tags))))))
                (error "No method for these types"
                       (list op type-tags))))))))


;;; 問題2.82

; あとで線形反復プロセスで動くようにする
;
;(define (coercion-all args)
;  (define (try-coercion args dest)
;    (if (null? args)
;        dest
;        (let ((target-type (type-tag (car args))))
;          (try-coercion 
;            (cdr args) 
;            (map (lambda (x) 
;                   (if (rq? (type-tag x) target-type)
;                       x
;                       (let ((coercion-proc (get-coercion (type-tag x) target-type)))
;                         (if coercion-proc
;                             (coercion-proc x)
;                             x)))) dest)))))
;  (try-coercion args args))


(define (apply-generic op . args)
  ;; new-typeに変換する
  ;; 表に定義されていないものは変換せずにそのまま返す
  (define (try-coercion x new-type)
    (let ((coercion-proc (get-coercion (type-tag x) new-type)))
      (if coercion-proc
          (coercion-proc x)
          x)))
  ;; 型を変換して再度演算する
  (define (re-apply-generic op args type-tags)
    (if (null? type-tags) 
        (error "No method for these types"
               (list op (map type-tag args)))
        (let ((new-args (map (lambda (x)
                               (try-coercion x (car type-tags)))
                             args)))
          (let ((new-type-tags (map type-tag new-args)))
            (let ((proc (get op new-type-tags)))
              (if proc
                  (apply proc (map contents new-args))
                  ;; 繰り返し
                  (re-apply-generic op args (cdr type-tags))))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (re-apply-generic op args type-tags)))))

(define x (make-scheme-number 4))
(define y (make-scheme-number 2))
(define z (make-complex-from-real-imag 1 4))
(add x y)
;> (scheme-number . 6)
(add y z)
;> (complex rectangular 3 . 4)
(add z x)
;> (complex rectangular 5 . 4)

;; 問題点:
;; scheme-number->complex の変換が定義されているが、
;; 階層構造を使わないと結局n!通りの型変換の定義が必要で一般的でない
;; 例えば、scheme-number->rationalという変換とrational->realという
;; 変換が定義されていた場合でも、rational型が引数になければ演算できないので
;; 結局全部定義しなくてはいけなくなる。


;;; 問題2.83

;; 各パッケージに追加する

(install-scheme-number-package)
(define x (make-scheme-number 12))
x
;> (scheme-number . 12)
(raise x)
;> (rational 12 . 1)

(install-rational-package)
(define x (make-rational 7 12))
x
;> (rational 7 . 12)
(raise x)
;> (complex rectangular 7/12 . 0)


;;; 問題2.84

;; それぞれの型のレベルを予め定義しておく

(define (get-level x)
  (let ((type (type-tag x)))
    (cond ((eq? type 'complex) 3)
          ((eq? type 'rational) 2)
          ((eq? type 'scheme-number) 1)
           )))

(define (raise-to level x)
  (if (= level (get-level x))
      x
      (raise-to level (raise x))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((level1 (get-level a1))
                      (level2 (get-level a2)))
                  (cond ((< level1 level2)
                         (apply-generic op (raise-to level2 a1) a2))
                        ((> level1 level2)
                         (apply-generic op a1 (raise-to level1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define x (make-scheme-number 12))
(define y (make-rational 7 12))
(define z (make-complex-from-real-imag 1 4))
(add x y)
(sub y z)
(mul z x)
(div x y)


;;; 問題2.85

;; 汎用演算projectを各パッケージに追加する

(define (drop x)
  (if (eq? (type-tag x) 'scheme-number)
      x
      (let ((projected (project x)))
        (if (equ? (raise projected) x)
            (drop projected)
            x))))

(define x (make-scheme-number 3))
(drop x)
;> (scheme-number . 3)
(define y (make-rational 8 2))
(drop y)
;> (scheme-number . 4)
(define z (make-complex-from-real-imag 4 0))
(drop z)
;> (scheme-number . 4)
(drop (make-rational 3 7))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (if (or (eq? op 'add)
                  (eq? op 'sub)
                  (eq? op 'mul)
                  (eq? op 'div))
              (drop (apply proc (map contents args))) ; 演算後dropする
              (apply proc (map contents args))) ; 汎用演算以外のapply-generic
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((level1 (get-level a1))
                      (level2 (get-level a2)))
                  (cond ((< level1 level2)
                         (apply-generic op (raise-to level2 a1) a2))
                        ((> level1 level2)
                         (apply-generic op a1 (raise-to level1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define x (make-scheme-number 12))
(define y (make-rational 12 1))
(define z (make-complex-from-real-imag 4 0))
(add x y)
;> (scheme-number . 24)
(sub y z)
;> (scheme-number . 8)
(mul z x)
;> (complex polar 48 . 0.0)
(div x y)
;> (rational 1 . 1)


