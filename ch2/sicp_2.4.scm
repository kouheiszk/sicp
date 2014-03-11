;;; 2.4抽象データの多重表現

;; 準備

(define (square x) (* x x))


;;; 2.4.1複素数の表現

;; 複素数の加減乗除
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

;; Benの直行座標表現

(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) 
  (cons (* r (cos a)) (* r (sin a))))

(define z-ri-ben (make-from-real-imag 1 -1))
(real-part z-ri-ben)
;> 1
(imag-part z-ri-ben)
;> -1
(magnitude z-ri-ben)
;> 1.414
;> √2
(angle z-ri-ben)
;> -0.7853
;> -π/4

(define z-ma-ben (make-from-mag-ang 1.414 -0.7853))
(real-part z-ma-ben)
;> 1
(imag-part z-ma-ben)
;> -1
(magnitude z-ma-ben)
;> 1.414
;> √2
(angle z-ma-ben)
;> -0.7853
;> -π/4

;; Alyssaの極座標表現

(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y) 
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

(define z-ri-alyssa (make-from-real-imag 1 -1))
(real-part z-ri-alyssa)
;> 1.000
(imag-part z-ri-alyssa)
;> -1.0
(magnitude z-ri-alyssa)
;> 1.414
;> √2
(angle z-ri-alyssa)
;> -0.7853
;> -π/4

(define z-ma-alyssa (make-from-mag-ang 1.414 -0.7853))
(real-part z-ma-alyssa)
;> 0.9999
(imag-part z-ma-alyssa)
;> -0.9997
(magnitude z-ma-alyssa)
;> 1.414
;> √2
(angle z-ma-alyssa)
;> -0.7853
;> -π/4


;; データ抽象のやり方では、
;; add-complex、sub-complex、mul-complex、div-complex
;; の同じ実装がBenの表現でもAlyssaの表現でも働く

(add-complex z-ri-ben z-ri-ben)
;> (2.0 . -1.0)
(add-complex z-ri-ben z-ma-ben)
;> (2.000 . -0.9999)
(add-complex z-ri-ben z-ri-alyssa)
;> (2.401 . -0.8742)
(add-complex z-ri-ben z-ma-alyssa)
;> (2.401 . -0.8742)

(sub-complex z-ri-ben z-ma-alyssa)
;> (0.4863 . 2.810)

(mul-complex z-ri-ben z-ma-alyssa)
;> (1.414 . -1.7853)

(div-complex z-ri-ben z-ma-alyssa)
;> (0.7072 . -0.2147)

;;; 2.4.2タグつきデータ



;;; 2.4.3データ主導プログラミングと加法性 



;;; 問題2.73

;; d
;; 演算子と変数を入れ替える




;;; 問題2.74




;;; 問題2.76




















