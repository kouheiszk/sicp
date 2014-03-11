;;; 2 データによる抽象の構築
;;;

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
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; 2.1










(define (make-rat n d)
  (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; 2.2

(define make-segment)
(define start-segment)
(define end-segment)
(define mid-segment)


;;  2.3

(define (height hw) (car hw))
(define (width hw) (cdr hw))
(define (make-rectangle sp hw) (cons sp hw))
(define (height-width rec) (cdr rec))

(define (area rec)
  (* (height(height-width rec) (width(height-width rec)))))

(define rec (make-rectangle (make-point 1 2) (make-height-width)))
(display (area rec))


;;  a  b  2^a*3^b
;;  0  0  1
;;  0  1  3
;;  1  0  2
;;  1  1  6
;;  2  0  4
;;  2  1  12
;;  2  2  36
;;
;;  素因数分解
;;








































