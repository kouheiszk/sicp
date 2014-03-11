;;;  

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t)
     (cons h (delay t)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))


;;; 問題3.70

(define (weighted-pairs s t weight)
  (define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
              (cond ((<= (weight s1car) (weight s2car))
                     (cons-stream s1car (merge (stream-cdr s1) s2)))
                    (else 
                      (cons-stream s2car (merge s1 (stream-cdr s2)))))))))
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge 
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight))))

;; a
(define (weight-a pair)
  (+ (car pair) (cadr pair)))
(define sa (weighted-pairs integers integers weight-a))

;; b
(define (weight-b pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair)) 
     (* 5 (car pair) (cadr pair))))
(define (divisible? x y) (= (remainder x y) 0))
(define i-235 
  (stream-filter (lambda (x)
                   (and (not (divisible? x 2))
                        (not (divisible? x 3))
                        (not (divisible? x 5))))
                 integers))
(define sb (weighted-pairs i-235 i-235 weight-b))



;;; 問題3.71

(define (ram-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i i) (* j j j))))
(define the-pairs
  (weighted-pairs integers integers ram-weight))
(define (search-Ram S-pairs)
  (let* ((w1 (ram-weight (stream-car S-pairs)))
         (rest-of-S-pairs (stream-cdr S-pairs))
         (w2 (ram-weight (stream-car rest-of-S-pairs))))
    (if (= w1 w2)
        (cons-stream w1 (search-Ram (stream-cdr rest-of-S-pairs)))
        (search-Ram rest-of-S-pairs))))
(define S-Ram (search-Ram the-pairs))


