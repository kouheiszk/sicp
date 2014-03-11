;; 1.2.6 素数性のテスト

;; 除数の探索

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2)) ;; 2から順に割っていきます

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1))))) ;; 1ずつ足して確かめる

(define (divides? a b)
  (= (remainder b a) 0)) ;; return b % a == 0 ? #t : #f

(define (prime? n)
  (= n (smallest-divisor n)))


(dolist (n '(2 3 4 5 6 7 8 9 10 11 12 13))
  (display n)
  (if (prime? n) (display " : PRIME"))
  (newline))



;; フェルマーのテスト

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
    ((even? exp)
     (remainder (square (expmod base (/ exp 2) m))
                m))
    (else
      (remainder (* base (expmod base (- exp 1) m))
                 m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))





