;;; 3.1.1

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 25)
;> 75
(withdraw 25)
;> 50
(withdraw 60)
;> "Insufficient funds"
(withdraw 15)
;> 35


;; balanceが大域変数であり、何処からでもアクセスできてしまう
;; ので、修正して、withdrawだけがアクセスできるようにする

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(new-withdraw 25)
;> 75
(new-withdraw 25)
;> 50
(new-withdraw 60)
;> "Insufficient funds"
(new-withdraw 15)
;> 35


;; 払い出し器を作る

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
;> 50
(W2 70)
;> 30
(W2 40)
;> "Insufficient funds"
(W1 40)
;> 10

;; W1とW2はそれぞれ自分の局所状態変数balanceを持つ
;; 完全に独立なオブジェクトである
;; 次は預入れをオブジェクトを含む、銀行口座オブジェクトを返す手続き

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))

((acc 'withdraw) 50)
;> 50
((acc 'withdraw) 50)
;> 0
((acc 'withdraw) 60)
"Insufficient funds"
((acc 'deposit) 40)
;> 90
((acc 'withdraw) 60)
;> 30



