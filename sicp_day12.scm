;;; 8queenのocoalc_by_R($command_arrayやcalc_by_R($command_arraoつまで

;; data set

(define a '(-10 2.5 -11.6 17))

;; define nil

(define nil '())

;; square

(define (square x) (* x x))
(square 2)

;; map

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

;; filter

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? )

;; accumulate

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))



