;;; 3.3.4ディジタル回路のシミュレータ

;;; 問題3.2.8

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;;; 問題3.2.9

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((not-a1 (make-wire))
          (not-a2 (make-wire))
          (c (make-wire)))
      (inverter a1 not-a1)
      (inverter a2 not-a2)
      (and-gate not-a1 not-a2 c)
      (inverter c output))
    )
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; 遅延はand-gate + 2 * inverter


