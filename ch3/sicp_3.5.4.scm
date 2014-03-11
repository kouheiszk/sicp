;;; 3.5.4 ストリームと遅延評価

(load "./sicp_3.5_common.scm")
(load "./sicp_3.5.2.scm")
(load "./sicp_3.5.3.scm")

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

;; これは返ってこない

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

;; 




