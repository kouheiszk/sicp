;; For stream

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t)
     (cons h (delay t)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

