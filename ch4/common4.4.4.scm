;; For stream

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t)
     (cons h (delay t)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define (stream-null? stream) (null? stream))

(define (list->stream exp)
  (let ((first (car exp)))
    (if (null? first)
        the-empty-stream
        (cons-stream first (list->stream (cdr exp))))))

