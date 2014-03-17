;; ex4.18

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; letを使い掃き出すと以下のようになる

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

;; letはlambdaに変換されるから、こうなる

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    ((lambda (a b)
       (set! y a)
       (set dy b))
     (integral (delay dy) y0 dt)
     (stream-map f y))
    y))

;; ここでlambdaの2つ目の引数がyを使っているが、
;; これはまだset!されていないのでerrorになる

