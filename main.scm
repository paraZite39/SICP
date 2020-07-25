(define (even? k) (equal? (modulo k 2) 0))
(define (cube x) (* x x x))

(define (rec-add y k n)
  (cond ((equal? k n) (y k))
        ((equal? k 0) (+ (y k) (rec-add y (+ k 1) n)))
        ((even? k) (+ (* 4 (y k)) (rec-add y (+ k 1) n)))
        (else (+ (* 2 (y k)) (rec-add y (+ k 1) n)))))

(define (integral f a b n)
  (let ((h (/ (- b a) n)))
      (define (y x) (f (+ a (* x h))))
      (* (/ h 3) (rec-add y 0 n))
  ))

(integral cube 0 1 1000)
