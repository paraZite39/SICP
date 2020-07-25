(define (rec-add y k n)
  (cond ((equal? k n) (y k))
        ((equal? k 0) (+ (y k) (rec-add y (k + 1) n)))
        ((equal? (remainder k 2) 0) (+ (* 4 (y k)) (rec-add y (k + 1) n)))
        (else (+ (* 2 (y k)) (rec-add y (k + 1) n)))))

(define (cube x) (* x x x))

(define (integral f a b n)
  (let ((h (/ (- b a) n))
        (k 0))
      (define (y x) (f (+ a (* x h))))
      (* (/ h 3) rec-add(y k n))
  ))