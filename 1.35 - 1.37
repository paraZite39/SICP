(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

; 1.35
(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; 1.36
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define solution-x-squared-1000 (fixed-point-print (lambda (x) (/ (log 1000) (log x))) 5.0))

; 1.37
(define (frac n d next)
  (/ n (+ d next)))

(define (cont-frac n d k)
  (define (rec-frac i)
    (if (> i k)
      (frac (n k) (d k) 0)
      (frac (n i) (d i) (rec-frac (+ i 1)))
    )
  )
  (rec-frac 1)
)

(define golden-ratio-cont (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))
