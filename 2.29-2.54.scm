(define (make-mobile left right)
  (cons left right))

(define left-branch car)
(define right-branch cdr)

(define (make-branch length structure)
  (cons length structure))

(define branch-len car)
(define branch-str cdr)

(define (total-weight mobile)
  (cond ((not (pair? mobile)) mobile)
        ((and (pair? (left-branch mobile))
             (pair? (right-branch mobile)))
         (+ (total-weight (left-branch mobile))
           (total-weight (right-branch mobile))))
        (else (total-weight (branch-str mobile)))))

(define (balanced? mobile)
  (cond ((not (pair? mobile)) #t)
        ((and (pair? (left-branch mobile))
              (pair? (right-branch mobile)))
         (and (= (* (branch-len (left-branch mobile)) 
                    (total-weight (left-branch mobile)))
                 (* (branch-len (right-branch mobile))
                    (total-weight (right-branch mobile))))
              (and (balanced? (left-branch mobile))
                   (balanced? (right-branch mobile)))))
        (else (balanced? (branch-str mobile)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

;2.33

(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

;2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (t) (cond ((null? t) 0)
                                ((list? t) (count-leaves t))
                                (else 1))) t)))

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;2.54
(define (equal? l1 l2)
  (cond ((or (null? l1) (null? l2)) #t)
        (or (not (list? l1)) 
            (not (list? l2))) (eq? l1 l2))
        (else (and (eq? (car l1) (car l2))
                   (equal? (cdr l1) (cdr l2)))))
