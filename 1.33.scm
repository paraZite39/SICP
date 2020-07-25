((define (filtered-accumulate arg combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (if (arg a) (term a) 0) (accumulate combiner null-value term (next a) next b))))
