#lang racket
(require malt)

(define samples
  (lambda (n s)
    (sampled n s (list))))

(define sampled
  (lambda (n i a)
    (cond
      ((zero? i) a)
      (else
        (sampled n (sub1 i)
          (cons (random n) a))))))

(printf "Test samples: ~a\n" (samples 10 5))

(declare-hyper batch-size)

(define sampling-obj
  (lambda (expectant xs ys)
    (let ((n (tlen xs)))
      (lambda (theta)
        (let ((b (samples n batch-size)))
          ((expectant (trefs xs b) (trefs ys b)) theta))))))

(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big_theta)
               (map (lambda (p g)
                      (- p (* alpha g)))
                    big_theta
                    (gradient-of obj big_theta)))))
      (revise f revs theta))))

(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

(printf "Test sampling-obj: ~a\n"
  (with-hypers
    ((revs 1000)
     (alpha 0.01)
     (batch-size 4))
    (gradient-descent
      (sampling-obj
        (l2-loss line) line-xs line-ys)
      (list 0.0 0.0))))

(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.9)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

(printf "Test sampling-obj for plane: ~a\n"
  (with-hypers
    ((revs 15000)
     (alpha 0.001)
     (batch-size 4))
    (gradient-descent
      (sampling-obj
        (l2-loss plane) plane-xs plane-ys)
      (list (tensor 0.0 0.0) 0.0))))