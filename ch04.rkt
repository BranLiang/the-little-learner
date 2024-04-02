#lang racket
(require malt)

(printf "The gradient of (lambda (theta) (sqr (ref theta 0))) at (27.0) is: ~a\n"
        (gradient-of (lambda (theta) (sqr (ref theta 0))) (list 27.0)))

(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

(printf "The gradient of (l2-loss line) at (0.0 0.0) is: ~a\n"
        (gradient-of ((l2-loss line) line-xs line-ys ) (list 0.0 0.0)))

(define revise
  (lambda (f revs theta)
    (cond
      ((zero? revs) theta)
      (else
       (revise f (sub1 revs) (f theta))))))

(printf "The revised theta is: ~a\n"
        (revise (lambda (theta)
                  (map (lambda (p)
                         (- p 3))
                       theta)) 5 (list 1 2 3)))

(define alpha 0.01)
(define revs 1000)

(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big_theta)
               (map (lambda (p g)
                      (- p (* alpha g)))
                    big_theta
                    (gradient-of obj big_theta)))))
      (revise f revs theta))))

(printf "The result of gradient descent is: ~a\n"
        (gradient-descent ((l2-loss line) line-xs line-ys ) (list 0.0 0.0)))
