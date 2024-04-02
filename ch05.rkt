#lang racket
(require malt)

; Examples of using malt to define and use hypers
(declare-hyper smaller)

(define nonsense?
  (lambda (x)
    (= (sub1 x) smaller)))

(printf "Non-sense? ~a\n"
        (with-hypers
            ((smaller 5))
          (nonsense? 6)))

(declare-hyper revs)
(declare-hyper alpha)

(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big_theta)
               (map (lambda (p g)
                      (- p (* alpha g)))
                    big_theta
                    (gradient-of obj big_theta)))))
      (revise f revs theta))))

(define quad-xs (tensor -1.0 0.0 1.0 2.0 3.0))
(define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))

(define quad
  (lambda (t)
    (lambda (theta)
      (+ (* (ref theta 0) (sqr t))
         (+ (* (ref theta 1) t) (ref theta 2))))))

(printf "Test quad: ~a\n"
        ((quad 3.0) (list 4.5 2.1 7.8)))

(printf "Test gradient descent: ~a\n"
        (with-hypers
            ((revs 1000)
             (alpha 0.001))
          (gradient-descent
           ((l2-loss quad) quad-xs quad-ys)
           (list 0.0 0.0 0.0))))

(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.9)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

(define plane
  (lambda (t)
    (lambda (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

(printf "Test plane: ~a\n"
        (with-hypers
          ((revs 1000)
           (alpha 0.001))
          (gradient-descent
            ((l2-loss plane) plane-xs plane-ys)
            (list (tensor 0.0 0.0) 0.0))))