#lang racket
(require malt)

(declare-hyper mu)

(define velocity-i
    (lambda (p)
        (list p (zeroes p))))

(define velocity-d
    (lambda (P)
        (ref P 0)))

(define velocity-u
    (lambda (P g)
        (let ((v (- (* mu (ref P 1)) (* alpha g))))
            (list (+ (ref P 0) v) v))))

(define velocity-gradient-descent
    (gradient-descent velocity-i velocity-d velocity-u))

(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.9)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

(define try-plane
  (lambda (a-gradient-descent a-revs)
    (with-hypers
      ((revs a-revs)
       (alpha 0.001)
       (batch-size 4))
    (a-gradient-descent
      (sampling-obj
        (l2-loss plane) plane-xs plane-ys)
      (list (tensor 0.0 0.0) 0.0)))))

(printf "Test velocity-gradient-descent ~a\n"
    (with-hypers
        ((mu 0.9))
        (try-plane velocity-gradient-descent 5000)))