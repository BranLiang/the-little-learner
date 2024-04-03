#lang racket
(require malt)

(declare-hyper beta)

(define smooth
    (lambda (decay-rate average g)
        (+ (* decay-rate average)
           (* (- 1.0 decay-rate) g))))

(define rms-u
    (lambda (P g)
        (let ((r (smooth beta (ref P 1) (sqr g))))
            (let ((alpha-hat (/ alpha (+ (sqrt r) epsilon))))
                (list (- (ref P 0) (* alpha-hat g)) r)))))

(define rms-i
    (lambda (p)
        (list p (zeroes p))))

(define rms-d
    (lambda (P)
        (ref P 0)))

(define rms-gradient-descent
    (gradient-descent
        rms-i rms-d rms-u))

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
  (lambda (a-gradient-descent a-revs an-alpha)
    (with-hypers
      ((revs a-revs)
       (alpha an-alpha)
       (batch-size 4))
    (a-gradient-descent
      (sampling-obj
        (l2-loss plane) plane-xs plane-ys)
      (list (tensor 0.0 0.0) 0.0)))))

(printf "Test rms-gradient-descent ~a\n"
    (with-hypers
        ((beta 0.9))
        (try-plane rms-gradient-descent 3000 0.01)))

(define adam-u
    (lambda (P g)
        (let ((r (smooth beta (ref P 2) (sqr g))))
            (let ((alpha-hat (/ alpha (+ (sqrt r) epsilon)))
                  (v (smooth mu (ref P 1) g)))
                (list (- (ref P 0) (* alpha-hat v)) v r)))))

(define adam-i
    (lambda (p)
        (let ((v (zeroes p)))
            (let ((r v))
                (list p v r)))))

(define adam-d
    (lambda (P)
        (ref P 0)))

(define adam-gradient-descent
    (gradient-descent
        adam-i adam-d adam-u))

(printf "Test adam-gradient-descent ~a\n"
(with-hypers
    ((mu 0.85)
     (beta 0.9))
        (try-plane adam-gradient-descent 1500 0.01)))
