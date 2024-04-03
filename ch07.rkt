#lang racket
(require malt)

; Inflate
(define lonely-i
  (lambda (p)
    (list p)))

; Deflate
(define lonely-d
  (lambda (P)
        (ref P 0)))

; Update
(define lonely-u
  (lambda (P g)
        (list (- (ref P 0) (* alpha g)))))

(define gradient-descent
  (lambda (inflate deflate update)
    (lambda (obj theta)
      (let ((f (lambda (big_theta)
                 (map update
                  big_theta
                  (gradient-of obj (map deflate big_theta))))))
        (map deflate
          (revise f revs
                  (map inflate theta)))))))

(define lonely-gradient-descent
  (gradient-descent lonely-i lonely-d lonely-u))

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
  (lambda (a-gradient-descent)
    (with-hypers
      ((revs 15000)
       (alpha 0.001)
       (batch-size 4))
    (a-gradient-descent
      (sampling-obj
        (l2-loss plane) plane-xs plane-ys)
      (list (tensor 0.0 0.0) 0.0)))))

(printf "Test lonely-gradient-descent ~a\n"
        (try-plane lonely-gradient-descent))
