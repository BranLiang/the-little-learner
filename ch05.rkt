#lang racket
(require malt)

(declare-hyper smaller)

(define nonsense?
  (lambda (x)
    (= (sub1 x) smaller)))

(printf "Non-sense? ~a\n"
        (with-hypers
            ((smaller 5))
          (nonsense? 6)))