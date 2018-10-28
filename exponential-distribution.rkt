#lang racket
(require plot)
(require math/base)
(define (pdf-exponential γ x)
  (* γ (expt euler.0
             (- (* γ x)))))

(plot-new-window? #t)

(plot (list
       (function (lambda (x)
                   (pdf-exponential (log 2) x))
                 #:label "γ = ln2"
                 #:color 1)
       (function (lambda (x)
                   (pdf-exponential (log 3) x))
                 #:label "γ = ln3"
                 #:color 2)
       (function (lambda (x)
                   (pdf-exponential (log 4) x))
                 #:label "γ = ln4"
                 #:color 3))
      #:x-min 0
      #:x-max 10
      #:x-label "x"
      #:y-label "y"
      #:title "exponential distribution pdf with γ=ln2, ln3, ln4"
      #:width 800
      #:height 600
      #:out-file "exponential_distribution.png")