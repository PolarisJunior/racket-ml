#lang racket

(require math/special-functions)
(require plot)

(plot-new-window? #t)

(define (beta-distribution α β x)
  (let ([normalizer (/ (* (gamma α) (gamma β))
                       (gamma (+ α β)))])
    (/ (* (expt x (sub1 α)) (expt (- 1 x) (sub1 β)))
     normalizer)))

(define (complement θ)
  (- 1 θ))
(define (beta-distribution-complement α β x)
  (beta-distribution α β (complement x)))

; dirichlet-distribution of dimension 2
(define (dirichlet-distribution α_1 α_2 x_1 x_2)
  (let ([normalizer (/ (* (gamma α_1) (gamma α_2))
                       (gamma (+ α_1 α_2)))])
    (/ (* (expt x_1 (sub1 α_1)) (expt x_2 (sub1 α_2)))
     normalizer)))

(define θ-ML (/ 1 5))

(plot (list (function (curry beta-distribution-complement .9 .1) 0.0001 .9999 #:label "prior (.9, .1)")
            (function (curry beta-distribution-complement 4.9 1.1) 0.0001 .9999 #:color "blue" #:label "posterior (4.9, 1.1)")
            (points (list (list θ-ML 0)) #:label "θ ML" #:size 10))
      #:title "Prior and Posterior θ distribution"
      #:width 800 #:height 600 #:x-label "θ" #:y-label "probability mass"
      #:out-file "priorvsposterior1.png")

(plot (list (function (curry beta-distribution-complement 2 3) 0.0001 .9999 #:label "prior (2, 3)")
            (function (curry beta-distribution-complement 6 4) 0.0001 .9999 #:color "blue" #:label "posterior (6, 4)")
            (points (list (list θ-ML 0)) #:label "θ ML" #:size 10))
      #:title "Prior and Posterior θ distribution"
      #:width 800 #:height 600 #:x-label "θ" #:y-label "probability mass"
      #:out-file "priorvsposterior2.png")

(plot (list (function (curry beta-distribution-complement 20 20) 0.0001 .9999 #:label "prior (20, 20)")
            (function (curry beta-distribution-complement 24 21) 0.0001 .9999 #:color "blue" #:label "posterior (24, 21)")
            (points (list (list θ-ML 0)) #:label "θ ML" #:size 10))
      #:title "Prior and Posterior θ distribution"
      #:width 800 #:height 600 #:x-label "θ" #:y-label "probability mass"
      #:out-file "priorvsposterior3.png")

(plot (list (function (curry beta-distribution-complement 1 1) 0.0001 .9999 #:label "prior (1, 1)")
            (function (curry beta-distribution-complement 5 2) 0.0001 .9999 #:color "blue" #:label "posterior (5, 2)")
            (points (list (list θ-ML 0)) #:label "θ ML" #:size 10))
      #:title "Prior and Posterior θ distribution"
      #:width 800 #:height 600 #:x-label "θ" #:y-label "probability mass"
      #:out-file "priorvsposterior4.png")

