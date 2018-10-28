#lang racket
(require math/base)
(require plot)

(plot-new-window? #t)

(define h-values
  (list 0.001 0.002 0.005
        0.01 0.02 0.05
        0.1 0.2 0.5))


(define (file->data file-name)
  (map string->number
       (string-split (file->string file-name))))

; pdf of the normal distribution
(define (normal-distribution μ σ x)
  (* (/ 1 (sqrt (* 2 pi (expt σ 2))))
     (expt euler.0
           (- (/ (expt (- x μ) 2) (* 2 (expt σ 2)))))))

; the standard normal distribution
(define standard-normal
  (curry (curry normal-distribution 0) 1))

; our kernel-function
(define kernel-function
  standard-normal)

(define hw4-training-f
  (file->data "hw4-f-train.dat"))
(define hw4-training-g
  (file->data "hw4-g-train.dat"))
(define hw4-validation-f
  (file->data "hw4-f-valid.dat"))
(define hw4-validation-g
  (file->data "hw4-g-valid.dat"))

; computes the kernel density estimator
(define (kernel-density-estimator K training-set h x)
  (* (/ (* (length training-set) h))
     (apply + (map (lambda (sample) (K (/ (- x sample) h)))
                     training-set))))
; for problem f, requires h as an argument
(define hw4-estimator-f
  (curry
   (curry kernel-density-estimator kernel-function)
   hw4-training-f))
; for problem g, requires h
(define hw4-estimator-g
  (curry
     (curry kernel-density-estimator kernel-function)
     hw4-training-g))

(define (actual-f x)
  (if (and (<= 0 x) (>= 1 x))
      (* 2 x)
      0))

(define (actual-g x)
  (cond [(and (<= 0 x) (>= .5 x)) (* 4 x)]
        [(and (<= .5 x) (>= 1 x)) (* 4 (- 1 x))]
        [else 0]))

; computes the log-likelihood of the data-set
; from the distribution function pdf
(define (likelihood pdf data-set)
  (apply +
         (map (compose1 log pdf) data-set)))

; h values mapped to likelihood of data-set with that h value
; pdf must be pdf of a kernel estimator
(define (h-likelihoods kernel-estimator data-set h-values)
  (map (lambda (h)
         (vector h
               (likelihood (kernel-estimator h) data-set)))
       h-values))

; takes list of point lists
; and returns list to be used in
; an error-bar plot
(define (get-bar-data data-points)
  (map (lambda (point) (list (first point) 0 (second point)))
       data-points))

(define renderers-f
  (for/list ([h h-values]
             [i (range 0 100)])
    (function (hw4-estimator-f h) #:color i)))

(define f-training-likelihoods (h-likelihoods hw4-estimator-f hw4-training-f h-values))
(define f-validation-likelihoods (h-likelihoods hw4-estimator-f hw4-validation-f h-values))

;(plot (list (points f-training-likelihoods #:y-min -420 #:y-max 460 #:label "training" #:x-min 0 #:x-max .51)
;            (points f-validation-likelihoods #:color "red" #:label "validation"))
;      #:title "f likelihood training vs validation data" #:x-label "h" #:y-label "log-likelihood"
;      #:width 800 #:height 600 #:out-file "hw4-f-likelihood.png")

(define g-training-likelihoods (h-likelihoods hw4-estimator-g hw4-training-g h-values))
(define g-validation-likelihoods (h-likelihoods hw4-estimator-g hw4-validation-g h-values))

;(plot (list (points g-training-likelihoods #:y-min -420 #:y-max 460 #:label "training" #:x-min 0 #:x-max .51)
;            (points g-validation-likelihoods #:color "red" #:label "validation"))
;      #:title "g likelihood training vs validation data" #:x-label "h" #:y-label "log-likelihood"
;      #:width 800 #:height 600 #:out-file "hw4-g-likelihood.png")

(define f-optimal-h 0.01)
(define g-optimal-h 0.02)

(define f-maximized
  (for/list ([n (range -50 151)])
    (let ([x (exact->inexact (/ n 100))])
      (list x (hw4-estimator-f f-optimal-h x)))))

(define g-maximized
  (for/list ([n (range -50 151)])
    (let ([x (exact->inexact (/ n 100))])
      (list x (hw4-estimator-g g-optimal-h x)))))

(plot (list ;(points f-maximized)
            (function (curry hw4-estimator-f f-optimal-h) -.5 1.5 #:label "KDE")
            (function actual-f -.5 1.5 #:color "blue" #:label "actual"))
      #:title "KDE vs actual for f with h=0.01" #:x-max 1.5 #:x-min -.5 #:x-label "x" #:y-label "f_h(x)" #:out-file "kde-maximized-f.png"
      #:width 800 #:height 600)
(plot (list ;(points g-maximized)
            (function (curry hw4-estimator-g g-optimal-h) -.5 1.5 #:label "KDE")
            (function actual-g -.5 1.5 #:color "blue" #:label "actual"))
      #:title "KDE vs actual for g with h=0.02" #:x-max 1.5 #:x-min -.5 #:x-label "x" #:y-label "g_h(x)" #:out-file "kde-maximized-g.png"
      #:width 800 #:height 600)
;(plot (discrete-histogram f-validation-likelihoods #:y-min -130 #:y-max 40) #:title "f validation data likelihood")
;(plot renderers-f
;      #:x-min -.1 #:x-max 1.25)