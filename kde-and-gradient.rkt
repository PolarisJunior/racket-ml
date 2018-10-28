#lang racket
(require math/base)
(require plot)
; used for testing to verify formulas
(require math/distributions)

(plot-new-window? #t)

(define (file->data file-name)
  (map string->number
       (string-split (file->string file-name))))

(define ugrad-data (file->data "ugrad.dat"))
(define coke-data (file->data "coke.dat"))
(define unknown-data (file->data "unknown.dat"))


(define ugrad-mean
  (/ (apply + ugrad-data)
     (length ugrad-data)))

(define ugrad-stddev
  (/ (apply +
            (map (lambda (x_i) (expt (- x_i ugrad-mean) 2))
                 ugrad-data))
     (length ugrad-data)))

; point-free style for fun 
(define ugrad-stddev2
  (/ (apply +
            (map (compose (curryr expt 2) (curryr - ugrad-mean))
                 ugrad-data))
     (length ugrad-data)))

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

(define (kernel-density-estimator K data-set h x)
  (/ (apply + (map (lambda (data-point) (K (/ (- x data-point) h))) data-set))
     (* (length data-set) h)))

; computes the log-likelihood of the data-set
; from the distribution function pdf
(define (likelihood pdf data-set)
  (apply +
         (map (compose1 log pdf) data-set)))



#|                GRADIENT ASCENT              |#

(define (logistic-pdf a b x)
  (/ (* a (expt euler.0 (+ (* (- a) x) (- b))))
     (expt (+ 1 (expt euler.0 (+ (* (- a) x) (- b)))) 2)))

(define boiler-data (file->data "boiler.dat"))
(define boiler-data-length (length boiler-data))

(define (gradient-numerator a b x_i)
  (- 1 (expt euler.0 (- (* (- a) x_i) b))))

(define (gradient-denominator a b x_i)
  (+ 1 (expt euler.0 (- (* (- a) x_i) b))))

(define (dl/da a b)
  (let ([n (length boiler-data)])
    (- (/ n a)
       (apply +
              (map (lambda (data-point)
                     (* data-point (/ (gradient-numerator a b data-point)
                                      (gradient-denominator a b data-point))))
                   boiler-data)))))

(define (dl/db a b)
  (- (apply +
            (map (lambda (data-point)
                   (/ (gradient-numerator a b data-point)
                      (gradient-denominator a b data-point)))
                 boiler-data))))

;(define (gradient-ascent iter a b)
;  (gradient-ascent-helper iter a b empty))

;(define (gradient-ascent-helper iter a b acc)
;  (let* ([new-a (+ a (* step-size (dl/da boiler-data a b)))]
;         [new-b (+ b (* step-size (dl/db boiler-data a b)))])
;        (cond [(>= 0 iter) empty]
;              [else (cons (list a b) (gradient-ascent (sub1 iter) new-a new-b))])))

(define (gradient-function a b)
  (list (dl/da a b) (dl/db a b)))

; for testing
(define (gradient-function2 x)
  (define (df x)
  (- (* 4 (expt x 3)) (* 9 (expt x 2))))
  (list (df x)))

(define (pythagorean lst1 lst2)
  (sqrt (apply +
               (map (compose (curryr expt 2) -) lst1 lst2))))
(define gradient-likelihood-points (make-vector 150000))
(define (gradient-ascent gradient-function)
  ; a > 0
  (define a-initial 1)
  (define b-initial -1)
  (define step-size 0.0000001)
  (define max-iterations 150000)
  (define tolerance 0.00000001)
  (let loop ([previous-step-size (/ 1 tolerance)]
             [cur-vals (list a-initial b-initial)] 
             [n 0])
    (if (and (> previous-step-size tolerance)
             (< n max-iterations))
        (let ([new-vals (map + cur-vals
                             (map (curry * step-size) (apply gradient-function cur-vals)))])
          (vector-set! gradient-likelihood-points n (list n (likelihood
                                                          (apply (curry logistic-pdf) cur-vals)
                                                          unknown-data)))
          (loop (pythagorean new-vals cur-vals) new-vals (add1 n)))
        (list cur-vals n))))
(gradient-ascent gradient-function)
(plot (points gradient-likelihood-points) #:out-file "gradient-ascent-likelihood.png"
      #:title "Likelihood by iteration" #:x-label "n" #:y-label "log-likelihood" #:width 800 #:height 600)

; for testing
(define (gradient-descent gradient-function)
  ; a > 0
  (define x-initial 10)
  (define step-size 0.00001)
  (define max-iterations 150000)
  (define tolerance 0.0000001)
  (let loop ([previous-step-size (/ 1 tolerance)]
             [cur-vals (list x-initial)] 
             [n 0])
    (if (and (> previous-step-size tolerance)
             (< n max-iterations))
        (let ([new-vals (map - cur-vals
                             (map (curry * step-size) (apply gradient-function cur-vals)))])
          (loop (pythagorean new-vals cur-vals) new-vals (add1 n)))
        (list cur-vals n))))


(displayln "ugrad estimate")
(likelihood (curry normal-distribution 10.0184 (sqrt 1.02786)) unknown-data)

(displayln "boiler estimate")
(likelihood (curry logistic-pdf 0.2851403405025848 -3.4961555608346506) unknown-data)

(displayln "coke estimate")
(likelihood (curry kernel-density-estimator standard-normal coke-data .5)
            unknown-data)


; plot we used
(plot (list (function (curry normal-distribution 10.0184 (sqrt 1.02786)) -1 21 #:label "ugrad" #:color "blue")
            (function (curry logistic-pdf 0.2851403405025848 -3.4961555608346506) -1 21 #:label "boiler" #:color "red" )
            (function (curry kernel-density-estimator standard-normal coke-data .5) -1 21 #:label "coke" #:color "green")
            (points (map (curryr vector 0) unknown-data) #:label "unknown"))
      #:x-label "x" #:y-label "f(x)" #:out-file "densities-by-model.png" #:title "Densities of different models"
      #:width 800 #:height 600)

;(define a-b-values (gradient-ascent max-iterations a-initial b-initial))

;(plot (points a-b-values)
;      #:x-label "a"
;      #:y-label "b")

;(plot (points (map (lambda (a-b) (list (dl/da boiler-data (first a-b) (second a-b))
;                                       (dl/db boiler-data (first a-b) (second a-b))))
;                   a-b-values))
;      #:x-label "dl/da"
;      #:y-label "dl/db")



