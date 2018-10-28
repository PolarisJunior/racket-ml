#lang racket

(define mlk-text (file->string "mlk-letter-estimation.txt"))
(define test-text (file->string "test-letter-estimation.txt"))
(define test-text-large (file->string "test-letter-estimation-large.txt"))

; list of letters from start-end (both inclusive)
(define (letters-in-range start end)
  (map integer->char
       (range (char->integer start)
              (add1 (char->integer end)))))


; returns list of alphabetic only characters in str,
; lower cased
(define (preprocess str)
  (filter char-alphabetic?
          (string->list (string-downcase str))))

; returns a mapping from letters letters in str to their
; occurrence in str
(define (sufficient-statistics str)
  (let [(processed (preprocess str))]
    (map (lambda (group) (cons (car group) (length group)))
         (group-by identity processed))))

; takes an occurrence dict and
; returns a dict from the keys to their maximum
; likelihood estimates
(define (maximum-likelihood occurrences)
  (let ([total (apply + (map cdr occurrences))])
    (map (lambda (pair) (cons (car pair) (/ (cdr pair) total)))
         occurrences)))

; takes a string and returns a dict
; from letters a-z to their maximum likelihood estimate
(define (estimate-ML str)
  (let ([likelihood-dict (maximum-likelihood (sufficient-statistics str))])
    (map (lambda (letter)
           (if (dict-has-key? likelihood-dict letter)
               (cons letter (dict-ref likelihood-dict letter))
               (cons letter 0)))
         (letters-in-range #\a #\z))))

; computes the laplace ML estimate
; with n_0 = 1, characters a-z
; returns a dict from letters to
; their estimates
(define (laplace-estimate str)
  (estimate-ML (string-append
                (list->string (letters-in-range #\a #\z)) str)))

(define (shift-counts counts δ)
  (map (lambda (count-pair) (cons (car count-pair)))))

; computes the ney-essen ML estimate
; takes str to compute on, δ, and sample/output space
(define (ney-essen-estimate str δ sample-space)
  (let* ([counts (sufficient-statistics str)]
         [distributable (* δ (length (filter (curry <= δ) (map cdr counts))))]
         [each-gets (/ distributable (length sample-space))])
    (maximum-likelihood (map (lambda (sample)
                               (if (dict-has-key? counts sample)
                                   (cons sample (+ each-gets (dict-ref counts sample)))
                                   (cons sample each-gets)))
                             sample-space))))

; returns the sum of likelihoods in an
; outcome to likelihood dict
; useful for sanity check
(define (likelihood-sum likelihoods)
  (apply + (map cdr likelihoods)))

; computes the log_2-likelihood of str
; using the likelihoods dict
(define (log-likelihood str likelihoods)
  (let ([occurrences (sufficient-statistics str)])
    (foldl (lambda (pair acc)
             (+ acc (* (cdr pair) (log (dict-ref likelihoods (car pair)) 2))))
           0.0
           occurrences)))

; count occurrences of letters a-z in str
(define (count-occurrences str)
  (map (lambda (pair)
         (cons (car pair) (sub1 (cdr pair))))
       (sufficient-statistics (string-append str (list->string (letters-in-range #\a #\z))))))

; takes str and returns its fingerprint
(define (fingerprint str)
  (let ([occurrences (count-occurrences str)])
    (sort (map (lambda (lst)
                 (cons (cdr (car lst)) (map car lst)))
               (group-by cdr occurrences)) < #:key car)))



; print sufficient statistics a:j
(let ([stats (sufficient-statistics mlk-text)])
(for-each (lambda (letter)
            (if (dict-has-key? stats letter)
                (displayln (string-append (string letter) " " (number->string (dict-ref stats letter))))
                (displayln (string-append (string letter) " 0"))))
          (letters-in-range #\a #\j)))
(println "Maximum likelihood")

; prints fingerprint of mlk text
(displayln "mlk text fingerprint")
(for-each (lambda (pair)
            (display (car pair))
            (display " ")
            (displayln (length (cdr pair))))
          (fingerprint mlk-text))

; takes estimates dict and fingerprint and prints them out
(define (print-stats-by-group estimates fingerprint)
  (for-each (lambda (category)
              (display (car category))
              (display " ")
              (display (car (cdr category)))
              (display ":")
              (displayln (exact->inexact (dict-ref estimates (car (cdr category))))))
            fingerprint))

(displayln "ML")
(print-stats-by-group (estimate-ML mlk-text) (fingerprint mlk-text))

(displayln "laplace")
(print-stats-by-group (laplace-estimate mlk-text) (fingerprint mlk-text))

(displayln "ney-essen")
(print-stats-by-group (ney-essen-estimate mlk-text 1 (letters-in-range #\a #\z)) (fingerprint mlk-text))


; test-text
; undefined
;(log-likelihood test-text-large (estimate-ML mlk-text))
(log-likelihood test-text (laplace-estimate mlk-text))
(log-likelihood test-text (ney-essen-estimate mlk-text 1 (letters-in-range #\a #\z)))

(displayln "Large Test Text")
; test-text-large
; undefined
; (log-likelihood test-text-large (estimate-ML mlk-text))
(log-likelihood test-text-large (laplace-estimate mlk-text))
(log-likelihood test-text-large (ney-essen-estimate mlk-text 1 (letters-in-range #\a #\z)))

; training-text
(displayln "Test Data")
(log-likelihood mlk-text (estimate-ML mlk-text))
(log-likelihood mlk-text (laplace-estimate mlk-text))
(log-likelihood mlk-text (ney-essen-estimate mlk-text 1 (letters-in-range #\a #\z)))

; ----------------------- PART 4 ---------------------------------------

(require math/number-theory)
(require plot)
(require math/base)
(require math/special-functions)

; computes the log gamma function
(define (my-log-gamma x)
  (define (my-log-gamma-helper x)
  (if (= x 1)
      (log x)
      (+ (log x) (my-log-gamma-helper (sub1 x)))))
  (my-log-gamma-helper (sub1 x)))

; implementation of n choose k 
(define (choose n k)
  (expt euler.0 (- (log-gamma (add1 n)) (log-gamma (add1 k)) (log-gamma (add1 (- n k))))))

(define (my-choose n k)
  (expt euler.0 (- (my-log-gamma (add1 n)) (my-log-gamma (add1 k)) (my-log-gamma (add1 (- n k))))))

; computes the probability of k successes
; in n trials with probability θ of each success
(define (binomial-distribution n k θ)
  (* (binomial n k) (expt θ k) (expt (- 1 θ) (- n k))))

;(define n 100)
;(define p .3141)

; get a list of pairs 
; where car = k/n and cdr = binomial distribution with n, p at k
; and k = 0:(n+1)
(define (get-data-points n p)
  (map (lambda (k) (cons (/ k n) (binomial-distribution n k p))) (range 0 (add1 n))))

; datapoints 
(define data-points
  (get-data-points 100 .3141))

; sanity check
(define test
  (apply + (map cdr data-points)))

; turns a pair into a list
(define (pair->list pair)
  (list (car pair) (cdr pair)))

; takes list of point pairs
; and returns list to be used in
; an error-bar plot
(define (get-bar-data data-points)
  (map (lambda (point) (list (car point) 0 (cdr point)))
       data-points))

(plot-new-window? #t)




; returns the probability from the discrete distribution that the absolute
; error from the real value is greater than ϵ
(define (probability-absolute-error-greater-than distribution ϵ θ-real)
  (apply + (map cdr (filter
                     (lambda (pair) (> (abs (- (car pair) θ-real)) ϵ))
                     distribution))))

; same as above but for relative error
(define (probability-relative-error-greater-than distribution ε θ-real)
  (apply + (map cdr (filter
                     (lambda (pair) (> (/ (abs (- (car pair) θ-real)) θ-real) ε))
                     distribution))))

(probability-absolute-error-greater-than data-points .02 .3141)
(probability-relative-error-greater-than data-points .02 .3141)

; does a coin flip, returns 0 for tails 1 for heads
(define (coin-flip θ)
  (if (< (random) θ)
      1
      0))

; does n coin flips, returns number of heads
(define (coin-flips n θ)
  (if (= n 0)
      0
      (+ (coin-flip θ) (coin-flips (sub1 n) θ))))

(coin-flips 100 .3141)

(define (error-points ϵ p data)
  (if (> ϵ 1)
      (list)
      (cons (cons ϵ (probability-absolute-error-greater-than data ϵ p)) (error-points (+ ϵ .005) p data))))

(define (plot-points point-list title x-label)
  (plot (list (points (map pair->list point-list))
              (error-bars (get-bar-data point-list)))
        #:y-max 1 #:y-min 0
        #:y-label "Probability" #:x-label x-label
        #:title title
        #:width 800
        #:height 600))

(define (plot-data-points)
  (plot (list (points (map pair->list data-points))
              (error-bars (get-bar-data data-points)))
        #:y-max .1 #:y-min 0
        #:y-label "Probability" #:x-label "θ heads Maximum Likelihood Estimate"
        #:title "Probability distribution of θ heads Maximum Likelihood Estimate"
        #:out-file "ml-estimate1.png"
        #:width 800
        #:height 600))

(define (plot-data-points2)
  (plot (list (points (map pair->list (error-points 0 .3141 data-points)))
              (error-bars (get-bar-data (error-points 0 .3141 data-points))))
        #:y-max 1 #:y-min 0
        #:y-label "Probability" #:x-label "ϵ"
        #:title "Probability θ ML estimate greater than ϵ"
        #:out-file "error-estimate1.png"
        #:width 800
        #:height 600))

(define (plot-data-points3)
  (plot (list (points (map pair->list (get-data-points 100 .34)))
              (error-bars (get-bar-data (get-data-points 100 .34))))
        #:y-max .1 #:y-min 0
        #:y-label "Probability" #:x-label "θ' heads Maximum Likelihood Estimate"
        #:title "Probability distribution of θ' heads Maximum Likelihood Estimate"
        #:out-file "ml-estimate2.png"
        #:width 800
        #:height 600))

(define (plot-data-points4)
  (plot (list (points (map pair->list (error-points 0 .34 (get-data-points 100 .34))))
              (error-bars (get-bar-data (error-points 0 .34 (get-data-points 100 .34)))))
        #:y-max 1 #:y-min 0
        #:y-label "Probability" #:x-label "ε"
        #:title "Probability θ' ML estimate greater than ε"
        #:out-file "error-estimate2.png"
        #:width 800
        #:height 600))

(probability-absolute-error-greater-than (get-data-points 100 .34) .02 .34)
(probability-relative-error-greater-than (get-data-points 100 .34) .02 .34)

(coin-flips 100 .34)




