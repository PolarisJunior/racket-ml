#lang racket

(define digits-file "digits.dat")

(define (file->data path)
  (string-split (file->string path)))

(define numbers (file->data digits-file))

(define first-60-numbers (take numbers 60))

(define first-digit (curryr substring 0 1))

(displayln "MSD of 1 in first 60 numbers")
(define number-of-ones (length (filter (curry equal? "1") (map first-digit first-60-numbers))))
number-of-ones


(require math/number-theory)

(define (bernoulli n p k)
  (* (binomial n k)
     (expt p k)
     (expt (- 1 p) (- n k))))

(define n_d (length first-60-numbers))

(define (B n k)
  (bernoulli n (/ 1 9) k))
(define B-60 (curry bernoulli n_d
                 (/ 1 9)))

(define (p_n-t-60 t)
  (exact->inexact (apply + (map (curry B n_d) (range t (add1 n_d))))))
(define result1
  (p_n-t-60 number-of-ones))
(define result2
  (p_n-t-60 6))

(define number-of-ones-all
  (length (filter (curryr equal? "1") (map first-digit numbers))))

; too long to execute
;(define result3
;  (exact->inexact (apply + (mapf (curry B (length numbers))
;                                (range number-of-ones-all (add1 (length numbers)))))))

(define first-digit-only-60 (map first-digit first-60-numbers))

(define sufficient-statistics-60
  (map (lambda (lst)
         (cons (car lst) (length lst)))
       (group-by identity first-digit-only-60)))

(define (ss->ml-model ss n)
  (map (lambda (pair)
         (cons (car pair) (/ (cdr pair) n))) ss))

(define (multinomial ss n)
  (apply (curry / (factorial n))
       (map (compose factorial cdr)
            ss)))
(displayln "multinomial example")
(apply (curry / (factorial 60))
       (map (compose factorial cdr)
            sufficient-statistics-60))
(displayln "model A")
(define likelihood-a
  (exact->inexact (* (multinomial sufficient-statistics-60 60) (expt (/ 9) 60))))
likelihood-a

(define (ss->ml ss n)
  (map (lambda (pair) (cons (car pair)
                            (/ (cdr pair) n))) ss))

(define likelihood-b
  (exact->inexact (* (multinomial sufficient-statistics-60 60)
                   (apply * (map (lambda (pair)
                                   (expt (/ (cdr pair) 60)
                                         (cdr pair)))
                                 sufficient-statistics-60)))))
(displayln "model B")
likelihood-b

(displayln "ratio")
(/ likelihood-a likelihood-b)

(define ss-all
  (map (lambda (lst)
         (cons (car lst) (length lst)))
       (group-by identity (map first-digit numbers))))

(define log-likelihood-a-all (+ (log (multinomial ss-all (length numbers)))
                                (* (log (/ 9)) (length numbers))))
(define log-likelihood-b-all
  (+ (log (multinomial ss-all (length numbers)))
     (apply + (map (lambda (fd)
                    (log (/ (dict-ref ss-all fd) (length numbers))))
                  (map first-digit numbers)))))
(displayln "test statistic all")
(* 2 (- log-likelihood-b-all log-likelihood-a-all))
; part 2
(require plot)

(define flintstone-path "flintstones_1.dat")
(define flintstone-data
  (map string->number (file->data flintstone-path)))
(define l_0
  8)


(define flintstone-mean
  (/ (apply + flintstone-data)
     (length flintstone-data)))

(define flintstone-points
  (for/list ([flintstone-length flintstone-data]
             ; if we want a 2d plot
             ;[n (range 1 (add1 (length flintstone-data)))]
             )
    ;(list n flintstone-length)
    (list flintstone-length 0)))
(define flintstone-mean-point
  (list (list flintstone-mean 0)))
(define flintstone-l_0-point
  (list (list l_0 0)))

(plot-new-window? #t)

(define (plot-flintstone)
  (plot (list (points flintstone-points #:label "Observed measurements")
              (points flintstone-mean-point #:color "red" #:label "Data average"
                      #:size 10 #:fill-color "red")
              (points flintstone-l_0-point #:color "green" #:label "Expected value"
                      #:size 10 #:fill-color "green"))
        #:width 800 #:height 600 #:title "Plot of 36 length measurements"
        #:x-label "Flintstone length (in)"
        #:x-max 9.1 #:x-min 6.9 #:y-label ""
        #:out-file "flintstone-plot.png"))

(define Var-Y
  (/ 3))

(define Z
  (/ (- (apply + flintstone-data) (* (length flintstone-data) 8))
     (sqrt (* (length flintstone-data) Var-Y))))