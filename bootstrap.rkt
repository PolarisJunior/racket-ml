#lang racket
(define hw5-data-1-file "data1.dat")
(define hw5-data-2-file "data2.dat")

(define (file->data file-name)
  (map string->number
       (string-split (file->string file-name))))

(define data-1 (file->data hw5-data-1-file))
(define data-2 (file->data hw5-data-2-file))

(define (mean data-set)
  (/ (apply + data-set)
     (length data-set)))
; section 3
(mean data-2)

(define chunks 56)

(define (split-data K data-set)
  (let* ([len (length data-set)]
         [chunk-size (/ len K)])
    (for/list ([i (range 0 K)])
      (take (drop data-set (* i chunk-size)) chunk-size))))

(define (sub-list-means K data-set)
   (map mean (split-data K data-set)))

(define (median values)
  (let ([len (length values)]
        [sorted-values (list->vector (sort values <))])
    (if (even? len)
        (/ (+ (vector-ref sorted-values (/ len 2))
              (vector-ref sorted-values (sub1 (/ len 2))))
           2)
        (vector-ref sorted-values (floor (/ len 2))))))
; method of moments
(define (MOM K data-set)
  (median (sub-list-means K data-set)))

(MOM chunks data-2)

(define (bootstrap-producer data-set)
  (let ([len (length data-set)]
        [data-set (list->vector data-set)])
    (lambda () (vector-ref data-set (random len)))))

; resample n times from data-set
(define (get-samples data-set n)
  (let ([producer (bootstrap-producer data-set)])
   (for/list ([i (range n)])
     (producer))))

; 1000 bootstrap samples of size len(data-2) from data-2
(define bootstrap-samples
  (for/list ([n (range 1000)])
    (get-samples data-2 (length data-2))))

; list of mean of bootstrap samples from data-2
; samples are lists
(define bootstrap-mean-ML
  (map mean bootstrap-samples))

; list of MOM of bootstrap samples from data-2
; samples are lists
(define bootstrap-mean-MOM
  (map (curry MOM chunks) bootstrap-samples))

(define (variance data μ)
  (/ (apply + (map (lambda (value)
                     (expt (- value μ) 2)) data))
     (sub1 (length data))))

(define (variance-biased data μ)
  (mean (map (lambda (value)
                     (expt (- value μ) 2)) data)))

; E[μ^ML,b]
(define expected-μ-ML-b
  (mean bootstrap-mean-ML))
; E[μ^MOM,b]
(define expected-μ-MOM-b
  (mean bootstrap-mean-MOM))

(define bootstrap-variance-of-ML
  (variance bootstrap-mean-ML expected-μ-ML-b))

(define bootstrap-variance-of-MOM
  (variance bootstrap-mean-MOM expected-μ-MOM-b))

; b 1
bootstrap-variance-of-ML
; b 2 
bootstrap-variance-of-MOM

; p 2
(mean data-1)

(variance data-1 (mean data-1))
(variance-biased data-1 (mean data-1))
(/ (variance data-1 (mean data-1)) (length data-1))
(sqrt (/ (variance data-1 (mean data-1)) (length data-1)))

(define bootstrap-samples-1
  (for/list ([n (range 1000)])
    (get-samples data-1 (length data-1))))

(define expected-μ-ML-1
  (mean (map mean bootstrap-samples-1)))

(define bootstrap-variance-of-μ-1
  (variance (map mean bootstrap-samples-1) expected-μ-ML-1))

; p 2e
bootstrap-variance-of-μ-1