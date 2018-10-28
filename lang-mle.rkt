#lang racket
; list of languages
(define languages
  (list "english" "french" "german" "spanish"))

; makes string lowercase
; and returns a list form of the string
; separated on white-space
(define (clean-string s)
  (string-split (string-downcase s)))

; Takes a String of the form a 12.34
; and returns the tuple (a, 12.34/1000.0)
(define (parse-occurrence s)
  (let ([pair (clean-string s)])
    (cons (car pair)
          (/ (string->number (car (cdr pair))) 1000.0))))

; takes a file-name for a file containing
; occurrence values and returns
; a mapping from letters to their occurrence probability
(define (make-stats file-name)
  (let* ([in (open-input-file file-name)]
         [file-stream (in-lines in)]
         [result (reverse (sequence-fold (lambda (acc x)
                              (cons (parse-occurrence x) acc))
                            (list)
                            file-stream))])
    (close-input-port in)
    result))

; returns a map from languages to
; their occurrence maps
(define (stats-of-langs langs)
  (map (lambda (lang) (cons lang (make-stats (string-append lang ".dat"))))
       langs))

; @true if c is a letter
(define (letter? c)
  (and (>= (char->integer c) (char->integer #\a))
       (<= (char->integer c) (char->integer #\z))))

; @returns list of chars in s lowercased keeping only letters
(define (preprocess s)
  (filter letter?
          (string->list (string-downcase s))))

; @returns ordinal position of the letter c, indexed on 0
(define (letter->index c)
  (if (letter? c)
      (- (char->integer c) (char->integer #\a))
      (raise "not a letter" #t)))

; takes ordinal position and returns corresponding letter, index on 0
(define (index->letter i)
  (if (and (>= i 0) (<= i 25))
      (integer->char (+ (char->integer #\a) i))
      (raise "not a valid index" #t)))

; takes a list of chars and returns a mapping
; of chars to their occurrences in cs
(define (count-letters cs)
  (foldl (lambda (x acc)
           (if (dict-has-key? acc x)
               (dict-set acc x (add1 (dict-ref acc x)))
               (dict-set acc x 1)))
         (hash) cs))

; Computes log base 2
(define (log_2 x)
  (log x 2))

; @returns the probability of seeing string s in the model
; lang-model
(define (compute-probability s lang-model)
  (let* ([letter-counts (count-letters (preprocess s))]
         [keys (dict-keys letter-counts)]
         [lang-stats lang-model])
    (foldl (lambda (key acc)
             (+ acc (* (dict-ref letter-counts key) (log_2 (dict-ref lang-stats (string key))))))
           0.0
           keys)))

; @returns map from languages to the log_2 probability
; of forming s in that language
(define (compute-probability-all s langs)
  (map (lambda (lang) (cons lang (compute-probability s (dict-ref (stats-of-langs langs) lang))))
       langs))

; pretty prints a (language, probability) tuple
(define (print-prob result-pair)
  (displayln (string-append
            (car result-pair) ": " (number->string (cdr result-pair)))))

; @returns the most likely language in langs
; of s in the form (language, log_2 probability)
(define (most-likely s langs)
  (let ([probabilities (compute-probability-all s langs)])
    (foldl (lambda (lang-prob acc)
             (print-prob lang-prob)
             (if (> (cdr lang-prob) (cdr acc))
                 lang-prob
                 acc))
           (cons null -inf.0)
           probabilities)))

; prints the most likely language in langs for s 
(define (print-most-likely s langs)
  (let [(likeliest (most-likely s langs))]
    (displayln s)
    (display "The most likely language for this text is ")
    (print-prob likeliest)
    (displayln "")))

; all letters from a to z
(define all-letters
  (map index->letter (range 0 26)))

; @returns str with a-z appended to it
(define (append-all-letters str)
  (string-append str (list->string all-letters)))


; maps letters to their # occurrences in s
(define (count-letters-in-string s)
  (count-letters (preprocess s)))

; @returns total letters characters in str 
(define (total-letters str)
  (length (preprocess str)))

; maps chars a-z to their probability*1000
; of occurring in s appended with a-z
(define (final-probability-map s)
  (let* ([s (append-all-letters s)]
         [total (total-letters s)]
         [letter-counts (count-letters-in-string s)])
    (map (lambda (letter)
           (cons letter (* 1000 (/ (dict-ref letter-counts letter) total))))
         all-letters)))

; writes a dat file for the probabilities of each
; letter in lincoln_text.txt * 1000
(define (write-lincoln-probability)
  (define out (open-output-file "lincoln_text.dat" #:exists 'append))
  (for-each (lambda (pair)
            (displayln (string-append (string (car pair)) " " (number->string (exact->inexact (cdr pair)))) out))
            (final-probability-map (file->string "lincoln_text.txt")))
  (close-output-port out))

; PROBLEM 3
(print-most-likely "La verite vaut bien qu’on passe quelques annees sans la trouver." languages)

(print-most-likely "As far as the laws of mathematics refer to reality, they are not certain, as far as they are
 certain, they do not refer to reality." languages)

(print-most-likely "Chi po, non vo; chi vo, non po; chi sa, non fa; chi fa, non sa; e cosi, male il mondo va." languages)

(print-most-likely "Las cuentas, claras, y el chocolate, espeso" languages)

(print-most-likely "Wir finden in den Buchern immer nur uns selbst. Komisch, dass dann allemal die
 Freude gross ist und wir den Autor zum Genie erklaren." languages)

; PROBLEM 4
(print-most-likely "Fourscore and seven years ago our fathers brought forth on this continent a new nation,
conceived in liberty and dedicated to the proposition that all men are created equal." (cons "lincoln_text" languages))
