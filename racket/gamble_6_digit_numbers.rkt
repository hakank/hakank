#| 

  6 digit numbers in  Racket Gamble.

  From 
  Gunnar Blom, Lars Holst, Dennis Sandell:
  "Problems and Snapshots from the World of Probability"
  Page 19f, Problem 2.5 Problems concerning random numbers

  Given the 6 digits numbers:

  a) Problem 1
     find the probability that at least one of the digits 0..9
     appears exactly twice.
     Answer: 2943/4000 ~ 0.7358

  b) Problem 2
     find the probability that at least two of the digits 0..9
     appears exactly once.
     Answer: 1179/1250 ~ 0.9432

  The p variable is the answer of the problem (k_occ is shown just 
  for fun :-)).

  From enumerate (takes about 55s)

  (n 6 m 2 k 1)
  var : k_occ
  1: 9963/20000 (0.49815)
  0: 1057/4000 (0.26425)
  2: 567/2500 (0.2268)
  3: 27/2500 (0.0108)
  mean: 19683/20000 (0.98415)

  var : p
  #t: 2943/4000 (0.73575)
  #f: 1057/4000 (0.26425)
  mean: 2943/4000 (0.73575)

  (n 6 m 1 k 2)
  var : k_occ
  4: 567/1250 (0.4536)
  2: 297/1250 (0.2376)
  6: 189/1250 (0.1512)
  3: 63/625 (0.1008)
  1: 2187/50000 (0.04374)
  0: 653/50000 (0.01306)
  mean: 177147/50000 (3.54294)

  var : p
  #t: 1179/1250 (0.9432)
  #f: 71/1250 (0.0568)
  mean: 1179/1250 (0.9432)


  This is a port of my WebPPL model 6_digit_numbers.wppl 
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model n k m)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (defmem (d i) (random-integer 10))
   
   (define digits (map (lambda (i) (d i) ) (range n)))
   
   ;; Number of occurrences of each digit in digits
   (define occ (for/list ([v 10])
                 (count-occurrences v digits)))
   
   ;; Number of digits that occurs exactly k times
   (define k_occ (count-occurrences k occ))

   ; Probabily that this occurs at least m times
   (define p (>= k_occ m))
   
   (list ; digits
         k_occ
         p 
         )
   
   )
  )

(define (run n m k) 
  (show2 "n" n "m" m "k" k)
  (show-marginals (model n m k)
                  (list ; "digits"
                        "k_occ"
                        "p"
                        )
                  #:num-samples 10000
                  ; #:truncate-output 1
                  ; #:skip-marginals? #t
                  ; #:credible-interval 0.94
                  ; #:show-stats? #t
                  ; #:show-histogram? #t
                  )
  )


;; Part 1:
;; At least one (m) of the digits 0..9 appears exactly twice (k).
(run 6 2 1)

;; Parth 2:
;; At least one of the digits 0..9 appears exactly once.
(run 6 1 2)

;; At least two of the digits 0..9 appears exactly twice.
;; (run 6 2 2)


;; At least one of the digits 0..9 appears exactly three times.
;;(run 6 1 3)

