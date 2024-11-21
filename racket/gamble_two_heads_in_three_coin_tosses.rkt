#| 

  Two head in three coin tosses in Racket/Gamble 

  From Statistics101 (Resample Stats)
  http://www.statistics101.net/QuickReference.pdf
  Page 9
  """
  (S)ay you wanted to know the probability of getting exactly two heads
  in a toss of three coins.
  """

  Here are some different approaches.

  variable : c
  1: 3/8 (0.375)
  2: 3/8 (0.375)
  0: 1/8 (0.125)
  3: 1/8 (0.125)
  mean: 3/2 (1.5)

  variable : p
  #f: 5/8 (0.625)
  #t: 3/8 (0.375)
  mean: 3/8 (0.375)

  (get-prob-value (get-probs (model) #:ix 2) #t): 3/8
  (binomial_dist_pdf 3 1/2 2): 3/8



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#| 
  Resampling (exact)
|#
(define (model)
  (enumerate

   (define coin '("head" "tail"))
   (define tosses (resample 3 coin))
   (define c (count-occurrences-eq "head" tosses))
   (define p (= c 2))
   (list c
         p
         ; tosses
         )
   )
)

(show-marginals (model)
                (list  "c"
                       "p"
                       "tosses"))

(newline)
(displayln (format "(get-prob-value (get-probs (model) #:ix 2) #t): ~a" (get-prob-value (get-probs (model) #:ix 1) #t)))
(displayln (format "(binomial_dist_pdf 3 1/2 2): ~a" (binomial_dist_pdf 3 1/2 2)))
(newline)

