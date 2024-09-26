#| 

  Dartboard in Racket/Gamble 

  From Statistics101 (Resample Stats)
  http://www.statistics101.net/QuickReference.pdf
  Page 19
  """
  A dartboard is divided into twenty equal wedges, ignoring the bull's eye. 
  Only six of the twenty wedges are worth points, so the probability of scoring on 
  one throw is 6/20, or 0.3, assuming your throws always hit the board. 
  What is the chance of hitting at least one scoring region in three consecutive throws? 
  (From CliffsQuickReview Statistics, p.48).
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define scoreProb (append (rep 3 "score") (rep 7 "no score")))
(define (model1)
  (enumerate

   (define num_throws 3)
   (define three_throws (resample num_throws scoreProb))
   (define s (count-occurrences-eq "score" three_throws))
   (define p (>= s 1))
   
   (list s
         p)
   
   )
)

(displayln "Model 1, using resample")
(show-marginals (model1)
                (list  "s"
                       "p"))


; Using binomial
(define (model2)
  (enumerate

   (define s (binomial 3 6/20))
   (define p (>= s 1))
   (list s
         p)        
   )
)

(displayln "Model 2, using binomial")
(show-marginals (model2)
                (list  "s"
                       "p"))


