#| 

  Distinct 6 dice in Racket/Gamble 

  From https://www.quora.com/What-are-the-most-interesting-or-popular-probability-puzzles-in-which-the-intuition-is-contrary-to-the-solution/answer/Justin-Rising
  """
  I'm going to flip six standard dice, and count the number of distinct values that come up. 
  So if the result is (6,5,1,2,4,6) I record a five, and if the result is (6,6,4,1,1,6) I 
  record a three. What's the probability that I record a four?

  I guarantee that unless you've worked out this puzzle before, your first answer is 
  going to be pretty far off. See the comments for the right answer.

  Answer:
  ... 
  And our final answer (including the extra factor from the first paragraph) is: 
  65×(64)×4!66≈0.501543209876543
  """

  * For n=6
  var : num_unique
  4: 325/648 (0.5015432098765432)
  3: 25/108 (0.23148148148148148)
  5: 25/108 (0.23148148148148148)
  2: 155/7776 (0.01993312757201646)
  6: 5/324 (0.015432098765432098)
  1: 1/7776 (0.0001286008230452675)
  mean: 31031/7776 (3.9906121399176953)

  var : p
  #t: 325/648 (0.5015432098765432)
  #f: 323/648 (0.4984567901234568)
  mean: 325/648 (0.5015432098765432)

  * For n=10, just the distribution (importance-sampler 1 000 000 samples)
  var : num_unique
  7: 0.355549
  6: 0.345091
  8: 0.136242
  5: 0.12886
  4: 0.017068
  9: 0.016205
  3: 0.000621
  10: 0.00036
  2: 4e-6
  mean: 6.513213


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model n)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define x (for/list ([i n]) (add1 (random-integer n))))
   (define num_unique (length (remove-duplicates x)))
   (define p (= num_unique 4))
   
   (list num_unique
         p
         )

   )
)

(show-marginals (model 6)
                (list "num_unique"
                       "p"
                     )
                    #:num-samples 1000000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


