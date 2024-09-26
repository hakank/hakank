#| 

  Half time in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/13
  """
  The probability of having accidents on a road in one hour is 3/4. 
  What is the probability of accidents in half an hour?

  Answer: 1/2
  Solution: Probability of no accident in 1 hour = 
    P(no accidents in the first half an hour ) * P(no accidents in the next half an hour)
   --> (1 - 3 / 4) = p^2 -> p = 1/2
  
  """

  Here's a simple approach (though not as simple as the reasoning in the Solution part):
  - Split into 60 minutes and generate the number of accidents. Sum the total number of accidents.
  - Take the first 30 minutes of accidents and sum these.
  - Divide the number of 30 minute accidents with the number of 60 minutes accidents

  Using enumerate

  var : t3
  1/2: 0.06799999999999996
  3/5: 0.01499999999999999
  4/7: 0.01499999999999999
  23/43: 0.013999999999999992
  6/13: 0.010999999999999992
  ...
  35/59: 0.0009999999999999994
  35/61: 0.0009999999999999994
  37/58: 0.0009999999999999994
  38/59: 0.0009999999999999994
  40/61: 0.0009999999999999994
  mean: 0.5000310045271952


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define lambda_ 3/4)
   (define t1 (for/list ([i 60]) (poisson lambda_)))
   (define t2 (for/list ([i 30]) (list-ref t1 i)))
   (define t3 (/ (sum t2) (sum t1)))
   (list (sum t1)
         (sum t2)
         t3

         )
   
   )
)

(show-marginals (model)
                (list  "t1"
                       "t2"
                       "t3"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


