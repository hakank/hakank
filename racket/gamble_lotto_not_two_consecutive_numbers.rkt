#| 

  Lotto not two consecutive numbers in Racket.Gamble 

  From some unknown source, but I quoted it here: http://www.hakank.org/sims/simulering.html
  """
  One day I was asked 
  
  What is the probability that no two numbers are consecutive in
  a lottery draw?
  
  Information:

  National Lottery in UK is to draw 6 different numbers from 1 to 49.
  so, for example,

    (a)   1  4  7   12  19  44    - no consecutive numbers
    (b)   3  6  17  18  44  46    - 17 and 18 are consecutive 
    (c)   1  2  3   17  29  49    - 1, 2 and 3 are consecutive

  We are asking the probability that class (a) occurs. Hope this is clear.

  Observation shows that it is NOT a small number (actually near half-and-half).
  ......

  A Monte Carlo simulation experiment produced 50,558 sets of six numbers 
  between 1 and 49 with none consecutive out of 100,000 trials giving an 
  estimated probability of .50558.
  """

  Enumeration is too slow, so importance sampler is used instead:

  var : s
  0: 0.5044799999999999
  1: 0.3894499999999999
  2: 0.09629999999999997
  3: 0.009429999999999997
  4: 0.00033999999999999997
  mean: 0.6116999999999999

  var : prob
  #t: 0.5044799999999999
  #f: 0.4955199999999999
  mean: 0.5044799999999999



  This is a port of my WebPPL model lotto_not_two_consecutive_numbers.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model)
  (; enumerate ; #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 49)
   (define m 6)

   (define a (range 1 (add1 n)))
   ;; The selected Lotto numbers
   (define lotto (sort (draw-without-replacement m a) <))

   (define s (for/sum ([i (sub1 m)])
               (if (= 1 (- (list-ref lotto (add1 i)) 
                           (list-ref lotto i))) 1 0)))

   ;; Probability of not getting any consecutive numbers
   (define prob (= s 0))
    
   (list s
         prob
         )
   
   )
)

(show-marginals (model)
                (list  "s"
                       "prob"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.94
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
