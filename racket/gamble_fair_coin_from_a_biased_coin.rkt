#| 

  Fair coin from a biased coin in Racket.Gamble 

  http://cplint.eu/e/von_neumann_trick.swinb
  """
  If you have a biased coin, how to obtain a fair coin, i.e., from a coin that lands heads with 
  probability p with p≠0.5, how to generate samples from (heads,tails) with P(heads)=P(tails)=0.5?

  John von Neuamnn gave the following solution

   1) Toss the coin twice.
   2) If you get HT return H, if you get TH return T
   3) Otherwise, return to 1 and start over

  In fact, if p is the probability of the biased coin landing heads, then the outcomes HT 
  and TH are equiprobable with probability p(1−p). However, with probability pp+(1−p)(1−p)
  these results are not obtained. In this case, we simply repeat the process. The probability 
  that we never get HT or TH is 0 so this procedure ends with certainty.

  See   
  - https://en.wikipedia.org/wiki/Fair_coin#Fair_results_from_a_biased_coin
  - von Neumann, John (1951). "Various techniques used in connection with random digits". 
    National Bureau of Standards Applied Math Series. 12: 36.
  """

  var : coin 0
  head: 0.5045
  tail: 0.4955

  var : biased coin 0
  head: 0.7989999999999999
  tail: 0.20099999999999998



  This is a port of my WebPPL model fair_coin_from_a_biased_coin.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)

   (defmem (biased_coin i)
     (categorical-vw2 (vector 0.8 0.2) (vector "head" "tail")))
    
   (define (coin i)
     (if (not (eq? (biased_coin i) (biased_coin (add1 i))))
         (biased_coin i)
         (coin (+ i 2))))
   
   (list (coin 0)
         (biased_coin 0)
         )
   
   )
  )

(show-marginals (model)
                (list  "coin 0"
                       "biased coin 0"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
