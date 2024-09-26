#| 

  Cluster watching in birthdays in Racket.Gamble 

  From 
  John Brignell "Of birthdays and clusters"
  https://www.numberwatch.co.uk/of_birthdays_and_clusters.htm
  """
  At the time, ninety people had died of vCJD. The probability of two of them 
  having the same birthday is 0.999993848, i.e. a certainty. In fact there was 
  an evens chance of three of them having the same birthday. Likewise, if we 
  divided the UK up into 1000 areas of equal population, we would find by the 
  same calculation that the probability of two of the ninety coming from the 
  same area is about 0.98. Yet the original Queniborough two were claimed to be 
  statistically significant and the hunt was begun for more 'linked pairs'.
  
  """
  
  Compare with my R code (with Swedish comments) at
  http://www.hakank.org/sims/simulering.html "Cluster watching"

  In summary: 
  - The probability that at least two of 90 persons have the same birthday is
    about 1.
    We would require about 4 person having the same birthdays to give it
    some sort of significancy.

  - The probability that at least two of 90 persons live in the same of 1000
    regions is about 0.987.
    We would require about 3 person living in the same region to give it
    some sort of significancy.

  * 90 people having birhday on a certain day (m = 90)

  var : meanVal
  18/73: 1.0
  mean: 0.2465753424657534

  var : maxVal
  3: 0.48700000000000004
  2: 0.46900000000000003
  4: 0.04
  5: 0.004
  mean: 2.579
  Credible interval (0.94): 2..3
  Percentiles:
  (0.01 2)
  (0.025 2)
  (0.1 2)
  (0.05 2)
  (0.25 2)
  (0.5 3)
  (0.75 3)
  (0.84 3)
  (0.9 3)
  (0.95 3)
  (0.975 4)
  (0.99 4)
  (0.999 4)
  Histogram:
  2: 471
  3: 482
  4: 46 
  5: 1  

  var : p
  #t: 1.0
  mean: 1.0

  var : p2
  2: 0.29300000000000004
  1: 0.229
  3: 0.21700000000000003
  4: 0.115
  0: 0.084
  5: 0.043
  6: 0.015
  7: 0.004
  mean: 2.259
  Credible interval (0.94): 0..5
  Percentiles:
  (0.01 0)
  (0.025 0)
  (0.1 1)
  (0.05 0)
  (0.25 1)
  (0.5 2)
  (0.75 3)
  (0.84 4)
  (0.9 4)
  (0.95 5)
  (0.975 5)
  (0.99 6)
  (0.999 8)
  Histogram:
  0: 81 
  1: 214
  2: 287
  3: 227
  4: 125
  5: 45 
  6: 17 
  7: 2  
  8: 2  

  * 90 people from 1000 regions (m = 1000)
    For the probability of at least 2 of the 90 persons came from the same
    of the 1000 areas.

  var : meanVal
  9/100: 1.0
  mean: 0.09

  var : maxVal
  2: 0.896
  3: 0.08399999999999999
  1: 0.018
  4: 0.002
  mean: 2.07
  Credible interval (0.94): 2..3
  Percentiles:
  (0.01 1)
  (0.025 2)
  (0.1 2)
  (0.05 2)
  (0.25 2)
  (0.5 2)
  (0.75 2)
  (0.84 2)
  (0.9 2)
  (0.95 3)
  (0.975 3)
  (0.99 3)
  (0.999 4)
  Histogram:
  1: 18 
  2: 884
  3: 93 
  4: 4  
  5: 1  

  var : p
  #t: 0.9820000000000001
  #f: 0.018
  mean: 0.9820000000000001

  var : p2
  0: 0.6950000000000001
  1: 0.246
  2: 0.052
  3: 0.004
  4: 0.002
  5: 0.001
  mean: 0.375
  Credible interval (0.94): 0..1
  Percentiles:
  (0.01 0)
  (0.025 0)
  (0.1 0)
  (0.05 0)
  (0.25 0)
  (0.5 0)
  (0.75 1)
  (0.84 1)
  (0.9 1)
  (0.95 1)
  (0.975 2)
  (0.99 2)
  (0.999 3)
  Histogram:
  0: 690
  1: 261
  2: 44 
  3: 4  
  4: 1  


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model m)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 90) ; number of births

   ; For each of the n births get a random day/region
   (define birthDays (for/list ([i n]) (random-integer m)))
   
   ; The number of births of each days / people from a certain region
   (define births (for/list ([j m])
                    (sum (for/list([i n]) (if (= (list-ref birthDays i) j) 1 0)))))
   
   (define meanVal (avg births))
   (define maxVal (apply max births))
   
   ; What is the probability that there are more than 1 births on one of the days
   ; (at last 2 persons coming from the same region)
   (define p (> maxVal 1))

   ; How many days/regions has values larger than 1?
   (define p2 (for/sum ([i n]) (if (> (list-ref births i) 1) 1 0)))
    
   (list meanVal
         maxVal
         p
         p2
         )
   
   )
)

(for ([m '(90 1000)])
  (newline)
  (displayln (format "m: ~a" m))
  (show-marginals (model m)
                  (list "meanVal"
                        "maxVal"
                        "p"
                        "p2"
                        )
                  #:num-samples 1000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  #:credible-interval 0.94
                  #:show-histogram? #t
                  #:show-percentiles? #t
                  )
  )

