#| 

  Unfair coin in Racket Gamble.

  From a Brilliant quiz
  """
  Maja thinks her coin is unfair. She flips it 4 times and gets heads every time. 
  She calculates that this would only occur with a fair coin roughly 6% of the time.

  Can she conclude there is a roughly 94% chance that her coin is unfair?
  """

  var : b
  2: 0.375
  1: 0.24999999999999994
  3: 0.24999999999999994
  0: 0.06250000000000001
  4: 0.06250000000000001
  mean: 1.9999999999999998
  ix: 0
  Credible interval (0.94): 0..4
  Credible-interval2 (0.94): 0..4 (ps: (0.030000000000000027 0.97))
  Percentiles::
  (0.01 0)
  (0.1 1)
  (0.025 0)
  (0.25 1)
  (0.5 2)
  (0.75 3)
  (0.84 3)
  (0.9 3)
  (0.975 4)
  (0.99 4)
  (0.999 4)
  Histogram:
  0: 646 
  1: 2518
  2: 3758
  3: 2478
  4: 605 



  The credible interval of 0.94 yields 0..4, which indicates that one can
  actually assume that a fair coin can give 4 heads.

  However, throwing a coin 6 times and all 6 throws shows head is more 
  suspicious which can be seen with this experiment

  (show-credible-interval(repeat (lambda () (sample  (enumerate (define b (binomial 6 0.5)) b) )) 10000) 0.94)
  Credible interval (0.94): 1..5

  Or more clearly using dist-inv-cdf and dist-pdf
  > (dist-inv-cdf (binomial-dist 6 0.5) 0.94)
  5

  > (dist-pdf (binomial-dist 6 0.5) 6)
  0.015625

  Here's the probabilities that we get n heads in n throws for n=0..10:

  (n 0 (dist-pdf (binomial-dist n 0.5) n) 1.0)
  (n 1 (dist-pdf (binomial-dist n 0.5) n) 0.5)
  (n 2 (dist-pdf (binomial-dist n 0.5) n) 0.25)
  (n 3 (dist-pdf (binomial-dist n 0.5) n) 0.125)
  (n 4 (dist-pdf (binomial-dist n 0.5) n) 0.0625)
  (n 5 (dist-pdf (binomial-dist n 0.5) n) 0.03125)
  (n 6 (dist-pdf (binomial-dist n 0.5) n) 0.015625)
  (n 7 (dist-pdf (binomial-dist n 0.5) n) 0.0078125)
  (n 8 (dist-pdf (binomial-dist n 0.5) n) 0.00390625)
  (n 9 (dist-pdf (binomial-dist n 0.5) n) 0.001953125)
  (n 10 (dist-pdf (binomial-dist n 0.5) n) 0.0009765625)


  This is a port of my PSI model unfair_coin.psi

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (enumerate 
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 4)

   (define b (binomial n 0.5))

   (list b
         )
   
   )
  )

(show-marginals (model)
                (list "b"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                #:credible-interval 0.94
                #:credible-interval2 0.94                
                ; #:show-stats? #t
                #:show-percentiles? #t
                #:show-histogram? #t
                )

(displayln "\nThrow a coin 6 times")
(show-credible-interval
 (repeat (lambda () (sample (enumerate (define b (binomial 6 0.5)) b) ))
         10000)
 0.94)


(show "\n(dist-inv-cdf (binomial-dist 6 0.5) 0.94)"
      (dist-inv-cdf (binomial-dist 6 0.5) 0.94))

(show "(dist-pdf (binomial-dist 6 0.5) 6)"
      (dist-pdf (binomial-dist 6 0.5) 6))
(newline)

(for ([n 11])
  (show2 "n" n "(dist-pdf (binomial-dist n 0.5) n)"
        (dist-pdf (binomial-dist n 0.5) n)))
  
(newline)
