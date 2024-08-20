#| 

  Coin toss in Racket Gamble.

  From https://people.duke.edu/~ccc14/sta-663/PyStan.html

var : p
0.6073095125250121: 0.0009999999999999994
0.6677723659067908: 0.0009999999999999994
0.6475592572219995: 0.0009999999999999994
0.5614168291768833: 0.0009999999999999994
...
0.5979291850035936: 0.0009999999999999994
0.5600699159628497: 0.0009999999999999994
0.6047837634027422: 0.0009999999999999994
0.5519253297163256: 0.0009999999999999994
mean: 0.6081250500345453
Min: 0.46714002172087243 Mean: 0.6066417352664348 Max: 0.7217552540726514 Variance: 0.0021689339858060857 Stddev: 0.04657181535871332
Credible interval (0.84): 0.5409416359061244..0.6702769318950514

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (coin-toss)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define n 100) ; Number of tosses
   (define p (beta 2 2))
   (define y (binomial n p))
    
   (observe/fail (= y 61)) ; Number of successes
    
   (list p)

   )
  )


(show-marginals (coin-toss)
                (list "p")
                #:truncate-output 4
                #:show-stats? #t
                #:credible-interval 0.84
                )
