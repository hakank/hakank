#| 

  Russian roulette in Racket/Gamble 

  If two persons (a and b) play russian roulette until one of them dies,
  what are the probabilites of survival for a and b?

  The rule is that if a person survives a round he/she spins the 
  magasine again, i.e. resets the probabilities to 1/6.
  The assumption is that player 1 always start.

  dies
  Marginal:
      1 : 0.5454545454545455
      2 : 0.4545454545454545
  ppp
  Marginal:
     0.5454545454545454 : 1

  The exact probability is 6/11

  Since I want to use Enumerate, let's change the rule a little 
  by adding a limit, say 1000: If there are more than 1000 games 
  then the game stops and no one dies (output 0). 
  However, we exclude that "player 0" case by using observe/fail.

  What is the probability of death for players 1 and 2?

  variable : dies
  1: 6/11 (0.5454545454545454)
  2: 5/11 (0.45454545454545453)
  mean: 16/11 (1.4545454545454546)

  variable : exact
  6/11: 1 (1.0)
  mean: 6/11 (0.5454545454545454)


  * 6 bullets, random player starts
  variable : dies
  1: 1/2 (0.5)
  2: 1/2 (0.5)
  mean: 3/2 (1.5)

  * With 10 bullets and player 1 starts (limit still 1000)
  variable : dies
  1: 10/19 (0.5263157894736842)
  2: 9/19 (0.47368421052631576)
  mean: 28/19 (1.4736842105263157)

  variable : exact
  10/19: 1 (1.0)
  mean: 10/19 (0.5263157894736842)

  * 100 bullets
  variable : dies
  1: 100/199 (0.5025125628140703)
  2: 99/199 (0.49748743718592964)
  mean: 298/199 (1.4974874371859297)

  variable : exact
  100/199: 1 (1.0)
  mean: 100/199 (0.5025125628140703)

  * 1000 bullets
  variable : dies
  1: 1000/1999 (0.5002501250625313)
  2: 999/1999 (0.49974987493746875)
  mean: 2998/1999 (1.4997498749374687)

  variable : exact
  1000/1999: 1 (1.0)
  mean: 1000/1999 (0.5002501250625313)

  * 10000 bullets
  variable : dies
  1: 10000/19999 (0.5000250012500626)
  2: 9999/19999 (0.4999749987499375)
  mean: 29998/19999 (1.4999749987499376)

  variable : exact
  10000/19999: 1 (1.0)
  mean: 10000/19999 (0.5000250012500626)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define limit 1000)
   
   ; (define num_bullets 6)
   ; (define num_bullets 10)
   ; (define num_bullets 100)
   (define num_bullets 10000)

   (define (f a)
     (if (> (length a) limit)
         (append a (list 0))
         (if (flip (/ 1 num_bullets))
             a
             (let ([next (if (= (last a) 1) 2 1)])
               (f (append a (list next)))))))

   (define a (f '(1))) ; Player 1 is always the first shooter
   ; (define a (f (list (uniform-draw '(1 2))))) ; random player starts
   
   (define dies (last a)) ; who died?
   (define exact (/ num_bullets (- (* num_bullets 2) 1)))

   (observe/fail (not (= dies 0)))
   
   (list limit
         dies
         exact
         )
   )
)

(show-marginals (model)
                (list  "limit"
                       "dies"
                       "exact"
                     )
                    #:num-samples 100000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


