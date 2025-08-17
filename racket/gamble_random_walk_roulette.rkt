#| 

  Random walk roulette (Mosteller) in Racket.Gamble 

  From Mosteller "Fifty Challenging Problem in Probability"
  """
  7. Curing the Compulsive Gambler
  Mr. Brown always bets a dollar on the number 13 at roulette against the
  advice of Kind Friend To help cure Mr Brown of playing roulette, Kind Friend
  always bets Brown $20 at even money that Brown will be behind at the end of
  36 plays. How is the cure working?
  (Most American roulette wheels have 38 equally likely numbers. If the
  player's number comes up, he is paid 35 times his stake and gets his original
  stake back; otherwise he loses his stake)
  
  ... 

  And so all told Mr. Brown gains +4.68 - 1.89 = +2.79 dollars per 36
  trials; he is finally making money at roulette. Possibly Kind Friend will be
  cured first. Of course, when Brown loses all 36, he is out $56, which may
  jolt him a bit.
  """ 

  According to this model:   
  Estimated gain of Mr. Brown is $3.875400000000003, and of Kind Friend: $-4.792000000000001


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define (model a b limit)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 36) ; 36 plays
   

   (define final (sum (for/list ([r n]) (if (= (add1 (random-integer 38)) 13) 36 -1))))
   (define outcome (< final 0))

   (define brown (+ final (if (> final 0) 20 -20)))
   (define kind-friend (if (< final 0) 20 -20))

   (list final
         outcome
         brown
         kind-friend
         )
   
   )
  
)


(show-marginals (model 0 0 1000)
                (list  "final"
                       "outcome"
                       "brown"
                       "kind-friend"
                     )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval '(0.84 0.9 0.95 0.99 0.999)
                #:show-histogram? #t
                ; #:show-percentiles? #t
                )
