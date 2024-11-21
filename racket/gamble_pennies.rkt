#| 

  Pennies in Racket/Gamble 

  From Statistics101 (Resampling Stats)
  File pennies.txt and penniesReadable.txt
  """
  Two players, each with a stake of 10 pennies play this game:
  A coin is tossed. If it is heads, player B gives A one penny.
  If it is tails, player A gives B one penny.
  What is the probability that one player will lose his entire
  stake of 10 pennies if they play for 200 tosses?
  From "Resampling: The New Statistics, Julian Simon, p. 110.
  -> 
  probabilityOfRuin: 0.8928
  """

  Note: I interpret this as 
  "probability that _anyone_ of the players will lose his entire stake",
  i.e. not one specific player.

  variable : s
  0: 0.16550000000001142
  1: 0.032400000000002226
  2: 0.017800000000001228
  3: 0.016800000000001158
  4: 0.014000000000000967
  ...
  183: 0.0004000000000000276
  188: 0.0003000000000000207
  185: 0.0001000000000000069
  187: 0.0001000000000000069
  190: 0.0001000000000000069
  mean: 52.620300000003624
  HPD interval (0.84): 0..114

  variable : p
  #t: 0.8345000000000009
  #f: 0.16550000000001142
  mean: 0.8345000000000009
  
  variable : first-ruin
  200: 0.10430000000000719
  41: 0.020300000000001376
  27: 0.019400000000001315
  33: 0.01870000000000128
  25: 0.018400000000001256
  ...
  177: 0.0029000000000002
  199: 0.002600000000000179
  187: 0.002300000000000159
  195: 0.002000000000000138
  9: 0.0019000000000001318
  mean: 90.54350000000622
  HPD interval (0.84): 9..163

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 200)
   (define stake 10)
   (define tosses (for/list ([i n]) (uniform-draw '(-1 1))))
   (define acc (scan + 0 tosses))
   (define s (for/sum ([v acc]) (b2i (> (abs v) stake))))
   (define p (> s 0))

   ; When is the first ruin?
   (define first-ruin (for*/first ([i n]
                                    #:when (>= (abs (list-ref acc i)) stake))
                        i))
   
   (list s
         p
         (if (not first-ruin) n first-ruin)
         )
   )
)

(show-marginals (model)
                (list  "s"
                       "p"
                       "first-ruin"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


