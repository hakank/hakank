#| 

  Unbiased coin in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/19
  """
  We have a weighted coin that shows a Head with probability p and tails with 1-p
  (0.5 < p < 1). How to define two equally likely events using this coin?

  Each event is a function of the outcomes of one or more coin tosses. For example, 
  we can toss 2 times and define Event1 as [HH, TT] and Event2 as [TH, HT].

  Hint: Since p is unknown, the probability of HH and TT are unreliable.
  Answer: Toss 2 times, event1 is HT, event2 is TH, and repeat the process otherwise.
  """

  var : p
  mean: 0.7506957394660627

  var : two-coins
  (H H): 0.5802999999999925
  (T H): 0.1698000000000154
  (H T): 0.16690000000001493
  (T T): 0.0830000000000067

  var : f
  T: 0.5016000000000012
  H: 0.49840000000000145

  * If we fix p to - say - 0.8, then enumerate #:limit 1e-04 gives this (after 1 minute):

  var : p
  4/5: 1 (1.0)
  mean: 4/5 (0.8)

  var : two-coins
  (H H): 9860693882929828164908345672577614608135406346245203143449110078095062656/15407285482427187519748617713009521019638261089701807236869127933182172031 (0.6400020233399624)
  (T H): 2465155520945732033325238287913221944110175159421770405281673627860937500/15407285482427187519748617713009521019638261089701807236869127933182172031 (0.15999934081557524)
  (H T): 2465155520945732033325238287913221944110175159421770405281673627860937500/15407285482427187519748617713009521019638261089701807236869127933182172031 (0.15999934081557524)
  (T T): 616280557605895288189795464605462523282504424613063282856670599365234375/15407285482427187519748617713009521019638261089701807236869127933182172031 (0.039999295028887175)

  var : f
  H: 15407286855027187519748617713009521019638261089701807236869127933182172031/30814570964854375039497235426019042039276522179403614473738255866364344062 (0.5000000445438622)
  T: 15407284109827187519748617713009521019638261089701807236869127933182172031/30814570964854375039497235426019042039276522179403614473738255866364344062 (0.4999999554561379)


  Cf gamble_fair_coin_from_a_biased_coin.rkt for a little different model.


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

   ; p should be > 0.5
   (define p0 (beta 1 1))
   (define p (if (< p0 0.5) (- 1 p0) p0))

   ; (define p (beta 1 1))
   ; (observe/fail (> p 0.5))
             
             
   ; Fixing probability (for enumerate)
   ; (define p 8/10)
   
   (define (coin) (if (flip p) "H" "T"))
   
   (define two-coins (list (coin) (coin)))

   ; The strategy shown in the Answer above
   (define (f)
     (define (loop a)
       (let ([c1 (coin)]
             [c2 (coin)])
           (if (not (eq? c1 c2))
             c1
             (loop '())
             )
         ))
     (loop '())
     )

   (list p
         two-coins
         (f)
         )
   
   )
)

(show-marginals (model)
                (list  "p"
                       "two-coins"
                       "f"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
