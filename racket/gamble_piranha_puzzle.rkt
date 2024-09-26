#| 

  Piranha puzzle in Racket.Gamble 

  (From Tijms 2004)
  http://cs.ioc.ee/ewscs/2020/katoen/katoen-slides-lecture1.pdf
  """
  One fish is contained within the confines of an opaque fishbowl.
  The fish is equally likely to be a piranha or a goldfish. A sushi
  lover throws a piranha into the fish bowl alongside the other fish.
  Then, immediately, before either fish can devour the other, one
  of the fish is blindly removed from the fishbowl. The fish that
  has been removed from the bowl turns out to be a piranha. What 
  is the probability that the fish that was originaly in the bowl
  by itself was a piranha?

  The Piranha Puzzle Program
  
   f1 := gf (0.5) f1 := pir;
   f2 := pir;
   s := f1 (0.5) s := f2;
   observe(s = pir)


  E(f1 = pir | P terminates) = 1/2 / 3/4 = 2/3
  """

  var : f1
  piranha: 2/3 (0.6666666666666666)
  goldfish: 1/3 (0.3333333333333333)

  var : f2
  piranha: 1 (1.0)

  var : s
  piranha: 1 (1.0)

  var : firstFishPiranha
  #t: 2/3 (0.6666666666666666)
  #f: 1/3 (0.3333333333333333)
  mean: 2/3 (0.6666666666666666)

  This is a port of my WebPPL model piranha_puzzle.wppl

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

   (define f1 (categorical-vw2 (vector 1/2 1/2) (vector "goldfish" "piranha")))
   (define f2 "piranha")
    
   (define s (categorical-vw2 (vector 1/2 1/2) (vector f1 f2)))
   
   (observe/fail (eq? s "piranha"))

   (list f1
         f2
         s
         (eq? f1 "piranha")
    )

   )
)

(show-marginals (model)
                (list  "f1"
                       "f2"
                       "s"
                       "firstFishPiranha"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


