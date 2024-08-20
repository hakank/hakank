#| 

  Sprinkler problem in Racket Gamble.

  From http://www.cs.ubc.ca/~murphyk/Bayes/bnintro.html

var : cloudy
#t: 0.5757997218358832
#f: 0.4242002781641168
mean: 0.5757997218358832

var : sprinkler
#f: 0.5702364394993046
#t: 0.4297635605006954
mean: 0.4297635605006954

var : rain
#t: 0.7079276773296246
#f: 0.29207232267037553
mean: 0.7079276773296246

var : wet grass
#t: 1.0
mean: 1.0


  This is a port of my WebPPL model sprinkler.wppl

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

   (define cloudy (flip 0.5))   
   (define sprinkler (if cloudy (flip 0.1) (flip 0.5)))
   (define rain (if cloudy (flip 0.8) (flip 0.2)))
   
   (define wet_grass
     (cond
       [(and (not sprinkler) (not rain)) #f]
       ((and sprinkler (not rain))      (flip 0.9))
       [(and (not sprinkler) rain)      (flip 0.9)]
       ((and sprinkler rain)            (flip 0.99))
       [else #f]))

  
   (observe/fail wet_grass)
   ;; (observe/fail (not wet_grass))
  
   (list cloudy
         sprinkler
         rain
         wet_grass
         )

   )
  )

(show-marginals (model)
                (list "cloudy"
                      "sprinkler"
                      "rain"
                      "wet grass"
                      )
                #:num-samples 100000
                )
