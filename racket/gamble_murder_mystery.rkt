#| 

  Murder mystery in Racket Gamble.

  From
  Andy Gordpn:
  "Reverend Bayes, meet Countess Lovelace: Probabilistic Programming for Machine Learning"
  https://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2012/Reverend-Bayes-meet-Countess-Lovelace-Probabilistic-Programming-for-Machine-Learning
  Around @14:00

  """
  Miss Scarlett dunnit 30%. Col Mustard dunnit 70%.
  Scarlett uses gun 3%, uses pipe 97%.
  Mustard uses gun 80%, uses pipe 20%.
  ...
  We found a gun at the Scene.
  What is the probability that Scarlett dunnit?
  """

  * Original question: we found a gun, what is the probability that Scarlet did it?

  var : scarlett
  #f: 0.984182776801406
  #t: 0.015817223198594018
  mean: 0.015817223198594018

  var : mustard
  #t: 0.7
  #f: 0.30000000000000004
  mean: 0.7

  var : withGun
  #t: 0.9999999999999999
  mean: 0.9999999999999999

  var : withPipe
  #f: 0.7878207381370826
  #t: 0.21217926186291736
  mean: 0.21217926186291736

  * If we had found a pipe instead:

  var : scarlett
  #t: 0.6751740139211138
  #f: 0.32482598607888635
  mean: 0.6751740139211138

  var : mustard
  #t: 0.7
  #f: 0.3000000000000001
  mean: 0.7

  var : withGun
  #f: 0.7198839907192576
  #t: 0.2801160092807425
  mean: 0.2801160092807425

  var : withPipe
  #t: 1.0000000000000002
  mean: 1.0000000000000002


  This is a port of my WebPPL model murder_mystery.wppl

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
   
   (define scarlett (flip 0.30))
   (define mustard (flip 0.70))
    
   (define  withGun (if scarlett (flip 0.03) (flip 0.80)))
   (define withPipe (if scarlett (flip 0.97) (flip 0.20)))
    
   (observe/fail withGun)
   ;; (observe/fail withPipe)
   ;; (observe/fail withGun < withPipe);

   (list scarlett
         mustard
         withGun
         withPipe
    )

  
   )
  )

(show-marginals (model)
                (list "scarlett"
                      "mustard"
                      "withGun"
                      "withPipe"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.84
                )
