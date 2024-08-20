#| 

  What is the color of the taxi in Racket Gamble.

  From https://www.bayesia.com/2018-03-02-probabilistic-reasoning-under-uncertainty

  Originally from Kahnemann, Slovic, Tversky "Judgement under uncertainty"
  

  There has been an accicent involving a taxi.
  There are two kinds of taxis:
    - yellow taxi: 85% 
    - white taxi: 15%

  A witness say: It was a white taxi involved in the accident.

  Researcher:
    - 80% of all witness statements are true
    - 20% of all witness statements are false.

  What is the probability that it was a white taxi involved in
  the accident?

  Answer: The probability that it was a white taxi involved in the accident
          is about 41%. And it's the same as in the talk.

var : involved
yellow: 0.5862068965517242
white: 0.41379310344827586

var : witness: white
#f: 0.5517241379310345
#t: 0.4482758620689656
mean: 0.4482758620689656

var : witness: yellow
#t: 0.5517241379310346
#f: 0.4482758620689655
mean: 0.5517241379310346


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (color-of-the-taxi)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    ;; Prior distributions of the different taxis. 
    (define involved (categorical-vw2 (vector 0.15 0.85)  (vector "white" "yellow")))
    
    ;; Witness says color but is is only x percent reliable.
    ;; Witness experts states that a witness can only be 80% reliable
    ;; (given the same circumstances as the accidents).
    (define (witness c)
        (if (eq? c involved) (flip 0.80) (flip 0.2)))
    
    (observe/fail (witness "white"))
    ; (observe/fail (witness "yellow"))    

    (list involved (witness "white") (witness"yellow"))
   )
  )


(show-marginals (color-of-the-taxi)
                (list "involved" "witness: white" "witness: yellow")
                )
