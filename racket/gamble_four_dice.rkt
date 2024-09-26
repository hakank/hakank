#| 

  Four dice in Racket/Gamble 
  
  Port of the WebPPL model
  https://gist.github.com/usptact/cc6b906aaf3470bda65bd45d6fb6c9d9#file-four_dice-wppl
  """
  4 dice: 4, 6, 8 and 12 sides
  each dice is perfect
  """

  There are four perfect dice with 4, 6, 8 and 12 sides respectively. 
  Select one of the four dice at random and roll this dice two times. 
  The outcome of the two rolls are the same. 
  Throw the selected dice a third time. What is the probability of 
  third roll same as the first two?

  * obs: #t
  Observe that the third roll is the same as the first two
  var : sel
  0: 36/65 (0.5538461538461539)
  1: 16/65 (0.24615384615384617)
  2: 9/65 (0.13846153846153847)
  3: 4/65 (0.06153846153846154)
  mean: 46/65 (0.7076923076923077)

  var : same
  #t: 1 (1.0)
  mean: 1 (1.0)

  * obs: #f
  Probability that the third roll is the same as the first two
  var : sel
  0: 2/5 (0.4)
  1: 4/15 (0.26666666666666666)
  2: 1/5 (0.2)
  3: 2/15 (0.13333333333333333)
  mean: 16/15 (1.0666666666666667)

  var : same
  #f: 59/72 (0.8194444444444444)
  #t: 13/72 (0.18055555555555555)
  mean: 13/72 (0.18055555555555555)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model [obs #f])
  (show "* obs" obs)
  (if obs
      (displayln "Observe that the third roll is the same as the first two")
      (displayln "Probability that the third roll is the same as the first two")      
    )
  (enumerate

   (define dice
     (lambda (sel)
     (case sel
       [(0) (categorical-vw2 (ones-vector 4 1/4) (list->vector (range 4)))]
       [(1) (categorical-vw2 (ones-vector 6 1/6) (list->vector (range 6)))]
       [(2) (categorical-vw2 (ones-vector 8 1/8) (list->vector (range 8)))]
       [(3) (categorical-vw2 (ones-vector 12 1/12) (list->vector (range 12)))]))
     )
   
   ;; pick one of four dice
   (define sel (categorical-vw2 (vector 1/4 1/4 1/4 1/4) (vector 0 1 2 3)))
   
   ;; roll the picked dice twice
   (define roll1 (dice sel))
   (define roll2 (dice sel))
   
   ;; two rolls must return the same face
   (observe/fail (= roll1 roll2))

   ;; roll for the last time
   (define roll3 (dice sel))

   ;; prob that third roll is the same as the first two
   (define same (= roll3 roll2 roll1))

   (when obs
     (observe/fail (= roll3 roll2))
     )
   (list sel
         same
         )
   )
)

(for ([obs '(#t #f)])
  (show-marginals (model obs)
                  (list  "sel"
                         "same"
                         ))

)
