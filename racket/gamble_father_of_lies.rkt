#| 

  Father of lies in Racket.Gamble 

  From https://brainstellar.com/puzzles/probability/28
  """
  A father claims about snowfall last night. First daughter tells 
  that the probability of snowfall on a particular night is 1/8. 
  Second daughter tells that 5 out of 6 times the father is lying! 
  What is the probability that there actually was a snowfall?
  """

  var : snowing
  #f: 35/36 (0.9722222222222222)
  #t: 1/36 (0.027777777777777776)
  mean: 1/36 (0.027777777777777776)

  var : father-says
  snow: 1 (1.0)

  The probability that it did snow last night is 1/36.

  Note: This assumes that the father really knows whether it snowed
        last night or not. 

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

   ; Probability of snowing is 1/8
   (define snowing (flip 1/8))

   ; Father lies 5 times of 6.
   ; We must take care of when he says "snow" both when it's snowing
   ; and when it's not snowing.
   (define father-says
     (if snowing
         (categorical-vw2 (vector 1/6 5/6) (vector "snow" "not snow"))
         (categorical-vw2 (vector 1/6 5/6) (vector "not snow" "snow"))))

   (observe/fail (eq? father-says "snow"))
        
   (list snowing
         father-says
         )
   
   )
)

(show-marginals (model)
                (list  "snowing"
                       "father-says"
                       ))

#|
  What if we add aother component: the father does not know
  if it snowed or not with a probability of 1/2, and the just
  say snow/not snow with probability 1/2.

  var : snowing
  #f: 14/15 (0.9333333333333333)
  #t: 1/15 (0.06666666666666667)
  mean: 1/15 (0.06666666666666667)

  var : father-knows
  #t: 3/5 (0.6)
  #f: 2/5 (0.4)
  mean: 3/5 (0.6)

  var : father-says
  snow: 1 (1.0)

  Then the probability of snowing would be 1/15 given that he
  said it was snowing.

  It's a little strange that the probability that he knows
  now is 3/5.... 

|#
(define (model2)
  (enumerate

   ; Probability of snowing is 1/8
   (define snowing (flip 1/8))

   ; - If the father does not know he just say something randomly.
   ;   There is a 50% probability that he does not know.
   ; - If he knows then he lies 5 times of 6.
   (define father-knows (flip 1/2))
   (define father-says
     (if (not father-knows)
         ; Father does not know (with probability 1/2) and then just say something
         (if (flip 1/2)
             "snow"
             "not snow")
         ; father knows:
         (if snowing
             (categorical-vw2 (vector 1/6 5/6) (vector "snow" "not snow"))
             (categorical-vw2 (vector 1/6 5/6) (vector "not snow" "snow")))
         )
     )

   (observe/fail (eq? father-says "snow"))
        
   (list snowing
         father-knows
         father-says
         )
   
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "snowing"
                       "father-knows"
                       "father-says"
                       ))
