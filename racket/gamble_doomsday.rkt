#| 

  Doomsday in Racket/Gamble 

  http://www.homoexcelsior.com/archive/transhuman/msg03904.html
  """
  Imagine that two big urns are put in front of you, and you know that 
  one of them contains ten balls and the other a million, but you are 
  ignorant as to which is which. You know the balls in each urn are 
  numbered 1, 2, 3, 4 ... etc. Now you take a ball at random from the 
  left urn, and it is number 7. Clearly, this is a strong indication 
  that that urn contains only ten balls. If originally the odds were 
  fifty-fifty, a swift application of Bayes' theorem gives you the 
  posterior probability that the left urn is the one with only ten 
  balls. (Pposterior (L=10) = 0.999990).

  But now consider the case where 
  instead of the urns you have two possible human races, and instead of 
  balls you have individuals, ranked according to birth order. As a 
  matter of fact, you happen to find that your rank is about sixty 
  billion. Now, say Carter and Leslie, we should reason in the same way 
  as we did with the urns. That you should have a rank of sixty billion 
  or so is much more likely if only 100 billion persons will ever have 
  lived than if there will be many trillion persons. Therefore, by 
  Bayes' theorem, you should update your beliefs about mankind's 
  prospects and realise that an impending doomsday is much more probable 
  than you have hitherto thought."
  """
  
  (ball_obs 7 num_balls1: 10 num_balls2: 1000000)
  var : urn
  urn1: 100000/100001 (0.999990000099999)
  urn2: 1/100001 (9.99990000099999e-6)


  (ball_obs 8 num_balls1: 10 num_balls2: 1000000)
  var : urn
  urn1: 100000/100001 (0.999990000099999)
  urn2: 1/100001 (9.99990000099999e-6)


  (ball_obs 9 num_balls1: 10 num_balls2: 1000000)
  var : urn
  urn1: 100000/100001 (0.999990000099999)
  urn2: 1/100001 (9.99990000099999e-6)


  (ball_obs 10 num_balls1: 10 num_balls2: 1000000)
  var : urn
  urn1: 100000/100001 (0.999990000099999)
  urn2: 1/100001 (9.99990000099999e-6)


  (ball_obs 11 num_balls1: 10 num_balls2: 1000000)
  var : urn
  urn2: 1 (1.0)
 

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model ball_obs num_balls1 num_balls2)
  (newline)
  (show2 "ball_obs" ball_obs  "num_balls1:" num_balls1  "num_balls2:" num_balls2)
  (enumerate

   (define urn (categorical-vw2 (vector 1/2 1/2) (vector "urn1" "urn2")))
   (define ball (if (eq? urn "urn1")
                    (add1 (random-integer num_balls1))
                    (add1 (random-integer num_balls2))))
        
   (observe/fail (= ball ball_obs))
   
   (list urn)

   )
)

(define num-balls1 10)
(define num-balls2 (expt 10 6))
(for ([ball-obs (range 7 12)])
  (show-marginals (model ball-obs num-balls1 num-balls2)
                  (list  "urn"))
  )


