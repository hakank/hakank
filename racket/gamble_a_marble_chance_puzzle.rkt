#| 

  A Marble Chance Puzzle in Racket/Gamble 

  From Cole Frederick: "A Marble Chance Puzzle - A simple test of logic"
  https://colefp.medium.com/a-marble-chance-puzzle-c93e3224deb4
  """
  You have two boxes. One box has a blue marble, and the other box has one blue 
  marble and one black marble. You cannot see into either box. You simultaneously 
  reach into each box, grab one marble from each, then switch them quickly without 
  looking. You then randomly pick a box, and then randomly pick a marble 
  from that box. What is the probability you pick a blue marble?
  """

  variable : pick-from-A
  blue: 1 (1.0)

  variable : pick-from-B
  blue: 1/2 (0.5)
  black: 1/2 (0.5)

  variable : boxA_2
  (blue): 1/2 (0.5)
  (black): 1/2 (0.5)

  variable : boxB_2
  (black blue): 1/2 (0.5)
  (blue blue): 1/2 (0.5)

  variable : pick-box
  (black blue): 1/4 (0.25)
  (blue): 1/4 (0.25)
  (blue blue): 1/4 (0.25)
  (black): 1/4 (0.25)

  variable : pick
  blue: 5/8 (0.625)
  black: 3/8 (0.375)

  variable : p
  #t: 5/8 (0.625)
  #f: 3/8 (0.375)
  mean: 5/8 (0.625)

  variable : scenario-A
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  variable : scenario-B
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   
   ; First step
   (define boxA_1 '("blue"))
   (define boxB_1 '("blue" "black"))   

   ; Pick one marble from each box 
   (define pick-from-A (uniform-draw boxA_1 ))
   (define pick-from-B (uniform-draw boxB_1 ))

   ; And move it to the other box
   (define boxA_2 (append (remove pick-from-A boxA_1) (list pick-from-B)))
   (define boxB_2 (append (remove pick-from-B boxB_1) (list pick-from-A)))

   ; Pick a box at random
   (define pick-box (uniform-draw (list boxA_2 boxB_2)))
   ; And draw a marble at random
   (define pick (uniform-draw pick-box))

   ; Probability of each outcome:
   ; 1:  A: blue  B: black blue
   ; 2:  A: black B: blue blue
   (define scenario-A (and (equal? boxA_2 '("blue")) (equal? boxB_2 '("black" "blue"))))
   (define scenario-B (and (equal? boxA_2 '("black")) (equal? boxB_2 '("blue" "blue"))))    

   (show2 "boxA_2 is blue" (equal? boxA_2 '("blue")))
   
   ; What is the probability that the drawn marble is blue.
   (define p (eq? pick "blue"))

   ; (show2 "pick-from-A" pick-from-A "pick-from-B" pick-from-B "boxA_2" boxA_2 "boxB_2" boxB_2 "pick-box" pick-box "picḱ" pick "p" p)
   
   (list boxA_1 boxB_1 pick-from-A pick-from-B boxA_2 boxB_2 pick-box pick p scenario-A scenario-B)
   
   )
)

(show-marginals (model)
                (list  "boxA_1" "boxB_1"
                       "pick-from-A"
                       "pick-from-B"
                       "boxA_2"
                       "boxB_2"
                       "pick-box"
                       "pick"
                       "p"
                       "scenario-A"
                       "scenario-B"
                     )
                    )


#|
  "Logical" approach using the two possible outcome after the switch:
  -  Outcome 1: A contains "blue" B contains "blue" and "black"
  -  Ourcome 2: A contains "black" B contains "blue" and "blue"


  Model 2
  variable : p1
  #<discrete-dist: [0 1/4] [1 3/4]>: 1 (1.0)

  variable : p2
  #<discrete-dist: [0 1/2] [1 1/2]>: 1 (1.0)

  variable : p
  1/2: 1/2 (0.5)
  1: 3/8 (0.375)
  0: 1/8 (0.125)
  mean: 5/8 (0.625)

|#
(define (model2)
  (enumerate

   ; Sub model
   (define (f A B)
     (enumerate
      
      ; Pick a box at random
      (define pick-box (uniform-draw (list A B)))
      ; And draw a marble at random
      (define pick (uniform-draw pick-box))
      
      ; What is the probability  that the drawn marble is blue.
      ; Note: It's easier to use numeric values 
      (if (eq? pick "blue") 1 0)
      
      ))

   (define p1 (f '("blue") '("blue" "black")))
   (define p2 (f '("black") '("blue" "blue")))

   ; Combine the probabilities
   (define p (mean (list (sample p1) (sample p2))))
   (list p1 p2 p)
   
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "p1"
                       "p2"
                       "p"
                     )
                    )


