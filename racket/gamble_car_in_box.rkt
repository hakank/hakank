#| 

  Car in box in Racket/Gamble 

  From https://medium.com/puzzle-sphere/can-you-find-the-right-box-d01521f17015
  """
  There are 3 boxes, exactly one of which has a car. You can keep the car 
  if you pick the correct box!

  On each box, there is a statement, exactly one of which is true.

  - Box 1: The car is in this box.
  - Box 2: The car is not in this box.
  - Box 3: The car is not in box 1.

  Which box has the car?
  """

  variable : car
  2: 1 (1.0)
  mean: 2 (2.0)

  variable : boxes
  (0 0 1): 1 (1.0)

  I.e. the car is in box 2 and the statement on box 3 is the true one.

  Cf my Picat model http://hakank.org/picat/car_in_box.pi

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
   
   (define n 3)
   (define car (add1 (random-integer n)))
   (define boxes (for/list ([i n]) (uniform-draw '(0 1))))

   ; Exactly one statement of the boxes is true
   (observe/fail (= (sum boxes) 1))

   #|
   ; Box 1: The car is in this box.
   (equivalence (lambda () (= (first boxes) 1)) (lambda () (= car 1)))

   ; Box 2: The car is not in this box.
   (equivalence (lambda () (= (second boxes) 1)) (lambda () (not (= car 2))))
  
   ; Box 3: The car is not in box 1.
   (equivalence (lambda () (= (third boxes) 1)) (lambda () (not (= car 1))))
   |#

   ; This works as well and is simpler

   ; Box 1: The car is in this box.
   (equivalence (= (first boxes) 1) (= car 1))

   ; Box 2: The car is not in this box.
   (equivalence (= (second boxes) 1) (not (= car 2)))
  
   ; Box 3: The car is not in box 1.
   (equivalence (= (third boxes) 1) (not (= car 1)))

   
   (list car
         boxes
         )
   

   )
)

(show-marginals (model)
                (list  "car" "boxes"))


