#| 

  Game show problem in Racket Gamble.

  From https://www.youtube.com/watch?v=WWAoh3XfWzA
  "A fun game show probability problem | Bonus cash stop riddle"
  """
  A fun probability problem for you! You've just won a game show and it's time to 
  play the bonus round for some extra cash! You play a random game where you open 
  up boxes and get the prize money inside until you open up a stop sign. How much 
  do you expect to make from playing this game?
  """

  There are 5 boxes with the following:
  - 1 * $10 
  - 2 * $1000
  - 1 * $10000
  - stop sign  

  You can draw until the stop sign and then you earn all the money 
  you picked. There is no penalty to draw until stop sign.

  Answer: Estimated value is $6005
  Calculated by:
     10*0.5 + 1000*0.5 + 1000*0.5 + 10000*0.5 = 6005
  where 0.5 is the probability that the value is to the left 
  of the stop sign, i.e. is selected.


  (boxes (10 1000 1000 10000 0) theoretical 6005)
  var : total
  0: 1/5 (0.2)
  12010: 1/5 (0.2)
  11010: 1/10 (0.1)
  1000: 1/10 (0.1)
  1010: 1/15 (0.06666666666666667)
  11000: 1/15 (0.06666666666666667)
  10000: 1/20 (0.05)
  10: 1/20 (0.05)
  2010: 1/20 (0.05)
  12000: 1/20 (0.05)
  2000: 1/30 (0.03333333333333333)
  10010: 1/30 (0.03333333333333333)
  mean: 6005 (6005.0)

  (boxes (10 1000 1000 10000 12345 0) theoretical 24355/2)
  var : total
  0: 1/6 (0.16666666666666666)
  24355: 1/6 (0.16666666666666666)
  23355: 1/15 (0.06666666666666667)
  1000: 1/15 (0.06666666666666667)
  12010: 1/30 (0.03333333333333333)
  24345: 1/30 (0.03333333333333333)
  11010: 1/30 (0.03333333333333333)
  12345: 1/30 (0.03333333333333333)
  23345: 1/30 (0.03333333333333333)
  10: 1/30 (0.03333333333333333)
  14355: 1/30 (0.03333333333333333)
  1010: 1/30 (0.03333333333333333)
  11000: 1/30 (0.03333333333333333)
  13345: 1/30 (0.03333333333333333)
  10000: 1/30 (0.03333333333333333)
  13355: 1/30 (0.03333333333333333)
  22355: 1/60 (0.016666666666666666)
  12000: 1/60 (0.016666666666666666)
  14345: 1/60 (0.016666666666666666)
  12355: 1/60 (0.016666666666666666)
  2000: 1/60 (0.016666666666666666)
  10010: 1/60 (0.016666666666666666)
  2010: 1/60 (0.016666666666666666)
  22345: 1/60 (0.016666666666666666)
  mean: 24355/2 (12177.5)


  This is a port of my WebPPL model game_show_problem.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (game_show_problem_theoretical boxes)
    (/ (sum boxes) 2)
)


(define (model boxes)
  (show2 "boxes" boxes "theoretical" (game_show_problem_theoretical boxes))
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   ; (define boxes '(10 1000 1000 10000 0))
   (define n (length boxes))
   
   ;; Pick a box and continue until the stop box (value = 0) is picked
   ;; Note: Since we have duplicate values we use boxes indices
   ;; instead of the values which makes it slightly more elaborate
   (define (pick_a_box boxes_ix_left val_so_far)
     (define len (length boxes_ix_left))
     (if (<= len 1)
         val_so_far
         (let* ([box_ix (random-integer len)] ;; Pick an _index_ 
                [val (list-ref boxes (list-ref boxes_ix_left box_ix))])  ;; Lookup the value of this selected index    
           ;; Stop if stop box is selected or just the stop box is left
           ;; (If the length is 1 then it must be the stop box)
           (if (= val 0)
               val_so_far
               (let ([new_boxes_ix (remove (list-ref boxes_ix_left box_ix) boxes_ix_left )])
                 (pick_a_box new_boxes_ix (+ val_so_far val))
                 )
               )
           )
         )     
     )
        
   (define total (pick_a_box (range n)  0))
        
   (list total
        )

   )
  )

(for ([boxes '( (10 1000 1000 10000 0)
                (10 1000 1000 10000 12345 0))])
  (show-marginals (model boxes)
                  (list  "total"
                         )
                  #:num-samples 1000
                  ; #:truncate-output 3
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                ; #:show-percentiles? #t
                  )
  )




