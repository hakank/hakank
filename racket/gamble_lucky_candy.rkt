#| 

  Lucky candy in Racket Gamble.

  From https://brainstellar.com/puzzles/probability/5
  """
  Lucky Candy

  How do you place 50 good candies and 50 rotten candies in two boxes such that if 
  you choose a box at random and take out a candy at random, it better be good!

  We need to maximize the probability of getting a good candy when selecting a random 
  box and a random candy from it.

  ...

  Answer: 
  Place 1 good candy in one box and all the remaining (49 good and 50 rotten candies) in the second box.

  Solution:
  Place 1 good candy in one box and all remaining (49 good and 50 rotten candies) in the second box.
    (1/2)*1 + (1/2)*(49/99)=74.74%
  """


  * (lucky-candy) 
    This is not a good model!

    This was my first stab at this problem. However the model seems to imply that we have a couple of optimal 
    solutions, but that makes no sense:

    ((box1-total 1 box2-total 9 box1-good 1 box1-bad 0 box2-good 4 box2-bad 5 box box1 pick-one good p #t) : 1/20 (0.05))
    ((box1-total 2 box2-total 8 box1-good 2 box1-bad 0 box2-good 3 box2-bad 5 box box1 pick-one good p #t) : 1/20 (0.05))
    ((box1-total 3 box2-total 7 box1-good 3 box1-bad 0 box2-good 2 box2-bad 5 box box1 pick-one good p #t) : 1/20 (0.05))
    ((box1-total 4 box2-total 6 box1-good 4 box1-bad 0 box2-good 1 box2-bad 5 box box1 pick-one good p #t) : 1/20 (0.05))
    ((box1-total 5 box2-total 5 box1-good 0 box1-bad 5 box2-good 5 box2-bad 0 box box2 pick-one good p #t) : 1/20 (0.05))
    ((box1-total 5 box2-total 5 box1-good 5 box1-bad 0 box2-good 0 box2-bad 5 box box1 pick-one good p #t) : 1/20 (0.05))

    So, back to the drawing board. 
    <Time passes>  
    OK, we have to test each distribution of the number of good and bad in each box for this. 
    See lucky-candy-test. 

  * lucky-candy-test

    Here we explicitly check all combinations of 5 good/5 bad candies and see which has the optimal number of
    successes. This simplification is quite faster than using 50/50 candies: 2.8s vs 18.8s (using enumerate).

    Below are all experiments. The two optimal one are
      - obs-num1-good: 1 obs-num1-bad: 0
      - obs-num1-good: 4 obs-num1-bad: 5  (i.e. box 2 has 1 good and 0 bad)
    which yields a probability 13/18 (0.7222222222222222) to get a good candy.
    All other combinations of good/bad candies in box1 are worse.

    The corresponding probabilities using the  equation above: 
       (1/2)*1+(1/2)*(4/9) = ~0.72222222222222222222
    which seems to confirm that the model is correct...

    Fhe full test with 50/50 candies give the following probabilities (it takes about 18.8s)
       #t: 74/99 (0.7474747474747475)
       #f: 25/99 (0.25252525252525254)
       mean: 74/99 (0.7474747474747475)
    which is the same as the answer in the problem statement above.

    Note: The first and last solutions include 0 probability for box1 and box2, respectively. 
    I _think_ my fix returning #f in the box check works.
    

obs-num1-good: 0 obs-num1-bad: 0
var : p
#f: 3/4 (0.75)
#t: 1/4 (0.25)
mean: 1/4 (0.25)

obs-num1-good: 1 obs-num1-bad: 0
var : p
#t: 13/18 (0.7222222222222222)
#f: 5/18 (0.2777777777777778)
mean: 13/18 (0.7222222222222222)


obs-num1-good: 2 obs-num1-bad: 0
var : p
#t: 11/16 (0.6875)
#f: 5/16 (0.3125)
mean: 11/16 (0.6875)


obs-num1-good: 3 obs-num1-bad: 0
var : p
#t: 9/14 (0.6428571428571429)
#f: 5/14 (0.35714285714285715)
mean: 9/14 (0.6428571428571429)


obs-num1-good: 4 obs-num1-bad: 0
var : p
#t: 7/12 (0.5833333333333334)
#f: 5/12 (0.4166666666666667)
mean: 7/12 (0.5833333333333334)


obs-num1-good: 5 obs-num1-bad: 0
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 0 obs-num1-bad: 1
var : p
#f: 13/18 (0.7222222222222222)
#t: 5/18 (0.2777777777777778)
mean: 5/18 (0.2777777777777778)


obs-num1-good: 1 obs-num1-bad: 1
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 2 obs-num1-bad: 1
var : p
#t: 23/42 (0.5476190476190477)
#f: 19/42 (0.4523809523809524)
mean: 23/42 (0.5476190476190477)


obs-num1-good: 3 obs-num1-bad: 1
var : p
#t: 13/24 (0.5416666666666666)
#f: 11/24 (0.4583333333333333)
mean: 13/24 (0.5416666666666666)


obs-num1-good: 4 obs-num1-bad: 1
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 5 obs-num1-bad: 1
var : p
#f: 7/12 (0.5833333333333334)
#t: 5/12 (0.4166666666666667)
mean: 5/12 (0.4166666666666667)


obs-num1-good: 0 obs-num1-bad: 2
var : p
#f: 11/16 (0.6875)
#t: 5/16 (0.3125)
mean: 5/16 (0.3125)


obs-num1-good: 1 obs-num1-bad: 2
var : p
#f: 23/42 (0.5476190476190477)
#t: 19/42 (0.4523809523809524)
mean: 19/42 (0.4523809523809524)


obs-num1-good: 2 obs-num1-bad: 2
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 3 obs-num1-bad: 2
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 4 obs-num1-bad: 2
var : p
#f: 13/24 (0.5416666666666666)
#t: 11/24 (0.4583333333333333)
mean: 11/24 (0.4583333333333333)


obs-num1-good: 5 obs-num1-bad: 2
var : p
#f: 9/14 (0.6428571428571429)
#t: 5/14 (0.35714285714285715)
mean: 5/14 (0.35714285714285715)


obs-num1-good: 0 obs-num1-bad: 3
var : p
#f: 9/14 (0.6428571428571429)
#t: 5/14 (0.35714285714285715)
mean: 5/14 (0.35714285714285715)


obs-num1-good: 1 obs-num1-bad: 3
var : p
#f: 13/24 (0.5416666666666666)
#t: 11/24 (0.4583333333333333)
mean: 11/24 (0.4583333333333333)


obs-num1-good: 2 obs-num1-bad: 3
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 3 obs-num1-bad: 3
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 4 obs-num1-bad: 3
var : p
#f: 23/42 (0.5476190476190477)
#t: 19/42 (0.4523809523809524)
mean: 19/42 (0.4523809523809524)


obs-num1-good: 5 obs-num1-bad: 3
var : p
#f: 11/16 (0.6875)
#t: 5/16 (0.3125)
mean: 5/16 (0.3125)


obs-num1-good: 0 obs-num1-bad: 4
var : p
#f: 7/12 (0.5833333333333334)
#t: 5/12 (0.4166666666666667)
mean: 5/12 (0.4166666666666667)


obs-num1-good: 1 obs-num1-bad: 4
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 2 obs-num1-bad: 4
var : p
#t: 13/24 (0.5416666666666666)
#f: 11/24 (0.4583333333333333)
mean: 13/24 (0.5416666666666666)


obs-num1-good: 3 obs-num1-bad: 4
var : p
#t: 23/42 (0.5476190476190477)
#f: 19/42 (0.4523809523809524)
mean: 23/42 (0.5476190476190477)


obs-num1-good: 4 obs-num1-bad: 4
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 5 obs-num1-bad: 4
var : p
#f: 13/18 (0.7222222222222222)
#t: 5/18 (0.2777777777777778)
mean: 5/18 (0.2777777777777778)


obs-num1-good: 0 obs-num1-bad: 5
var : p
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)


obs-num1-good: 1 obs-num1-bad: 5
var : p
#t: 7/12 (0.5833333333333334)
#f: 5/12 (0.4166666666666667)
mean: 7/12 (0.5833333333333334)


obs-num1-good: 2 obs-num1-bad: 5
var : p
#t: 9/14 (0.6428571428571429)
#f: 5/14 (0.35714285714285715)
mean: 9/14 (0.6428571428571429)


obs-num1-good: 3 obs-num1-bad: 5
var : p
#t: 11/16 (0.6875)
#f: 5/16 (0.3125)
mean: 11/16 (0.6875)


obs-num1-good: 4 obs-num1-bad: 5
var : p
#t: 13/18 (0.7222222222222222)
#f: 5/18 (0.2777777777777778)
mean: 13/18 (0.7222222222222222)

obs-num1-good: 5 obs-num1-bad: 5
var : p
#f: 3/4 (0.75)
#t: 1/4 (0.25)
mean: 1/4 (0.25)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")


(define (lucky-candy model-type)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define num-good 5)
   (define num-bad 5)
   
   (define box1-good (random-integer (add1 num-good)))
   (define box1-bad (random-integer (add1 num-bad)))
   (define box2-good (random-integer (add1 num-good)))
   (define box2-bad (random-integer (add1 num-bad)))

   (define box1-total (+ box1-good box1-bad))
   (define box2-total (+ box2-good box2-bad))

   ; To simplify: at least one must be in each box. TODO: Fix this
   (observe/fail (> box1-total 0))
   (observe/fail (> box2-total 0))   

   ; Symmetry: There must be more or equal number of candies in box2 than box1
   (observe/fail (<= box1-total box2-total))      
   
   (observe/fail (= (+ box1-good box2-good) num-good))
   (observe/fail (= (+ box1-bad box2-bad) num-bad))   
   
   ;; TESTING
   ;; (observe/fail (= box1-good 1))
   ;; (observe/fail (= box1-bad 0))
   
   ;; (observe/fail (= box1-good 1))
   ;; (observe/fail (= box1-bad 0))
   
   
   (define box (uniform-draw '("box1" "box2")))
   
   (define pick-one
     (case box
       [("box1") (categorical-vw2 (vector box1-good box1-bad) (vector "good" "bad"))]
       [("box2") (categorical-vw2 (vector box2-good box2-bad) (vector "good" "bad"))]
       )
     )

   (define p (eq? pick-one "good"))

   ; (observe/fail (eq? box "box1")) ; WLOG: We pick box 1
   
   (observe/fail (eq? pick-one "good")) ; We draw a good one

   (if (eq? model-type "show-model")
       (list "box1-total" box1-total "box2-total" box2-total
             "box1-good" box1-good "box1-bad" box1-bad 
             "box2-good" box2-good "box2-bad" box2-bad 
             "box" box "pick-one" pick-one "p" p)
       (list box1-total box2-total
             box1-good box1-bad 
             box2-good box2-bad 
             pick-one p)
               )

   )
  
  )

; (show-marginals) is not helpful without observations on the number of god/bad in each box.
; See lucky-candy-test below instead.
; (show-marginals (lucky-candy "show-marginals") (list "box1-good" "box1-bad" "box2 good" "box2-bad" "box" "pick-one" "p"))
; This is not so helpful since it does not discriminate the optimal solution!
; (show-model (lucky-candy "show-model") #:num-samples 10000 #:no-stats? #t #:no-cred? #t)


;;;
;;; Instead do a systematic testing of checking all combinations of number of good and bad in box1.
;;: Note that we (WLOG) only use 5, not 50, good and bad candies to speed things up.
;;;
(define (lucky-candy-test model-type obs-box1-good obs-box1-bad)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define num-good 5)
   (define num-bad 5)
   
   (define box1-good (random-integer (add1 num-good)))
   (define box1-bad (random-integer (add1 num-bad)))
   (define box2-good (random-integer (add1 num-good)))
   (define box2-bad (random-integer (add1 num-bad)))

   (define box1-total (+ box1-good box1-bad))
   (define box2-total (+ box2-good box2-bad))

   
   (observe/fail (= (+ box1-good box2-good) num-good))
   (observe/fail (= (+ box1-bad box2-bad) num-bad))   
  
   ;; TESTING   
   (observe/fail (= box1-good obs-box1-good))
   (observe/fail (= box1-bad obs-box1-bad))

  
   (define box (uniform-draw '("box1" "box2")))

   (define pick-one
     (case box
       ; Nice: the probabilities does not need to sum to 1.
       ; But categorical-vw2 (i.e. make-discrete-dist*) does not like 0 probability, and
       ; throws an exception (-> no solution)
       ; [("box1") (categorical-vw2 (vector box1-good box1-bad) (vector "good" "bad"))]
       ; [("box2") (categorical-vw2 (vector box2-good box2-bad) (vector "good" "bad"))]
       
       ; Fixing the 0 probability issues (I hope). It seems to work by returning #f for
       ; the two 0 probability cases whhen either of the two boxes are empty.
       [("box1") (if (= box1-total 0)
                     #f
                     (categorical-vw2 (vector box1-good box1-bad) (vector "good" "bad")))]
       [("box2") (if (= box2-total 0)
                     #f
                     (categorical-vw2 (vector box2-good box2-bad) (vector "good" "bad")))]
       )
     )
  
   (define p (eq? pick-one "good"))

   ; Note: Neither of these can be active since it defies the purpose of the test.
   ; (observe/fail (eq? box "box1")) 
   ; (observe/fail (eq? pick-one "good")) ; We draw a good one

   
   ;; For show-model
   (if (eq? model-type "show-model")
       ; For show-model
       (list ;"box1-total" box1-total "box2-total" box2-total
        ; "box1-good" box1-good "box1-bad" box1-bad 
        ; "box2-good" box2-good "box2-bad" box2-bad 
        "box" box
        "pick-one" pick-one
        "p" p)
       
       ;; For show-marginals
       (list p))
   
   )
  )


;; The full test 50/50 on the optimal strategy
;; (show-marginals (lucky-candy-test "show-marginals" 1 0) (list "p"))

; Zero probability -> "discrete-dist: improper weights"
; (show-marginals (lucky-candy-test "show-marginals" 0 0) (list "p"))

(for* ([obs-num1-bad (range 0 6)]
       [obs-num1-good (range 0 6)])
  ; (displayln (list "obs-num1-good" obs-num1-good "obs-num1-bad" obs-num1-bad))
    (displayln (format "\nobs-num1-good: ~a obs-num1-bad: ~a" obs-num1-good obs-num1-bad))
    (with-handlers ([exn:fail? (lambda (exn) (displayln "no path"))])
      ; (show-model (lucky-candy-test "show-model" obs-num1-good obs-num1-bad) #:num-samples 10000 #:no-stats? #t #:no-cred? #t)
      (show-marginals (lucky-candy-test "show-marginals" obs-num1-good obs-num1-bad) (list "p"))
     )  
  )
