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

  This is another take on this problem (compared to gamble_lucky_candy.rkt). It's a neater way 
  of getting the optimal strategy (strategies). Though it's still brute force, based on the
  lucky-candy-test from gamble_lucky_candy.rkt, and still use 5 (not 100) candies for speed.

  Here's the sorted list of the probability of success for combinations of different values 
  for num-good and num-good in 0..5:

  (0 1 13/18 0.7222222222222222)
  (5 4 13/18 0.7222222222222222)
  (0 2 11/16 0.6875)
  (5 3 11/16 0.6875)
  (0 3 9/14 0.6428571428571429)
  (5 2 9/14 0.6428571428571429)
  (0 4 7/12 0.5833333333333334)
  (5 1 7/12 0.5833333333333334)
  (1 2 23/42 0.5476190476190477)
  (4 3 23/42 0.5476190476190477)
  (1 3 13/24 0.5416666666666666)
  (4 2 13/24 0.5416666666666666)
  (0 5 1/2 0.5)
  (1 1 1/2 0.5)
  (1 4 1/2 0.5)
  (2 2 1/2 0.5)
  (2 3 1/2 0.5)
  (3 2 1/2 0.5)
  (3 3 1/2 0.5)
  (4 1 1/2 0.5)
  (4 4 1/2 0.5)
  (5 0 1/2 0.5)
  (2 4 11/24 0.4583333333333333)
  (3 1 11/24 0.4583333333333333)
  (2 1 19/42 0.4523809523809524)
  (3 4 19/42 0.4523809523809524)
  (1 5 5/12 0.4166666666666667)
  (4 0 5/12 0.4166666666666667)
  (2 5 5/14 0.35714285714285715)
  (3 0 5/14 0.35714285714285715)
  (2 0 5/16 0.3125)
  (3 5 5/16 0.3125)
  (1 0 5/18 0.2777777777777778)
  (4 5 5/18 0.2777777777777778)
  (0 0 1/4 0.25)
  (5 5 1/4 0.25)

  Here we see that the strategies of 1 good and 0 bad (or 5 good and 4 bad) give the best strategy.
  
  Below is a list of the probabilities for the optimal strategies for 0..50 candies:

  (n 1 res 1/2 0.5)
  (n 2 res 2/3 0.6666666666666666)
  (n 3 res 7/10 0.7)
  (n 4 res 5/7 0.7142857142857143)
  (n 5 res 13/18 0.7222222222222222)
  (n 6 res 8/11 0.7272727272727273)
  (n 7 res 19/26 0.7307692307692307)
  (n 8 res 11/15 0.7333333333333333)
  (n 9 res 25/34 0.7352941176470589)
  (n 10 res 14/19 0.7368421052631579)
  (n 11 res 31/42 0.7380952380952381)
  (n 12 res 17/23 0.7391304347826086)
  (n 13 res 37/50 0.74)
  (n 14 res 20/27 0.7407407407407407)
  (n 15 res 43/58 0.7413793103448276)
  (n 16 res 23/31 0.7419354838709677)
  (n 17 res 49/66 0.7424242424242424)
  (n 18 res 26/35 0.7428571428571429)
  (n 19 res 55/74 0.7432432432432432)
  (n 20 res 29/39 0.7435897435897436)
  (n 21 res 61/82 0.7439024390243902)
  (n 22 res 32/43 0.7441860465116279)
  (n 23 res 67/90 0.7444444444444445)
  (n 24 res 35/47 0.7446808510638298)
  (n 25 res 73/98 0.7448979591836735)
  (n 26 res 38/51 0.7450980392156863)
  (n 27 res 79/106 0.7452830188679245)
  (n 28 res 41/55 0.7454545454545455)
  (n 29 res 85/114 0.7456140350877193)
  (n 30 res 44/59 0.7457627118644068)
  (n 31 res 91/122 0.7459016393442623)
  (n 32 res 47/63 0.746031746031746)
  (n 33 res 97/130 0.7461538461538462)
  (n 34 res 50/67 0.746268656716418)
  (n 35 res 103/138 0.7463768115942029)
  (n 36 res 53/71 0.7464788732394366)
  (n 37 res 109/146 0.7465753424657534)
  (n 38 res 56/75 0.7466666666666667)
  (n 39 res 115/154 0.7467532467532467)
  (n 40 res 59/79 0.7468354430379747)
  (n 41 res 121/162 0.7469135802469136)
  (n 42 res 62/83 0.7469879518072289)
  (n 43 res 127/170 0.7470588235294118)
  (n 44 res 65/87 0.7471264367816092)
  (n 45 res 133/178 0.7471910112359551)
  (n 46 res 68/91 0.7472527472527473)
  (n 47 res 139/186 0.7473118279569892)
  (n 48 res 71/95 0.7473684210526316)
  (n 49 res 145/194 0.7474226804123711)
  (n 50 res 74/99 0.7474747474747475)



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")


;;;
;;; Instead do a systematic testing of checking all combinations of number of good and bad in box1.
;;: Note that we (WLOG) only use 5, not 50, good and bad candies to speed things up.
;;;
(define (lucky-candy num-good num-bad obs-box1-good obs-box1-bad)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define box1-good (random-integer (add1 num-good)))
   (define box1-bad  (random-integer (add1 num-bad)))
   (define box2-good (random-integer (add1 num-good)))
   (define box2-bad  (random-integer (add1 num-bad)))

   (define box1-total (+ box1-good box1-bad))
   (define box2-total (+ box2-good box2-bad))

   
   (observe/fail (= (+ box1-good box2-good) num-good))
   (observe/fail (= (+ box1-bad box2-bad) num-bad))   
  
   ; observe the input data
   (observe/fail (= box1-good obs-box1-good))
   (observe/fail (= box1-bad obs-box1-bad))

  
   (define box (uniform-draw '("box1" "box2")))

   (define candies (vector "good" "bad"))
   
   (define pick-one
     (case box
       [("box1")  (if (= box1-total 0)
                     #f
                     (categorical-vw2 (vector box1-good box1-bad) candies))]
       [("box2") (if (= box2-total 0)
                     #f
                     (categorical-vw2 (vector box2-good box2-bad) candies))]
       )
     )
  
   (define p (eq? pick-one "good"))

   ; Return p as integer
   ; (boolean->integer p)
   ; (list "p" p)
   p
   
   )
  )


(define (get-true d)
  (let* ([dist (if (discrete-dist? d) d (sampler->discrete-dist d 10))]
         [ds (discrete-dist->lists dist)]
         [ts (first ds)]
         [vs (second ds)])
    (if (equal? ts '(#f #t))
        (second vs)
        (first vs))))


(define (run max-val [debug? #f])
  (sort
   (for*/list ([obs-num1-bad (range 0 (add1 max-val))]
               [obs-num1-good (range 0 (add1 max-val))])
     (with-handlers ([exn:fail?
                      (lambda (exn)
                        (displayln (format "no path bad: ~a good ~a" obs-num1-bad obs-num1-good)))])
       (let* ([res (lucky-candy max-val max-val obs-num1-good obs-num1-bad)]
              [p (get-true res)])
         (when debug? 
           (show2 "obs-num1-good" obs-num1-good "obs-num1-bad" obs-num1-bad "res" res "p" p)
           (flush-output))
         (list obs-num1-good obs-num1-bad p (* 1.0 p) )
         
         ))
     )
   > #:key third)
  )

(for ([r (run 5)])
  (displayln r)
  )
(newline)

(displayln "Probabiliy for optmal strategy for some n:")
(for* ([n (range 1 51)])
  (let ([res (get-true (lucky-candy n n 1 0))])
    (show2 "n" n "res" res (* 1.0 res))
    (flush-output)
    ))

(displayln "\n50 candies:")
(time (get-true (lucky-candy 50 50 1 0)))
