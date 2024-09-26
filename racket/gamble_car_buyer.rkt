#| 

  Car buyer in Racket.Gamble 

  From the Netica model Car_Buyer Neapolital.dne
  """
  This decision network is from Neapolitan90 (p.373), which is a simpler version of the car 
  buyer example of Howard62.  Eliminating the rationale given there of how the numbers are 
  arrived at, we have the following story:

  Joe is going to buy a used car, which could be good with probability 0.8 or a lemon with 
  probability 0.2.  After accounting for repairs, Joe's profit will be $60 if the car is 
  good, and $-100 if it is bad.  Before buying the car he has the option of having one test 
  or two tests done on it.  The first test costs $9, and both together cost $13.  The first 
  test has a 90% chance of returning positive if the car is good, and a 40% chance if it's 
  a lemon.  If the first test returns positive, then the second test has a 88.89% chance of 
  returning positive if the car is good, and a 33.33% chance if it's a lemon.  If the first 
  test returns negative, then the second test has a 100% chance of returning positive if 
  the car is good, and a 44.44% chance if it's a lemon.

  Joe has 2 decisions to make: whether to do the tests, and whether to buy the car.  These 
  are represented by the "Do Tests?" and "Buy It?" decision nodes.  The outcome of the tests 
  are given by nodes "First Test" and "Second Test".  The costs of the tests are represented 
  by utility node U, and the profits after repairs (not including test costs) by utility 
  node V.

  When Joe decides whether to do the tests, he doesn't know the value of any of these 
  variables, so there are no links entering the "Do Tests?" node.  When he decides whether 
  to buy, he will know the outcome of both tests (the outcomes may be "not done"), and so 
  there are links from those two nodes to "Buy It?".  He will also know the value of 
  "Do Tests?" since he has already made that decision, so you could put a link from that 
  node to "Buy It?", but it is not necessary since it is a no-forgetting link and there is 
  already a directed path from "Do Tests?" to "Buy It?".
  """

var : condition
lemon: 0.5999999999999999
good: 0.4

var : do_tests
both: 0.9999999999999999

var : first_test
negative: 0.9999999999999999

var : second_test
notDone: 0.4
negative: 0.3333335999999999
positive: 0.26666639999999997

var : buy_it
dont_buy: 0.5999999999999999
buy: 0.4

var : v
0: 0.5999999999999999
60: 0.4
mean: 24.0

var : u
-13: 0.9999999999999999
mean: -12.999999999999998

var : comb
(both negative notDone buy 60 -13 47): 0.4
(both negative negative dont_buy 0 -13 -13): 0.3333335999999999
(both negative positive dont_buy 0 -13 -13): 0.26666639999999997

  This is a port of my WebPPL model car_buyer.wppl

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
   
   (define conditions (vector "good"  "lemon"))
   (define doTests    (vector "none"  "first"  "both"))
   (define tests      (vector "notDone"  "positive"  "negative"))
   
   (define conditionx (categorical-vw2 (vector 80 20) (vector "good" "lemon")))
   (define do_tests   (categorical-vw2 (vector 28 26.2 22.7333) doTests))

   (define first_test
     (cond
       [(and (eq? conditionx "good") (eq? do_tests "none"))  (categorical-vw2 (vector 100 0 0) tests)]
        [(and (eq? conditionx "good") (eq? do_tests "first")) (categorical-vw2 (vector 0 90 10) tests)]
        [(and (eq? conditionx "good") (eq? do_tests "both"))  (categorical-vw2 (vector 0 90 10) tests)]
        [(and (eq? conditionx "lemon") (eq? do_tests "none")) (categorical-vw2 (vector 100 0 0) tests)]
        [(and (eq? conditionx "lemon") (eq? do_tests "first")) (categorical-vw2 (vector 0 40 60) tests)]
        [(and (eq? conditionx "lemon") (eq? do_tests "both"))  (categorical-vw2 (vector 0 40 60) tests)]
        [else "first_test_xxx"]))
   
   (define second_test
     (cond
       [(and (eq? first_test "notDone") (eq? conditionx "good") (eq? do_tests "none")) (categorical-vw2 (vector 100  0  0) tests)]
       [(and (eq? first_test "notDone") (eq? conditionx "lemon") (eq? do_tests "none"))  (categorical-vw2 (vector 100  0  0) tests)]
       
       [(and (eq? first_test "positive") (eq? conditionx "good") (eq? do_tests "first")) (categorical-vw2 (vector 100  0  0) tests)]
       [(and (eq? first_test "positive") (eq? conditionx "good") (eq? do_tests "both"))  (categorical-vw2 (vector 0  88.889  11.111) tests)]
       
       [(and (eq? first_test "positive") (eq? conditionx "lemon") (eq? do_tests "first")) (categorical-vw2 (vector 100  0  0) tests)]
       [(and (eq? first_test "positive") (eq? conditionx "lemon") (eq? do_tests "both"))  (categorical-vw2 (vector 100  33.3333  66.6667) tests)]
       
       [(and (eq? first_test "negative") (eq? conditionx "good") (eq? do_tests "first"))  (categorical-vw2 (vector 100  0  0) tests)]
       [(and (eq? first_test "negative") (eq? conditionx "good") (eq? do_tests "both"))   (categorical-vw2 (vector 100  0  0) tests)]
       
       [(and (eq? first_test "negative") (eq? conditionx "lemon") (eq? do_tests "first")) (categorical-vw2 (vector 100  0  0) tests)]
       [(and (eq? first_test "negative") (eq? conditionx "lemon") (eq? do_tests "both"))  (categorical-vw2 (vector 0  44.4444  55.5556) tests)]
       [else "second_testxxx"]))
    
   
   (define buy_it
     (if (or
      (and (eq? do_tests "first") (eq? second_test "notDone") (eq? first_test "negative"))
      (and (eq? do_tests "both") (eq? second_test "positive") (eq? first_test "negative"))
      (and (eq? do_tests "both") (eq? second_test "negative") (eq? first_test "positive"))
      (and (eq? do_tests "both") (eq? second_test "negative") (eq? first_test "negative"))
      )
         "dont_buy"
         "buy"))
    

   (define v
     (cond
       [(and (eq? conditionx "good") (eq? buy_it "buy")) 60]
       [(and (eq? conditionx "good") (eq? buy_it "dont_buy")) 0]
       [(and (eq? conditionx "lemon") (eq? buy_it "buy")) -100]
       [(and (eq? conditionx "lemon") (eq? buy_it "dont_buy")) 0]
       [else "v_xxx"]))
    
    (define u 
      (cond
        [(eq? do_tests "none") 0]
        [(eq? do_tests "first") -9]
        [(eq? do_tests "both") -13]
        [else "u_xxx"]))
    
    
    (observe/fail (eq? do_tests "both"))
    (observe/fail (eq? first_test "negative"))
    ;; (observe/fail (eq? second_test "negative"))

    ;; (observe/fail (eq? do_tests "first"))
    ;; (observe/fail (eq? first_test "negative"))
    ;; (observe/fail (eq? buy_it "buy"))
    ;; (observe/fail (> v 0))

    (list conditionx
          do_tests
          first_test
          second_test
          buy_it
          v
          u
          (list do_tests first_test second_test buy_it v u (+ v u))
          )
    
    )

)

(show-marginals (model)
                (list  "condition"
                       "do_tests"
                       "first_test"
                       "second_test"
                       "buy_it"
                       "v"
                       "u"
                       "comb"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


