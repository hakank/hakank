#| 

  Intelligence test in Racket Gamble.

  From
  https://mathematica.stackexchange.com/questions/128945/problems-encoding-a-bayesian-network-with-just-five-nodes-using-probabilitydistr

  Example is from "page 53 in Probabilistic Graphical Models (2009), by Daphne Koller 
  and Neir Friedman:"

  """                                                                                                                                            
  The network has five nodes (random variables):

  Difficulty of a class taken by a student (0 = easy, 1 = hard)
  Intelligence of the student (0 = low, 1 = high)
  Grade achieved by the student (1 = A, 2 = B, 3 = C)
  SAT score of the student (0 = low, 1 = high)
  Letter of recommendation by the teacher (0 = False, 1 = True)
  We would like to use this network to do probabilistic inference (causal or evidential) like:                                                   
  "What is the probability of the student achieving an A, given that he is intelligent?"
  """

  * observation: intelligence = "intelligence_high"
   "What is the probability of the student achieving an A, given that he is intelligent?"

   var : grade
   grade_a: 0.7400000000000001   <---
   grade_b: 0.16800000000000007
   grade_c: 0.09200000000000003

   var : intelligence
   intelligence_high: 1.0

   var : difficulty
   difficulty_easy: 0.6000000000000001
   difficulty_hard: 0.40000000000000013

   var : sat
   sat_high: 0.8
   sat_low: 0.20000000000000007

   var : letter
   #t: 0.7677200000000002
   #f: 0.23228000000000004
   mean: 0.7677200000000002


  * observation: letter = true

   var : grade
   grade_a: 0.6485698815135683
   grade_b: 0.34447063320168175
   grade_c: 0.006959485284749652

   var : intelligence
   intelligence_low: 0.5415100649764302
   intelligence_high: 0.4584899350235698

   var : difficulty
   difficulty_easy: 0.7364313925340806
   difficulty_hard: 0.26356860746591915

   var : sat
   sat_low: 0.6061325487323225
   sat_high: 0.39386745126767736

   var : letter
   #t: 0.9999999999999998
   mean: 0.9999999999999998


  This is a port of my WebPPL model intelligence_test.wppl

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
   
   (define difficulty (categorical-vw2 (vector 0.6 0.4) (vector "difficulty_easy" "difficulty_hard")))
   (define intelligence (categorical-vw2 (vector 0.7 0.3) (vector "intelligence_low" "intelligence_high")))
    
   (define grades (vector "grade_a" "grade_b" "grade_c"))
   (define grade
     (cond
       [(and (eq? intelligence "intelligence_low") (eq? difficulty "difficulty_easy"))
        (categorical-vw2 (vector 0.3 0.4 0.3 ) grades)]
       [(and (eq? intelligence "intelligence_low") (eq? difficulty "difficulty_hard"))
        (categorical-vw2 (vector 0.05  0.25 0.7) grades)]
       [(and (eq? intelligence "intelligence_high") (eq? difficulty "difficulty_easy"))
        (categorical-vw2 (vector 0.9 0.08 0.02) grades)]
       [(and (eq? intelligence "intelligence_high") (eq? difficulty "difficulty_hard"))
        (categorical-vw2 (vector 0.5 0.3 0.2) grades)]
       [else "grade_unknonw"]))
   

   ;; receives a Letter of recommendation
   (define letter
     (case grade
       [("grade_a") (flip 0.9)]
       [("grade_b") (flip 0.6)]
       [("grade_c") (flip 0.01)]))
   
   
   (define sat
     (if (eq? intelligence "intelligence_high")
         (categorical-vw2 (vector 0.8  0.2)  (vector "sat_high" "sat_low"))
         (categorical-vw2 (vector 0.05 0.95) (vector "sat_high" "sat_low"))))
   
   
   ;; What is the probability of the student achieving an A, given that he is intelligent?
   
   ; (observe/fail (eq? sat "sat_high"))
   ; (observe/fail (not letter))
   ; (observe/fail letter)
   ; (observe/fail (eq? grade "grade_a"))
   (observe/fail (eq? intelligence "intelligence_high"))
   
   (list grade intelligence difficulty sat letter)
   
   )
  )

(show-marginals (model)
                (list "grade"
                      "intelligence"
                      "difficulty"
                      "sat"
                      "letter"
                      )
                #:num-samples 10000
                #:truncate-output 4
                ; #:skip-marginals? #t
                )


