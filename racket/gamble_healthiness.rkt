#| 

  Healthiness model in Racket.Gamble 

  From BLOG examples/healtiness.blog:
  """
  healthiness model
  This BLOG program describes the model for healthiness in 
  a person.
  See exercise 3.5 in:
  Probabilistic Graphical Models: Principles and Techniques
  Daphne Koller, Nir Friedman, MIT 2009
  Each person is described by whether they are health
  conscious, have free time, exercise, have a good diet, 
  have normal weight, have high cholesterol, and whether
  they tested positive for high cholesterol.
  @author: jnieh
  @date: 2012-09-13
  """

  Given the observation (not (TestedHighCholesterol "P1")):

  var : HealthConscious
  #t: 0.564
  #f: 0.436
  mean: 0.564

  var : LittleFreeTime
  #f: 0.5000000000000001
  #t: 0.5
  mean: 0.5

  var : Exercise
  #t: 0.5256000000000002
  #f: 0.4744
  mean: 0.5256000000000002

  var : GoodDiet
  #t: 0.6600000000000001
  #f: 0.33999999999999997
  mean: 0.6600000000000001

  var : NormalWeight
  #t: 0.5556800000000001
  #f: 0.44432000000000005
  mean: 0.5556800000000001

  var : HighCholesterol
  #f: 0.9000000000000004
  #t: 0.1
  mean: 0.1

  var : TestedHighCholesterol
  #f: 1.0
  mean: 0 (0.0)


  This is a port of my WebPPL model healthiness.wppl

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
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    (defmem (HealthConscious p) (flip 0.5))
    
    (defmem (LittleFreeTime p) (flip 0.5))
    
    (defmem (Exercise p)
      (let ([HealthConsciousP (HealthConscious p)]
            [LittleFreeTimeP  (LittleFreeTime p)])
        (cond
          [(and HealthConsciousP       LittleFreeTimeP)       (flip 0.5)]
          [(and HealthConsciousP       (not LittleFreeTimeP)) (flip 0.9)]
          [(and (not HealthConsciousP) LittleFreeTimeP)       (flip 0.1)] 
          [(and (not HealthConsciousP) (not LittleFreeTimeP)) (flip 0.5)])))
    
    (defmem (GoodDiet p) (if (HealthConscious p) (flip 0.7) (flip 0.3)))
    
    (defmem (NormalWeight p)
      (let ([GoodDietP (GoodDiet p)]
            [ExerciseP (Exercise p)])
        (cond
          [(and GoodDietP       ExerciseP)       (flip 0.8)]
          [(and GoodDietP       (not ExerciseP)) (flip 0.5)]
          [(and (not GoodDietP) ExerciseP)       (flip 0.5)]
          [(and (not GoodDietP) (not ExerciseP)) (flip 0.2)])))
    
    (defmem (HighCholesterol p) (if (GoodDiet p) (flip 0.3) (flip 0.7)))
    
    (defmem (TestedHighCholesterol p) (if (HighCholesterol p) (flip 0.9) (flip 0.1)))
    
    ; Evidence
    (observe/fail (not (TestedHighCholesterol "P1"))) ;; Original evidence
    ; (observe/fail (Exercise "P1"))
    ; (observe/fail (LittleFreeTime "P1"))

  
    (list (HealthConscious "P1")
          (LittleFreeTime "P1")
          (Exercise "P1")
          (GoodDiet "P1")
          (NormalWeight "P1")
          (HighCholesterol "P1")
          (TestedHighCholesterol "P1")
          )
    
    )
  )

(show-marginals (model)
                (list  "HealthConscious"
                       "LittleFreeTime"
                       "Exercise"
                       "GoodDiet"
                       "NormalWeight"
                       "HighCholesterol"
                       "TestedHighCholesterol"

                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


