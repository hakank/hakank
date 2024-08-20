#| 

  Student mood after exam in Racket Gamble.

  From Joost-Pieter Katoen
  "Probabilistic Programming Quantitative Modeling for the Masses?"
  (MMB 2018 Conference, Erlangen)
  
  Slide 4:
  """
  How likely does a student end up with a bad mood after getting
  a bad grade for an easy exam, given that she is well prepared?
  """

  Observe: preparation = good:

  var : difficulty
  easy: 0.5999999999999999
  hard: 0.4

  var : preparation
  good: 0.9999999999999999

  var : grade
  good: 0.6799999999999999
  bad: 0.32

  var : mood
  good: 0.508
  bad: 0.49199999999999994


  This is a port of my WebPPL model student_mood_after_exam.wppl

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

    (define badGood (vector "bad" "good"))
    (define difficulty (categorical-vw2 (vector 0.6 0.4) (vector "easy" "hard")))
    (define preparation (categorical-vw2 (vector 0.7 0.3) badGood))
    
    (define grade
      (cond
        [(and (eq? difficulty "easy") (eq? preparation "bad"))  (categorical-vw2 (vector 0.95 0.05) badGood)]
        [(and (eq? difficulty "easy") (eq? preparation "good")) (categorical-vw2 (vector 0.5 0.5) badGood)]
        [(and (eq? difficulty "hard") (eq? preparation "bad"))  (categorical-vw2 (vector 0.6 0.4) badGood)]
        [(and (eq? difficulty "hard") (eq? preparation "good"))  (categorical-vw2 (vector 0.05 0.95) badGood)]
        [else "unknown grade"]))
    
    (define mood (if (eq? grade "bad")
                     (categorical-vw2 (vector 0.9 0.1) badGood)
                     (categorical-vw2 (vector 0.3 0.7) badGood)))
    
    
    ;; (observe/fail (eq? difficulty "easy"))
    (observe/fail (eq? preparation "good"))
    ;; (observe/fail (eq? grade "bad"))
    ;; (observe/fail (eq? mood "bad"))

    (list difficulty
          preparation
          grade
          mood
          )

   )
  )

(show-marginals (model)
                (list "difficulty"
                      "preparation"
                      "grade"
                      "mood"
                      )
                #:num-samples 100000
                )
