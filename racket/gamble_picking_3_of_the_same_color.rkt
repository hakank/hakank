#| 

  Picking 3 of the same color in Racket.Gamble 

  From https://www.reddit.com/r/askmath/comments/1fani5o/a_maths_problem_my_dad_has_to_do_for_work_and/
  """
  A maths problem my dad has to do for work and nobody can figure it out
  Probability

  If I have a bag with 6 balls (3 blue and 3 red) and I pick out all 6, at which 
  point have I picked 3 of the same colour?

  this is confusing us because it’s not a normal probability question. we’ve all 
  been able to do “how likely am i to pick out three of the same?”, but we can’t 
  figure out how to do “how many balls do i have to pick until i’ve got 3 of the same?”

  we’re very confused please help
  """

  var : ret
  5: 3/5 (0.6)
  4: 3/10 (0.3)
  3: 1/10 (0.1)
  mean: 9/2 (4.5)

  From this we can draw 2 conclusions:
  1) The mean number of draws needed is 4.5 ("probability" approach)
  2) The max number of draws needed is 5 ("logical" approach)


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

   (define balls '("blue" "blue" "blue" "red" "red" "red"))
   (define num-balls (length balls))

   ; Permute the list
   (define a (draw-without-replacement num-balls balls))
   
   (define (f a num-blue num-red total)
     (let ([len (length a)]
           [pick (first a)]
           [a2 (rest a)]
           [total1 (add1 total)])
       (if (or
            (= num-blue 3)
            (= num-red 3)
            (= len 0))
           total
           (if (eq? pick "blue")
               (f a2 (add1 num-blue) num-red        total1)
               (f a2 num-blue        (add1 num-red) total1)
               ))))

   (define ret (f a 0 0 0))

   (list (list a ret)
         ret
         )
   
   
   )
)

(show-marginals (model)
                (list  "a ret"
                       "ret"
                     )
                    #:num-samples 1000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
