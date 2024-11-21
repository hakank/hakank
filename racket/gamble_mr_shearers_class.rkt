#| 

  Mr Shearer's class in Racket/Gamble 

  https://minerva-demo.github.io/#category=Probability&index=2
  """
  Question
  Three-fourths of the students in Mr. Shearer's class have brown hair and six-sevenths 
  of his students are right-handed. If Mr. Shearer's class has 28 students, what is the 
  smallest possible number of students that could be both right-handed and have brown hair?
  ...
  Reference answer
  Mr. Shearer has 3/4(28)=21 students with brown hair and 6/7(28)=24 students who are 
  right-handed. Since there are 28−24=4 left-handed students, at most 4 of the 
  21 brown-haired students are left-handed. Therefore, at least 17 of them are 
  right-handed. Final Answer: The final answer is 17.
  ...
  Problem source: MATH Counting & Probability Level 1
  """

  variable : num-both
  18: 0.462
  17: 0.282
  19: 0.203
  20: 0.051
  21: 0.002
  mean: 18.029

  probs: ((18 0.453) (17 0.29) (19 0.213) (20 0.04) (21 0.004))
  min value: (17 0.29)

  This is a port of my WebPPL model mr_shearers_class.wppl.  
  However, the WebPPL model uses factor to minimize the value (num-both) 
  directly in the model. This feature is not supported in Gamble, so 
  we have to do an external check for this.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num_students 28)

   ; 3/4 of the class has brown hair
   (define brown_hair_p 3/4)
   (define has_brown_hair (for/list ([i num_students]) (bernoulli brown_hair_p)))
   ; Enforce the number of brown haired
   (observe/fail (= (* (sum has_brown_hair) 4) (* 3 num_students)))

   ; 6/7 of the class is right handed
   (define right_handed_p 6/7)
   (define is_right_handed (for/list ([i num_students]) (bernoulli right_handed_p)))
   ; Enforce the number of right handed
   (observe/fail (= (* (sum is_right_handed) 7) (* 6 num_students)))
  
   ; How many has both brown hair and is right handed?
   (define num_both
     ; (for/list ([i num_students]) (b2i (= (list-ref has_brown_hair i) (list-ref is_right_handed i) 1))))
     (sum (map (lambda (b r) (b2i (= b r 1))) has_brown_hair is_right_handed)))

   (list num_both
    )

   )
)

(show-marginals (model)
                (list  "num-both"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


(define probs (first (get-probs (model))))
(show "probs" probs)
(define min-val (apply min (map first probs)))

(displayln (format "min value: ~a" (assoc min-val probs )))
