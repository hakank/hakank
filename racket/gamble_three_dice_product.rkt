#| 

  Three dice product in Racket/Gamble 

  From https://x.com/StudyMaths/status/1938559419719033125
  """
  Why is one of the boxes so much harder to fill than the rest?


  Roll a set of three dice 20 times and calculate their product.
  Record the results in the Carrol diagram

                 Odd product           Even product
                 -----------------------------------
  Less than 40   |0.09259259259259259  0.5185185185185185
                 |
  40 or more     |0.032407407407407460 0.35648148148148145


  """

  1. < 40: 0.6111111111111112  >40: 0.3888888888888889
  2. odd : 1/8                 even: 7/8                  


variable : roll
12: 5/72 (0.06944444444444445)
24: 5/72 (0.06944444444444445)
30: 1/18 (0.05555555555555555)
36: 1/18 (0.05555555555555555)
60: 1/18 (0.05555555555555555)
...
64: 1/216 (0.004629629629629629)
1: 1/216 (0.004629629629629629)
216: 1/216 (0.004629629629629629)
27: 1/216 (0.004629629629629629)
125: 1/216 (0.004629629629629629)
mean: 343/8 (42.875)

variable : < 40 odd
#f: 49/54 (0.9074074074074074)
#t: 5/54 (0.09259259259259259)
mean: 5/54 (0.09259259259259259)

variable : < 40 even
#t: 14/27 (0.5185185185185185)
#f: 13/27 (0.48148148148148145)
mean: 14/27 (0.5185185185185185)

variable : >= 40 odd
#f: 209/216 (0.9675925925925926)
#t: 7/216 (0.032407407407407406)
mean: 7/216 (0.032407407407407406)

variable : >= 40 even
#f: 139/216 (0.6435185185185185)
#t: 77/216 (0.35648148148148145)
mean: 77/216 (0.35648148148148145)

variable : lt40
#t: 11/18 (0.6111111111111112)
#f: 7/18 (0.3888888888888889)
mean: 11/18 (0.6111111111111112)

variable : ge40
#f: 11/18 (0.6111111111111112)
#t: 7/18 (0.3888888888888889)
mean: 7/18 (0.3888888888888889)

variable : odd
#f: 7/8 (0.875)
#t: 1/8 (0.125)
mean: 1/8 (0.125)

variable : even
#t: 7/8 (0.875)
#f: 1/8 (0.125)
mean: 7/8 (0.875)



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

   (define n 3)

   (define roll (for/product ([i 3]) (add1 (random-integer 6))))

   (define lt40_odd (and (< roll 40) (= (modulo roll 2) 1)))
   (define lt40_even (and (< roll 40) (= (modulo roll 2) 0)))
   (define ge40_odd (and (>= roll 40) (= (modulo roll 2) 1)))
   (define ge40_even (and (>= roll 40) (= (modulo roll 2) 0)))

   (define lt40 (and (< roll 40)))
   (define ge40 (and (>= roll 40)))
   
   (define odd (= (modulo roll 2) 1))   
   (define even (= (modulo roll 2) 0))
   (list roll
         lt40_odd
         lt40_even
         ge40_odd
         ge40_even
         lt40
         ge40
         odd
         even
         )

   )
)

(show-marginals (model)
                (list  "roll"
                       "< 40 odd"
                       "< 40 eveb"
                       ">= 40 odd"
                       ">= 40 even"
                       "lt40"
                       "ge40"
                       "odd"
                       "even"
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


