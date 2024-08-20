#| 

  M & M problem in Racket Gamble.

  From Think Bayes, page 6f.
  """
  M&M’s are small candy-coated chocolates that come in a variety of colors.
  Mars, Inc., which makes M&M’s, changes the mixture of colors from time
  to time.

  In 1995, they introduced blue M&M’s. Before then, the color mix in a bag
  of plain M&M’s was 30% Brown, 20% Yellow, 20% Red, 10% Green, 10%
  Orange, 10% Tan. Afterward it was 24% Blue , 20% Green, 16% Orange,
  14% Yellow, 13% Red, 13% Brown.

  Suppose a friend of mine has two bags of M&M’s, and he tells me that one
  is from 1994 and one from 1996. He won’t tell me which is which, but he
  gives me one M&M from each bag. One is yellow and one is green. What is
  the probability that the yellow one came from the 1994 bag?

  """

  Page 7 (the table): The answer is 20/27: ~0.74074.

  Here are two models. Model 2 is - arguable - simpler.

  Model 1
  var : mix 0
  mix1994: 20/27 (0.7407407407407407)
  mix1996: 7/27 (0.25925925925925924)

  var : mix 1
  mix1996: 20/27 (0.7407407407407407)
  mix1994: 7/27 (0.25925925925925924)

  var : color 0
  yellow: 1 (1.0)

  var : color 1
  green: 1 (1.0)

  Model 2
  var : mix 0
  mix1994: 20/27 (0.7407407407407407)
  mix1996: 7/27 (0.25925925925925924)

  var : mix 1
  mix1996: 20/27 (0.7407407407407407)
  mix1994: 7/27 (0.25925925925925924)

  var : color 0
  yellow: 1 (1.0)

  var : color 1
  green: 1 (1.0)



  This is a port of my WebPPL model m_and_m_problem.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;
; This is the first model
;
(define (model1)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    (define colors (vector "brown" "yellow" "red" "green" "orange" "tan" "blue"))
    (define mixes (vector "mix1994"  "mix1996"))

    
    ;; First pick a bag in mix(0) and then pick the other bag in mix(1)
    (define (mix1 i)
        (if (= i 0) 
            (uniform-draw mixes)
            (if (eq? (mix1 0) "mix1994")
                "mix1996"
                "mix1994")))
    (define mix (mem (lambda (i) (mix1 i))))
    
    (define color (mem (lambda (i)
        (if (eq? (mix i) "mix1994")
            (categorical-vw2 (vector 30 20 20 10 10 10 0) colors)
            (categorical-vw2 (vector 13 14 13 20 16 0 24) colors)))))
    
    ;; Note: This condition is not enough ...
    ; (observe/fail (and (eq? (color 0) "yellow") (eq? (color 1) "green")))
    
    ;; ... we must add that mix(0) and mix(1) are different.
    (observe/fail (eq? (color 0) "yellow"))
    (observe/fail (eq? (color 1) "green"))
    (observe/fail (not (eq? (mix 0) (mix 1))))
    
    (list (mix 0)
          (mix 1) 
          (color 0)
          (color 1)
          )
    
   )
  )

(displayln "Model 1")
(show-marginals (model1)
                (list "mix 0"
                      "mix 1"
                      "color 0"
                      "color 1"
                      "breakpoint"
                      "xs post"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.84
                )


;
; This is the second model, arguable simpler than model 1.
;
(define (model2)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    (define colors (vector "brown" "yellow" "red" "green" "orange" "tan" "blue"))
    (define mixes (vector "mix1994"  "mix1996"))

    
    ;; First pick a bag in mix(0) and then pick the other bag in mix(1)
    (define mix0 (uniform-draw mixes))
    (define mix1 (if (eq? mix0 "mix1994") "mix1996" "mix1994"))

    (define mix1994ps (vector 30 20 20 10 10 10 0))
    (define mix1996ps (vector 13 14 13 20 16 0 24))

    (define color0
      (if (eq? mix0 "mix1994")
          (categorical-vw2 mix1994ps colors)
          (categorical-vw2 mix1996ps colors)))

    (define color1
      (if (eq? mix1 "mix1994")
      (categorical-vw2 mix1994ps colors)
      (categorical-vw2 mix1996ps colors)))

    (observe/fail (eq? color0 "yellow"))
    (observe/fail (eq? color1 "green"))    
      
    (list mix0
          mix1
          color0
          color1
          )
    
   )
  )

(displayln "Model 2")
(show-marginals (model2)
                (list "mix 0"
                      "mix 1"
                      "color 0"
                      "color 1"
                      "breakpoint"
                      "xs post"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.84
                )
