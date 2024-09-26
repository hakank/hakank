#| 

  Girl births in Racket.Gamble 

  From Gelman et.al "Regression and Other Stories", 69f

  Part 1 (page 69)
  """
  The probability that a baby is a girl or boy is approximately 48.8% or 51.2%, respectively, 
  and these of births do not vary much across the world. Suppose that 400 babies are born in a 
  hospital in a given year. How many will be girls?
  """

  mean: 195.19999999999996
  Credible interval (0.5): 187..200
  Credible interval (0.9): 177..210


  Part 2 (page 70)
  """
  Accounting for twins
  We can complicate the model in various ways. For example, there is a 1/125 chance that a 
  birth event results in fraternal twins, of which each has an approximate 49.5% chance of being a girl,  
  and a 1/300 chance of identical twins, which have an approximate 49.5% chance of being a pair of girls.
  """

  var : girl
  mean: 195.2315848422622

  var : identical_twins
  #f: 0.996693245153204
  #t: 0.0033067548467959136
  mean: 0.0033067548467959136

  var : fraternal_twins
  #f: 0.9920264540387743
  #t: 0.00797354596122563
  mean: 0.00797354596122563

  var : p
  0.488: 0.9887196991919783
  0.495: 0.011280300808021546
  mean: 0.48807896210565604

  Credible interval (0.5): 186..200
  Credible interval (0.9): 178..210


  This is a port of my WebPPL model girl_births.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 400)
   (define girl (binomial n 0.488))

   (list girl )

   )
)

(displayln "\nModel 1")
(show-marginals (model1)
              (list  "girl"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.9
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
(define *samples1* (repeat (lambda () (first (sample (model1)))) 1000))
(show-credible-interval *samples1* 0.5)
(show-credible-interval *samples1* 0.9)

(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    (define n 400)
    (define identical_twins (flip 1/300))
    (define fraternal_twins (flip 1/125))
    
    ;; Cannot be both identical_twins and fraternal_twins
    (observe/fail (not (and identical_twins fraternal_twins)))

    ;; The probability of a girl:
    (define p (if (or identical_twins fraternal_twins) 0.495 0.488))

    ;; Number of girls
    (define girl (binomial n p))

    (list girl
          identical_twins
          fraternal_twins
          p
          )
           
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "girl"
                       "identical_twins"
                       "fraternal_twins"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.90
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(define *samples2* (repeat (lambda () (first (sample (model2)))) 1000))
(show-credible-interval *samples2* 0.5)
(show-credible-interval *samples2* 0.9)

