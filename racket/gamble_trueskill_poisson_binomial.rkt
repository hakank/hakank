#| 

  True skill Poisson Binomial in Racket.Gamble 

  From SPPL model trueskill-poisson-binomial.pynb

  * Prior, no observations
  var : perfB1
  mean: 89.99655000000517

  var : skillA
  mean: 99.96223000000569

  var : perfA
  mean: 89.96396000000509

  var : result
  mean: 0.47347000000003253


  * With observations
  var : perfB1
  mean: 89.97899000000515

  var : skillA
  mean: 100.00266000000559

  var : perfA
  mean: 90.00740000000508

  var : result
  mean: 0.47575000000003287


  This is a port of my WebPPL model trueskill_poisson_binomial.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model [use-observations? #t])
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define perfB1 (binomial 100 0.9))

   (define skillA (poisson 100))
    
   (define perfA1 (binomial skillA 0.9))

   (define result (if (> perfA1 perfB1) 1 0))


   (when use-observations?
     (observe/fail (<= 80 perfB1))
     (observe/fail (and (>= skillA 77) (< skillA 125)))
     (observe/fail (= result 0))
     )

   (list perfB1
         skillA
         perfA1
         result
        )
   
   )
)

(displayln "Prior, no observations")
(show-marginals (model #f)
                (list  "perfB1"
                       "skillA"
                       "perfA"
                       "result")
                #:num-samples 100000
                #:skip-marginals? #t)

(newline)
(displayln "With observations")
(show-marginals (model #f)
                (list  "perfB1"
                       "skillA"
                       "perfA"
                       "result")
                #:num-samples 100000
                #:skip-marginals? #t)


