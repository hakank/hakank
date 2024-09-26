#| 

  Disease infection in Racket.Gamble 

  This is a port of the SPPL model disease-infection.pynb

  The SPPL model give a value of 330 for num_met.

  * p is hardcoded to 0.5 as in the SPPL model

  var : num_people
  mean: 670.2118260314933
  Credible interval (0.84): 602..738

  var : num_met
  mean: 334.8943701712623
  Credible interval (0.84): 307..368

  var : num_infected
  mean: 100.00000000000003
  Credible interval (0.84): 100..100

  var : p
  mean: 0.5000000000000001
  Credible interval (0.84): 0.5..0.5


  * If we also estimate p the result give a quite larger value of 
   num_people, and a much larger credible interval: almost the full
    range of the prior 500..1500.


  var : num_people
  mean: 899.5082012757703
  Credible interval (0.84): 513..1329

  var : num_met
  mean: 335.5759196643458
  Credible interval (0.84): 298..373

  var : num_infected
  mean: 99.99999999999999
  Credible interval (0.84): 100..100

  var : p
  mean: 0.41235067450124685
  Credible interval (0.84): 0.22329453249584486..0.5537010466609658


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define estimate-p #t)

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define num_infected 100)
    
    (define num_people (+ 500 (random-integer 1000)))

    ; The probability p for num_met is hardcoded to 0.5 in the SPPL model,
    ; but let's go crazy and estimate it as well.    
    ; Change estimate-p above to #t for that.
    (define p (if estimate-p (uniform 0 1) 0.5 ))    
    (define num_met (binomial num_people p))
    
    (observe-sample (binomial-dist num_met 0.3) num_infected)
              
    (list num_people
          num_met
          num_infected
          p
          )

   )
)

(show-marginals (model)
                (list  "num_people"
                       "num_met"
                       "num_infected"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


