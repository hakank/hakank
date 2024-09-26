#| 

  Generating Bernoulli/Binomial distribution in Racket.Gamble 

  From Handbook on probability distributions
  page 8ff

  See gamble_distributions.rkt and gamble_distributions_test.rkt
  for more on this.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")



(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define p 8/10)
   (define bern (bernoulli_dist p))
   (define bern2 (bernoulli p)) ; Built-in
   
   (list bern
         bern2
         )
   
   )
)

(displayln "Bernoulli")
(show-marginals (model1)
                (list  "bern"
                       "bern2"
                     )
                    #:num-samples 10000
                    )

(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define p 8/10)
   (define n 10)
   (define binom (binomial_dist n p))
   (define binom2 (binomial_int_dist n p)) 
   (define binom3 (binomial n p))
   
   (list binom
         binom2
         binom3
         )
   
   )
)

(displayln "Binomial")
(show-marginals (model2)
                (list " binom"
                       "binom2"
                       "binom3"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )

