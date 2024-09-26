#| 

  Repeated IQ measurements in Racket.Gamble 

  From https://mhtess.github.io/bdappl/chapters/03-simpleModels.html
  """
  Repeated measures of IQ

  The data are the measures xijx_(ij)xij​ 
     for the i=1,...,ni = 1, . . . ,ni=1,...,n people 
  and their j=1,...,mj = 1, . . . ,mj=1,...,m repeated test scores.

  We assume that the differences in repeated test scores are distributed as 
  Gaussian error terms with zero mean and unknown precision. The mean of the 
  Gaussian of a person’s test scores corresponds to their latent true IQ. 
  This will be different for each person. The standard deviation of the 
  Gaussians corresponds to the accuracy of the testing instruments in 
  measuring the one underlying IQ value. We assume this is the same for 
  every person, since it is conceived as a property of the tests themselves.
  """

  var : sigma
  mean: 6.60206193585826
  Credible interval (0.9): 3.0592525346028916..9.550697446462017

  var : mus 0
  mean: 95.03230170685146
  Credible interval (0.9): 88.79917572956266..101.69685593641971

  var : mus 1
  mean: 110.12512515692133
  Credible interval (0.9): 104.25004174094849..116.30471064508423

  var : mus 2
  mean: 154.85411265150364
  Credible interval (0.9): 149.47634667416108..160.7821106078753


  This is a port of my WebPPL model repeated_iq_measurements.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define data '(
               (90 95 100) ; p1
               (105 110 115) ; p2
               (150 155 160) ; p3
               ))

(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define len (length data))
   
   ;; everyone shares same sigma (corresponding to measurement error)
   (define sigma (uniform 0 50))

   ;; each person has a separate latent IQ
   (defmem (mus i) (uniform 0 200))

   (for ([i len])
     (for ([v (list-ref data i)])
           (observe-sample (normal-dist (mus i) sigma) v)))

   (list sigma
         (mus 0)
         (mus 1)
         (mus 2)
         )
   
   )
)

(show-marginals (model)
                (list  "sigma"
                       "mus 0"
                       "mus 1"
                       "mus 2"
                     )
                    #:num-samples 100000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.90
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


