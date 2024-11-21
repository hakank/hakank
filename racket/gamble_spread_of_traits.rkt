#| 

  Spread of traits in Racket/Gamble 

  From http://godplaysdice.blogspot.com/2007/09/more-men-at-top-and-at-bottom.html
  """
  More men at the top, and at the bottom. 

  As has been documented by a lot of people, it seems that a lot of psychological traits 
  have the following properties:

  - men and women have approximately the same average for this trait, and
  - both genders have an approximately normal distribution for this trait, but
  - the distribution of men's values for this trait has a larger standard deviation 
    than the women's values
  """

  Here we model this by using gaussian 100 with stdev=20 for men and stdev=10 for women.
  As can be seen, the range for men is much larger than for women. However, the probability
  that women has higher value than men is (still) 0.5.

  variable : men
  mean: 100.01784810581658
  Min: 30.410180498219987 Mean: 100.04351337755973 Max: 169.9852883279272 Variance: 404.0711188888651 Stddev: 20.10152031287348
  HPD interval (0.84): 72.51662883924956..128.62456922502233
  HPD interval (0.9): 66.71396216775113..132.89900318213472
  HPD interval (0.95): 61.99378417441559..141.17707406842015
  HPD interval (0.99): 46.20112298968174..150.50683443872228

  variable : women
  mean: 99.9549833249369
  Min: 57.983958150078216 Mean: 100.14091049393784 Max: 136.34666448218718 Variance: 99.637417883473 Stddev: 9.981854431090097
  HPD interval (0.84): 86.88151039736607..114.854664246292
  HPD interval (0.9): 84.00083735342261..116.70442625328798
  HPD interval (0.95): 80.77737131748094..119.53126696273067
  HPD interval (0.99): 73.83785589929593..125.20580677537919

  variable : diff
  mean: 0.06286478087948943
  Min: -75.63878851719448 Mean: -0.09739711637775504 Max: 86.85208868089524 Variance: 495.22537379879395 Stddev: 22.253659784376904
  HPD interval (0.84): -32.93783844883778..29.599610978524467
  HPD interval (0.9): -37.41198665661476..35.742480347064415
  HPD interval (0.95): -43.97857430090245..42.943516089942435
  HPD interval (0.99): -57.72779542345738..56.742089266462386

  variable : p
  mean: 0.49770000000000153
  Min: 0 Mean: 0.4997 Max: 1 Variance: 0.24999991 Stddev: 0.4999999099999919



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

   (define men (normal 100 20))
   (define women (normal 100 10))
   (define diff (- men women))
   (define p (> women men))

   (list men
         women
         diff
         p
         )
    
   )
)

(show-marginals (model)
                (list  "men"
                       "women"
                       "diff"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.9 0.95 0.99)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


