#| 

  Multivariate hypergeometric distribution in Racket/Gamble 

  From Mathematica MultivariateHypergeometricDistribution
  """
  An urn contains 12 red balls, 23 blue balls, and 9 green balls. 

  Find the distribution of a sample of 5 balls drawn without replacement:

  dist = MultivariateHypergeometricDistribution[5, {12, 23, 9}];  
  Probability[x==2 && z==3],{x,y,} in dist)
  -> 
  9/1763
  N@%
  -> 
  0.00510493

  Find the average number of balls of each color in a sample:
  Mean[dist] // N
  -> {1.36364, 2.61364, 1.02273}
  """

  PDF
  9/1763
  0.005104934770277935

  Mean
  '(15/11 115/44 45/44)
  '(1.3636363636363635 2.6136363636363638 1.0227272727272727)

  Generate (frequency)
  ((1 3 1) : 0.16379310344827586)
  ((2 3 0) : 0.10344827586206896)
  ((1 2 2) : 0.09482758620689655)
  ((0 4 1) : 0.09482758620689655)
  ((2 2 1) : 0.08620689655172414)
  ((1 4 0) : 0.08620689655172414)
  ((0 3 2) : 0.06896551724137931)
  ((2 1 2) : 0.0603448275862069)
  ((3 1 1) : 0.05172413793103448)
  ((3 2 0) : 0.05172413793103448)
  ((0 5 0) : 0.034482758620689655)
  ((1 1 3) : 0.02586206896551724)
  ((0 2 3) : 0.02586206896551724)
  ((2 0 3) : 0.017241379310344827)
  ((4 0 1) : 0.017241379310344827)
  ((4 1 0) : 0.017241379310344827)


  Cf gamble_urn_model_generalized.rkt with the following parameters:

  (define num_balls '(12 23 9))
  (define add_balls '(-1 -1 -1))
  (define num_draws 5)
  (define prob (list "num_observed" (lambda (v) (equal? v (list 2 0 3)))))
  -> 
  variable : p
  #f: 1754/1763 (0.994895065229722)
  #t: 9/1763 (0.005104934770277935)
  mean: 9/1763 (0.005104934770277935)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(displayln "PDF")
(multivariate_hypergeometric_pdf 5 (list 12 23 9)  (list 2 0 3))
(* 1.0 (multivariate_hypergeometric_pdf 5 (list 12 23 9)  (list 2 0 3)))


(displayln "Mean")
(multivariate_hypergeometric_mean 5 (list 12 23 9))
(map exact->inexact (multivariate_hypergeometric_mean 5 (list 12 23 9)))
(newline)

(displayln "Generate (frequency)")
(show-freq (for/list ([i 100])
  (multivariate_hypergeometric_dist 5 (list 12 23 9))
  ))

(newline)

#|
  The example as a model

  variable : d
  (1 3 1): 621/3526 (0.17612024957458877)
  (2 2 1): 6831/49364 (0.13838019609431976)
  (2 3 0): 759/7052 (0.10762904140669313)
  (1 2 2): 1242/12341 (0.10064014261405073)
  (1 4 0): 345/3526 (0.09784458309699376)
  (0 4 1): 1035/14104 (0.07338343732274533)
  (0 3 2): 207/3526 (0.05870674985819626)
  (3 2 0): 1265/24682 (0.05125192447937768)
  (2 1 2): 621/12341 (0.050320071307025364)
  (3 1 1): 1035/24682 (0.04193339275585447)
  (0 5 0): 437/14104 (0.030984117980714692)
  (1 1 3): 414/19393 (0.021347909039344092)
  (0 2 3): 69/3526 (0.019568916619398753)
  (4 1 0): 1035/98728 (0.010483348188963617)
  (3 0 2): 90/12341 (0.0072927639575399075)
  (2 0 3): 9/1763 (0.005104934770277935)
  (4 0 1): 405/98728 (0.004102179726116198)
  (0 1 4): 207/77572 (0.0026684886299180115)
  (1 0 4): 27/19393 (0.001392254937348528)
  (5 0 0): 9/12341 (0.0007292763957539907)
  (0 0 5): 9/77572 (0.00011602124477904399)

  variable : red
  1: 53940/135751 (0.3973451392623259)
  2: 3720/12341 (0.30143424357831616)
  0: 3596/19393 (0.18542773165575208)
  3: 1240/12341 (0.10047808119277206)
  4: 180/12341 (0.014585527915079815)
  5: 9/12341 (0.0007292763957539907)
  mean: 15/11 (1.3636363636363635)

  variable : blue
  3: 2415/7052 (0.34245604083947817)
  2: 2185/7052 (0.3098411798071469)
  4: 2415/14104 (0.17122802041973909)
  1: 19665/155144 (0.12675320992110556)
  5: 437/14104 (0.030984117980714692)
  0: 2907/155144 (0.018737431031815605)
  mean: 115/44 (2.6136363636363638)

  variable : green
  1: 765/1763 (0.4339194554736245)
  0: 527/1763 (0.29892229154849687)
  2: 765/3526 (0.21695972773681224)
  3: 1785/38786 (0.04602176042902078)
  4: 315/77572 (0.00406074356726654)
  5: 9/77572 (0.00011602124477904399)
  mean: 45/44 (1.0227272727272727)

  variable : p
  #f: 1754/1763 (0.994895065229722)
  #t: 9/1763 (0.005104934770277935)
  mean: 9/1763 (0.005104934770277935)


|#
(define (model)
  (enumerate

   (define n 5)
   (define num-balls (list 12 23 9))
   (define drawn-balls (multivariate_hypergeometric_dist n num-balls))

   (define red (first drawn-balls))
   (define blue (second drawn-balls))
   (define green (third drawn-balls))
   
   (define p (equal? drawn-balls (list 2 0 3)))

   (list drawn-balls
         red
         blue
         green
         p)

   )
)

(show-marginals (model)
                (list  "drawn-balls"
                       "red"
                       "blue"
                       "green"
                       "p"
                     )
                    #:num-samples 1000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


