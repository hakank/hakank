#| 

  Extreme value distribution maximum wind speed in Racket.Gamble 

  From Mathematica ExtremeValueDistribution
  """
  ExtremeValueDistribution can be used to model monthly maximum wind speeds. Recorded monthly 
  maxima of wind speeds in km/h for Boston, MA, from January 1950 till December 2009:
   ...
  Fit the distribution to the data:
  edist = EstimatedDistribution(maxWinds, ExtremeValueDistribution(a, b))
  -> ExtremeValueDistribution(47.2374, 9.1497))

  ... 

  Find the probability of monthly maximum wind speed exceeding 60 mph:
  Probability(x > Quantity(60, "mph"), x ~ edist)
  -> 0.00454842
  """

  The original data was in km/h but the last query is in mph, 
  so let's convert 60mph to km/h:
  """  
  UnitConvert[Quantity[60, ("Miles")/("Hours")], "km/h"] // N
  -> Quantity[96.5606, ("Kilometers")/("Hours")]
  Probability[x > QuantityMagnitude@%, x \[Distributed] edist]
  0.00454842
  """

  (data length: 72 mean 73.56 stddev 13.865583291012324)
  var : a
  69.55645316993287: 0.19200000000000006
  68.47227973357936: 0.10109999999999998
  67.3938114252814: 0.08239999999999999
  70.68451950347767: 0.07399999999999998
  68.70857397124001: 0.06029999999999998
  ...
  79.01112890692454: 0.013999999999999995
  75.2008341235125: 0.012899999999999997
  83.62088567611346: 0.008199999999999999
  66.98511507652623: 0.006799999999999998
  68.45874448378304: 0.0011999999999999997
  mean: 69.61750968443357
  HPD interval (0.94): 66.50534673575974..71.28174710278188

  var : b
  9.728413193753745: 0.21709999999999993
  10.692472701626219: 0.17459999999999992
  9.378559000638734: 0.15019999999999994
  7.572683369690062: 0.10019999999999998
  11.587773488599154: 0.09859999999999998
  ...
  7.66488734027244: 0.020799999999999992
  11.42516147776124: 0.020299999999999995
  7.713945366294564: 0.018999999999999996
  9.223741546712152: 0.010899999999999998
  7.323705735430142: 0.0036999999999999993
  mean: 9.815320124032112
  HPD interval (0.94): 7.0969358717291975..10.185738291453296

  var : post
  63.99087448160311: 0.029699999999999994
  84.62521329651406: 0.027599999999999993
  66.5970538191691: 0.024499999999999994
  97.54193441189778: 0.020899999999999995
  67.48020246298809: 0.020799999999999996
  ...
  58.81908967821251: 0.00019999999999999996
  82.32593604348038: 9.999999999999998e-5
  68.42984161074129: 9.999999999999998e-5
  59.30169546255724: 9.999999999999998e-5
  74.21227670240283: 9.999999999999998e-5
  mean: 74.69963847801709
  HPD interval (0.94): 54.353429449576424..91.45059311425202

  var : p
  #f: 0.9287999999999998
  #t: 0.07119999999999999
  mean: 0.07119999999999999


  This is a port of my WebPPL model extreme_value_test.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; Annual maximum wind speed in Boston 1938..2009
; From Mathematica (FrechetDistribution, see above)
(define data '(72.36 64.8 72.36 68.4 77.76 55.44 96.48 96.48 66.6 72.36
                     61.2 55.8 96.48 74.16 77.76 88.92 138.96 77.76 85.32 66.6
                     70.56 63 81.72 66.6 74.16 77.76 64.8 57.6 66.6 77.76 63
                     64.8 74.16 74.16 79.56 77.76 68.76 59.4 86.76 84.96 92.88
                     81.72 79.56 81.72 64.8 105.48 64.8 85.32 64.8 63 72.36
                     100.08 59.4 75.96 81.72 87.12 77.76 68.76 74.16 68.4 75.96
                     63 63 64.8 57.6 61.2 59.4 66.6 57.24 63 55.44 59.4))

(show2 "data length:" (length data) "mean" (avg data) "stddev" (stddev data))

(define (model)
  (; enumerate 
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   ; (define a (uniform 0 100))
   (define a (normal (avg data) 10))
   ; (define b (uniform 0 100))
   (define b (normal (stddev data) 10))

   (define sigma 10)
   
   (for ([i (length data)])
     (observe-sample (normal-dist (extreme_value_dist2 a b) sigma) (list-ref data i)))

   (define post (extreme_value_dist2 a b))   
   (define p (> post 96.5606)) ; 60mph -> 96.5606km/h

   ; (show2 a b post)
   
   (list a
         b
         post
         p
    )
   
   ) ; #:transition (single-site)
  )

(show-marginals (model)
                (list "a"
                      "b"
                      "post"
                      "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.94
                ; #:credible-interval2 0.94
                #:hpd-interval (list 0.94)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 1000
                ; #:thin 100
                )

