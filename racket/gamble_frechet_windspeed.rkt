#| 

  Frechet windspeed in Racket/Gamble 

  From Mathematica (FrechetDistribution)
  """
  FrechetDistribution can be used to model annual maximum wind speeds:
  maxWinds =  Table(Max(
   DeleteMissing(
    WeatherData("Boston", "WindSpeed", (year), "Value"))), (year, 
   1938, 2009)) ;; QuantityMagnitude
  -> 
  (72.36, 64.8, 72.36, 68.4, 77.76, 55.44, 96.48, 96.48, 66.6, 72.36,
  61.2, 55.8, 96.48, 74.16, 77.76, 88.92, 138.96, 77.76, 85.32, 66.6,
  70.56, 63, 81.72, 66.6, 74.16, 77.76, 64.8, 57.6, 66.6, 77.76, 63,
  64.8, 74.16, 74.16, 79.56, 77.76, 68.76, 59.4, 86.76, 84.96, 92.88,
  81.72, 79.56, 81.72, 64.8, 105.48, 64.8, 85.32, 64.8, 63, 72.36,
  100.08, 59.4, 75.96, 81.72, 87.12, 77.76, 68.76, 74.16, 68.4, 75.96,
  63, 63, 64.8, 57.6, 61.2, 59.4, 66.6, 57.24, 63, 55.44, 59.4)

  Fit the distribution to the data:
  edist = EstimatedDistribution(maxWinds,FrechetDistribution(alpha, beta))
  -> FrechetDistribution(7.26626, 66.9224)

  Find the probability of annual maximum wind exceeding 90 km/h:
  Probability(x > 90, x in edist)
  -> 0.109663

  Find average annual maximum wind speed:
  Mean(edist)
  -> 73.6778

  data = RandomVariate(edist, 1000);
  MinMax(data)
  -> (51.1608, 213.02)
   """

  This model uses my frechet_dist from gamble_distributions.rkt to 
  recover the parameters alpha and beta. Well, it's a little unreliable (and slow).
  Using mh-sampler seems to be better than importance-sampler, with the possible
  drawback that it just rely on a few sampled values, for example for alpha in
  the first run (with sigma 1):

  * sigma 1

  data length:: 72
  mean(data):: 73.56
  var : alpha
  7.011214471033917: 0.7501000000000001
  7.350945456185531: 0.18980000000000002
  15.025748621056723: 0.04139999999999999
  44.80771702248723: 0.0187
  mean: 8.114291723538816
  Min: 7.011214471033917 Mean: 7.011214471034659 Max: 7.011214471033917 Variance: 5.513317312153443e-25 Stddev: 7.425171588693047e-13
  Credible interval (0.84): 7.011214471033917..7.011214471033917

  var : beta
  70.16131084007235: 0.36889999999999995
  70.21926159252398: 0.34070000000000006
  70.28834605908875: 0.1726
  71.66485253701254: 0.06670000000000001
  71.06875720875182: 0.0317
  58.15419329597456: 0.0101
  ...
  69.39503033410405: 0.0053
  72.39123105124688: 0.004
  mean: 70.21561972827557
  Min: 70.06181689561295 Mean: 70.1810617232454 Max: 70.29591755615165 Variance: 0.0062678149941681 Stddev: 0.07916953324460174
  Credible interval (0.84): 70.06181689561295..70.21926159252398

  var : post
  74.11371186149769: 0.0736
  82.49753010989787: 0.0326
  73.62161224422815: 0.0301
  76.72086836586581: 0.0272
  72.65938534560429: 0.0257
  ...
  80.35236875635478: 0.0002
  61.17615439475788: 0.0001
  57.02463124471219: 0.0001
  69.13885463872793: 0.0001
  87.39540160624716: 0.0001
  mean: 77.63149991259705
  Min: 53.50460204086387 Mean: 78.40708283492616 Max: 128.83004679920353 Variance: 249.73770479280012 Stddev: 15.803091621350555
  Credible interval (0.84): 59.071660871816405..91.44395151305363

  var : p
  #f: 0.8624999999999999
  #t: 0.13749999999999998
  mean: 0.13749999999999998
  Min: 0 Mean: 0.1608 Max: 1 Variance: 0.13494336 Stddev: 0.3673463760539908

  * sigma 10
  data length:: 72
  mean(data):: 73.56
  var : alpha
  13.362611196800863: 0.2942000000000002
  17.00183081822023: 0.2133000000000001
  12.90844396803443: 0.18630000000000008
  14.613937968318142: 0.08100000000000004
  18.12886520540434: 0.06710000000000003
  ...
  72.80995758354459: 0.007800000000000003
  28.506397183363006: 0.005500000000000003
  84.06575815418682: 0.004300000000000002
  51.280962342033206: 0.0016000000000000007
  14.809758002038503: 0.0007000000000000003
  mean: 18.887861820253384
  Min: 8.762449660010061 Mean: 9.647312552104989 Max: 12.90844396803443 Variance: 1.0019123184673917 Stddev: 1.0009557025500138
  Credible interval (0.84): 8.762449660010061..9.427510448946192

  var : beta
  69.4889920635447: 0.2733000000000002
  68.59476968468947: 0.13080000000000008
  73.20458291052307: 0.12620000000000006
  71.10864048205973: 0.09090000000000004
  69.85770109964317: 0.07650000000000004
  ...
  69.41225805090009: 0.009200000000000003
  68.19041207158315: 0.006700000000000003
  73.1732321601126: 0.005700000000000003
  69.29391485341118: 0.0015000000000000007
  68.50593447090631: 0.00010000000000000005
  mean: 70.48568973861272
  Min: 65.71543157702925 Mean: 68.33420056858787 Max: 71.14070346121981 Variance: 1.4702640818581512 Stddev: 1.2125444659302813
  Credible interval (0.84): 66.81307032328287..69.7092560596264

  var : post
  83.1755696434945: 0.028500000000000015
  67.40566408866455: 0.02330000000000001
  78.66463387591313: 0.02290000000000001
  73.59379525662266: 0.02240000000000001
  71.18267043094714: 0.02230000000000001
  ...
  72.36279664434889: 0.0003000000000000001
  68.36597811769583: 0.0003000000000000001
  74.88981578998118: 0.0002000000000000001
  73.29300292896035: 0.0002000000000000001
  70.68766296549826: 0.00010000000000000005
  mean: 74.43811842725839
  Min: 57.612805995926344 Mean: 75.02885611525188 Max: 164.8247798658004 Variance: 206.521282471396 Stddev: 14.370848356008633
  Credible interval (0.84): 60.388914852468815..82.03014529189541

  var : p
  #f: 0.9641000000000001
  #t: 0.035900000000000015
  mean: 0.035900000000000015
  Min: 0 Mean: 0.0679 Max: 1 Variance: 0.06328959 Stddev: 0.2515742236398634



  This is a port of my WebPPL model frechet_windspeed.wppl

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

(show "data length:" (length data))
(show "mean(data):" (avg data))


(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define alpha (uniform 0 100))
   ; (define beta (uniform 0 100))
   ; (define beta (normal (avg data) 10)) ;; a more informed prior
   (define beta (normal (avg data) 10)) ;; a more informed prior   

   ;; (define sigma (uniform 0 100)) ;; Letting sigma be free as well...
   (define sigma 3)
   
   (for ([i (length data)])
     (observe-sample (normal-dist (frechet_dist alpha beta) sigma) (list-ref data i)))

   (define post (frechet_dist alpha beta))
   (define p (> post 90))
    
   (list alpha
         beta
         post
         p
         ; sigma
    )
   )
)

(show-marginals (model)
                (list  "alpha"
                       "beta"
                       "post"
                       "p"
                       ; "sigma"
                     )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


