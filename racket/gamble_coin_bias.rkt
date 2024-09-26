#| 

  Coin bias in Racket.Gamble 

  From the R2 model CoinBias.cs 

  Output from the R2 model:
  """
    Mean: 0.421294
    Variance: 0.0162177
    Number of accepted samples = 692
  """
  
  var : bias
  0.6013552259469682: 0.0026823005129375573
  0.6024186603388273: 0.0026821881986097587
  0.594381333767502: 0.002681472613324417
  0.5941704291809459: 0.002681405483821145
  0.5933879285963183: 0.0026811349303185023
  ...
  0.016352679824667597: 3.2838827393042676e-7
  0.012009698386408958: 1.3123318784276177e-7
  0.011135156074790925: 1.0478637442646731e-7
  0.011019446335646237: 1.0157734119739272e-7
  0.0022027380113221248: 8.258766135391152e-10
  mean: 0.4157490447818984
  Credible interval (0.84): 0.276379406301179..0.6422936514062954


  This is a port of my WebPPL model coin_bias.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define x '(1 1 0 1 0))
   (define n (length x))
    
   ;; Beta(2,5) has mean about 0.2855
   (define bias (beta 2 5))
   (for ([v x])
     (observe-sample (bernoulli-dist bias) v)
     )
 
   (list bias
         )


   )
)

(show-marginals (model)
                (list  "bias"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


