#| 

  Weight scale problem in Racket.Gamble 

  Port of Pyro's Weight scale model
  """
  Suppose we are trying to figure out how much something weighs, but the scale we're using 
  is unreliable and gives slightly different answers every time we weigh the same object. 
  We could try to compensate for this variability by integrating the noisy measurement 
  information with a guess based on some prior knowledge about the object, like its 
  density or material properties. The following model encodes this process:

    weight | guess ~ Normal(guess,1)
    measurement | guess, weight ~ Normal(weight,0.75)

  Note that this is a model not only for our belief over weight, but also for the result 
  of taking a measurement of it. The model corresponds to the following stochastic function: 
     def scale(guess):
        weight = pyro.sample("weight", dist.Normal(guess, 1.0))
        return pyro.sample("measurement", dist.Normal(weight, 0.75))
  """

var : weight
mean: 9.51981936120868
Histogram:
7.229 : 1  
7.457 : 0  
7.684 : 0  
7.912 : 0  
8.139 : 4  
8.367 : 15 
8.595 : 16 
8.822 : 91 
9.05  : 115
9.277 : 139
9.505 : 131
9.733 : 147
9.96  : 112
10.188: 115
10.415: 70 
10.643: 15 
10.87 : 15 
11.098: 8  
11.326: 2  
11.553: 3  

var : measurement
mean: 9.487637546679595
Histogram:
6.388 : 1  
6.664 : 0  
6.939 : 0  
7.214 : 6  
7.49  : 16 
7.765 : 15 
8.041 : 38 
8.316 : 36 
8.591 : 62 
8.867 : 75 
9.142 : 128
9.417 : 126
9.693 : 111
9.968 : 98 
10.243: 87 
10.519: 71 
10.794: 57 
11.069: 26 
11.345: 27 
11.62 : 13 


  This is a port of my WebPPL model weight_scale.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model [guess_val 9.5])
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define guess guess_val)
   (define weight (normal guess 1.0))
   (define measurement (normal weight 0.75))
   
   (observe-sample (normal-dist weight 1) guess)
        
   (list weight
         measurement
           )
 
   )
)

(show-marginals (model)
                (list  "weight"
                       "measurement"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:show-histogram? #t
                ; #:show-percentiles? #t
                )


