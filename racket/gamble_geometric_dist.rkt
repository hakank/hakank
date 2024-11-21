#| 

  Geometric dist in Racket/Gamble 

  From Handbook on probability distributions
  page 19
  Expectation: (1-p)/p
  """
  A basic algorithm is to use i.i.d. Bernoulli variables as follows:
  * initialize X to 0 and generate U from an uniform distribution,
  * while U > p do ; generate U from an uniform distribution; X = X + 1;
  * return X.
  """

  Here all the version of the generator functions are tested.
. 
  geometric_dist (g1) and geometric_dist2 (g2) cannot be used with enumerate.
  geometric_dist3 (g3) can be used with enumerate. It's also compared 
  with the built-in function (g0),

  var : g0
  0: 0.09986000000013759
  1: 0.08905000000012392
  2: 0.07890000000011206
  3: 0.07217000000010469
  4: 0.06637000000009852
  ...
  93: 1.0000000000014456e-5
  96: 1.0000000000014456e-5
  98: 1.0000000000014456e-5
  104: 1.0000000000014456e-5
  107: 1.0000000000014456e-5
  mean: 8.9902300000131
  HPD interval (0.84): 0..17
  HPD interval (0.95): 0..28
  HPD interval (0.99): 0..43

  var : g1
  0: 0.10069000000013864
  1: 0.08990000000012523
  2: 0.08074000000011412
  3: 0.07342000000010587
  4: 0.06424000000009661
  ...
  99: 1.0000000000014456e-5
  103: 1.0000000000014456e-5
  105: 1.0000000000014456e-5
  111: 1.0000000000014456e-5
  113: 1.0000000000014456e-5
  mean: 9.003420000013111
  HPD interval (0.84): 0..17
  HPD interval (0.95): 0..28
  HPD interval (0.99): 0..44

  var : g2
  0: 0.09929000000013667
  1: 0.08965000000012456
  2: 0.08029000000011383
  3: 0.07348000000010596
  4: 0.06548000000009765
  ...
  85: 1.0000000000014456e-5
  89: 1.0000000000014456e-5
  92: 1.0000000000014456e-5
  119: 1.0000000000014456e-5
  121: 1.0000000000014456e-5
  mean: 9.01839000001313
  HPD interval (0.84): 0..17
  HPD interval (0.95): 0..28
  HPD interval (0.99): 0..44

  var : g3
  0: 0.10032000000013835
  1: 0.08937000000012461
  2: 0.08134000000011467
  3: 0.07183000000010425
  4: 0.06683000000009894
  ...
  86: 1.0000000000014456e-5
  90: 1.0000000000014456e-5
  93: 1.0000000000014456e-5
  94: 1.0000000000014456e-5
  99: 1.0000000000014456e-5
  mean: 9.009590000013125
  HPD interval (0.84): 0..17
  HPD interval (0.95): 0..28
  HPD interval (0.99): 0..44


  Model 2 using geometric_dist3 and enumerate (with #:limit)
  var : g3
  0: 0.10000000000953037
  1: 0.09000000000857733
  2: 0.08100000000771959
  3: 0.07290000000694763
  4: 0.0656100000062529
  ...
  214: 1.613975805374309e-11
  215: 1.4525782248368797e-11
  216: 1.3073204023531934e-11
  217: 1.1765883621178756e-11
  218: 1.0589295259060895e-11
  mean: 8.999999979128496
  HPD interval (0.84): 0..16
  HPD interval (0.95): 0..29
  HPD interval (0.99): 0..45



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")



(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p 1/10)

   (define g0 (geometric p)) ; built-in
   (define g1 (geometric_dist p))
   (define g2 (geometric_dist2 p))
   (define g3 (geometric_dist3 p))         

   (list g0
         g1
         g2
         g3
         )
   
   )
)

(show-marginals (model)
                (list  "g0"
                       "g1"
                       "g2"
                       "g3"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.95 0.99)                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

; Using geometric_dist3 enumerate (with limit)
(define (model2)
  (enumerate #:limit 1e-10 ; 1e-15 can be used for p=1/10

   ; (define p 1/10) ; Generates huge ratio numbers       
   (define p 0.1)
             
   (define g3 (geometric_dist3 p))         

   (list g3
         )
   
   )
)

(displayln "\nModel 2 using geometric_dist3 and enumerate (with #:limit)")
(show-marginals (model2)
                (list  "g3"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.95 0.99)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


