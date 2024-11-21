#| 

  Mean of uniform in Racket/Gamble 

  How much differs the mean from random selection of (discrete) uniform distribution?
  Here we take n samples from 1..n.

  For n=10, there is a small chance that the mean is >= 8, about 0.0023
  The exact value (from Model 2) is 0.0028197500000000115.

  * Model 1: Using randomInteger (1..10) with Rejection
  * Model 2: Using multinormial with Enumerate (exact)
  * Model 3: Parameter estimation from the results in Model 2 (100 samples)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#|
 Using 1+randomInteger

variable : m
5.5: 0.04348000000000002
5.6: 0.043360000000000024
5.4: 0.04270000000000002
5.7: 0.04256000000000002
5.3: 0.04180000000000002
...
8.7: 6.000000000000003e-5
2.2: 3.0000000000000014e-5
8.8: 1.0000000000000006e-5
9.1: 1.0000000000000006e-5
2.3: 1.0000000000000006e-5
mean: 5.498125000000001
Percentiles:
(0.01 3.4)
(0.025 3.7)
(0.1 4.3)
(0.05 4.0)
(0.25 4.9)
(0.5 5.5)
(0.75 6.1)
(0.84 6.4)
(0.9 6.7)
(0.95 7.0)
(0.975 7.3)
(0.99 7.6)
(0.999 8.2)

variable : s
55: 0.04348000000000002
56: 0.043360000000000024
54: 0.04270000000000002
57: 0.04256000000000002
53: 0.04180000000000002
...
87: 6.000000000000003e-5
22: 3.0000000000000014e-5
23: 1.0000000000000006e-5
88: 1.0000000000000006e-5
91: 1.0000000000000006e-5
mean: 54.98125000000004
Percentiles:
(0.01 34)
(0.025 37)
(0.1 43)
(0.05 40)
(0.25 49)
(0.5 55)
(0.75 61)
(0.84 64)
(0.9 67)
(0.95 70)
(0.975 73)
(0.99 76)
(0.999 82)

variable : p
#f: 0.9971100000000004
#t: 0.002890000000000001
mean: 0.002890000000000001
Percentiles:
(0.01 #f)
(0.025 #f)
(0.1 #f)
(0.05 #f)
(0.25 #f)
(0.5 #f)
(0.75 #f)
(0.84 #f)
(0.9 #f)
(0.95 #f)
(0.975 #f)
(0.99 #f)
(0.999 #t)


|#

(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 10)
   (define sample (for/list ((i n)) (add1 (random-integer n))))
   (define m (avg sample))
   (define s (sum sample))
   (define p (>= m 8))
  
   (list (* 1.0 m)
         s
         p
         )
   )
)

(show-marginals (model1)
                (list  "m"
                       "s"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  Using multinomial (importance-sampler)
variable : m
5.3: 0.044000000000000004
5.4: 0.04370000000000001
5.9: 0.04370000000000001
5.2: 0.041800000000000004
5.6: 0.04150000000000001
...
2.4: 0.00010000000000000002
2.5: 0.00010000000000000002
2.6: 0.00010000000000000002
8.6: 0.00010000000000000002
8.7: 0.00010000000000000002
mean: 5.4852000000000025
Percentiles:
(0.01 3.4)
(0.025 3.7)
(0.1 4.3)
(0.05 4.0)
(0.25 4.9)
(0.5 5.5)
(0.75 6.1)
(0.84 6.4)
(0.9 6.7)
(0.95 7.0)
(0.975 7.3)
(0.99 7.7)
(0.999 8.3)

variable : p
#f: 0.9969000000000001
#t: 0.0031
mean: 0.0031
Percentiles:
(0.01 #f)
(0.025 #f)
(0.1 #f)
(0.05 #f)
(0.25 #f)
(0.5 #f)
(0.75 #f)
(0.84 #f)
(0.9 #f)
(0.95 #f)
(0.975 #f)
(0.99 #f)
(0.999 #t)


|#
(define (model2 [only-m #f])
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 10)
   ; Get the "coefficients" for each value 1..n
   (define sample (multinomial_dist n (for/list ((i n)) (/ 1 n))))
   ; Multiply each coefficient with its value
   (define mults (for/list ((i (length sample))) (* (add1 i) (list-ref sample i))))
   (define m (avg mults))
   (define p (>= m 8))

   (if only-m
       (* 1.0 m)
       (list (* 1.0 m)
             p
             )
       )
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "m"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  Get 100 values from Model 2 and try to estimate the parameters
  for a gaussian distribution.

sim: (6.2 5.2 5.5 6.5 3.9 5.7 5.7 5.7 5.5 6.7 5.6 5.9 4.8 5.7 4.6 6.2 7.0 4.7 7.2 6.8 5.7 4.4 4.4 4.6 5.2 5.8 5.3 6.7 5.7 4.1 5.7 6.3 6.8 5.8 4.1 5.4 5.7 5.8 4.2 6.5 5.6 3.4 7.2 6.4 5.9 5.2 5.5 6.3 5.0 7.1 5.5 6.0 5.9 5.0 5.1 5.6 5.1 5.3 5.8 5.2 5.8 5.3 3.5 5.5 5.7 5.5 5.4 5.9 5.8 5.1 4.0 5.6 6.8 5.3 5.5 4.5 5.8 6.7 6.3 4.9 5.7 5.6 6.9 4.4 5.7 6.3 5.4 5.2 6.6 6.6 6.8 4.7 6.4 4.5 6.9 3.9 5.9 5.0 5.1 7.1)

Model 3
variable : mu
5.551897465436411: 0.11080101421251339
5.547647658714725: 0.10741144355798032
5.621192047653708: 0.10285704546048091
5.565179294803482: 0.08576357731975093
5.579728519214209: 0.0838311261590894
...
1.479565156099748: 4.6785799373790374e-269
1.2497680541015592: 2.519235350896405e-269
0.10564751969060956: 1.0796722932413166e-269
8.688276923997702: 7.197815288275443e-270
0.6415867487550815: 3.5989076441377216e-270
mean: 5.579654551020094
Percentiles:
(0.01 5.3892577022699655)
(0.025 5.432537151493069)
(0.1 5.479610499404135)
(0.05 5.466721655586294)
(0.25 5.5325194729408365)
(0.5 5.595652857771096)
(0.75 5.674590482449816)
(0.84 5.69058899619675)
(0.9 5.715911918997225)
(0.95 5.755608605026871)
(0.975 5.767726690901246)
(0.99 5.788361205248892)
(0.999 5.862167787116697)

variable : sigma
0.8652179362171635: 0.11080101421251339
0.8314619930330885: 0.10741144355798032
0.8817171802272028: 0.10285704546048091
0.9010675240825037: 0.08576357731975093
0.9052013543625087: 0.0838311261590894
...
1.1797834852232982: 4.6785799373790374e-269
1.248041596168804: 2.519235350896405e-269
1.5967029501018613: 1.0796722932413166e-269
0.8816346359392639: 7.197815288275443e-270
1.4305392542276918: 3.5989076441377216e-270
mean: 0.8790479639414225
Percentiles:
(0.01 0.7356167812851002)
(0.025 0.7356167812851002)
(0.1 0.7885170911931328)
(0.05 0.7714614424537822)
(0.25 0.8062592655657621)
(0.5 0.8702936258681758)
(0.75 0.9157500673262436)
(0.84 0.933833505082263)
(0.9 0.933833505082263)
(0.95 0.9836058387509582)
(0.975 0.9840217569555448)
(0.99 1.0221113317178463)
(0.999 1.0992142252243495)

variable : post
5.721964741631715: 0.11080101421251339
4.449208257943409: 0.10741144355798032
5.5301219761330715: 0.10285704546048091
4.52051872813923: 0.08576357731975093
6.137938265961193: 0.0838311261590894
...
0.17122378231194002: 4.6785799373790374e-269
2.364667611499435: 2.519235350896405e-269
1.007309454814811: 1.0796722932413166e-269
7.815632506791646: 7.197815288275443e-270
-0.11124167745925273: 3.5989076441377216e-270
mean: 5.698111820515368
Percentiles:
(0.01 4.216035386449333)
(0.025 4.216035386449333)
(0.1 4.636483520779179)
(0.05 4.636483520779179)
(0.25 5.255531291735199)
(0.5 5.418506965559855)
(0.75 5.7733728614098725)
(0.84 6.216949007690968)
(0.9 6.244907659146807)
(0.95 6.640226691069286)
(0.975 6.640226691069286)
(0.99 6.640226691069286)
(0.999 6.640226691069286)

|#

(define sim (repeat (lambda () (sample (sampler->discrete-dist (model2 #t) 10))) 100))
(show "sim" sim)
(define (model3)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define mu (uniform 0 10))
   (define sigma (uniform 0 5))
   
   (for/list ([i (length sim)])
    (observe-sample (normal-dist mu sigma) (list-ref sim i)))

   (define post (normal mu sigma))
   (list mu
         sigma
          post
          )
   )
)

(displayln "\nModel 3")
(show-marginals (model3)
                (list  "mu"
                       "sigma"
                       "post"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


