#| 

  Test of my distributions in in Racket.Gamble 

  Here are some tests of the distributions in gamble_distributions.rkt


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999))

#|
  Laplace

laplace mu:10 b:3.4
Some samples
(5.72980483226096 10.265496315626526 5.151656688251884 15.524459729093806 18.039462158888583 5.840759902991554 6.480543520320399 19.229308507166504 -1.46152562948987 9.690545198137963)
(laplace_pdf 10 3.4 0 : 0.007765230931514782)
(laplace_pdf 10 3.4 0.5 : 0.008995415149867104)
(laplace_pdf 10 3.4 1.0 : 0.010420487739786237)
(laplace_pdf 10 3.4 1.5 : 0.01207132332704394)
(laplace_pdf 10 3.4 2.0 : 0.013983687760571597)
(laplace_pdf 10 3.4 2.5 : 0.016199012990321844)
(laplace_pdf 10 3.4 3.0 : 0.01876529470291102)
(laplace_pdf 10 3.4 3.5 : 0.02173813216258827)
(laplace_pdf 10 3.4 4.0 : 0.02518193278599826)
(laplace_pdf 10 3.4 4.5 : 0.02917130754821167)
(laplace_pdf 10 3.4 5.0 : 0.03379268745191424)
(laplace_pdf 10 3.4 5.5 : 0.039146196081045005)
(laplace_pdf 10 3.4 6.0 : 0.0453478188083208)
(laplace_pdf 10 3.4 6.5 : 0.0525319156531798)
(laplace_pdf 10 3.4 7.0 : 0.060854132231966195)
(laplace_pdf 10 3.4 7.5 : 0.0704947718669663)
(laplace_pdf 10 3.4 8.0 : 0.08166270191205155)
(laplace_pdf 10 3.4 8.5 : 0.09459987892664665)
(laplace_pdf 10 3.4 9.0 : 0.10958659073727654)
(laplace_pdf 10 3.4 9.5 : 0.12694752895753042)

(laplace_cdf 10 3.4 0 : 0.026401785167150256)
(laplace_cdf 10 3.4 0.5 : 0.030584411509548153)
(laplace_cdf 10 3.4 1.0 : 0.035429658315273206)
(laplace_cdf 10 3.4 1.5 : 0.0410424993119494)
(laplace_cdf 10 3.4 2.0 : 0.04754453838594343)
(laplace_cdf 10 3.4 2.5 : 0.05507664416709427)
(laplace_cdf 10 3.4 3.0 : 0.06380200198989747)
(laplace_cdf 10 3.4 3.5 : 0.07390964935280012)
(laplace_cdf 10 3.4 4.0 : 0.08561857147239409)
(laplace_cdf 10 3.4 4.5 : 0.09918244566391968)
(laplace_cdf 10 3.4 5.0 : 0.11489513733650841)
(laplace_cdf 10 3.4 5.5 : 0.13309706667555302)
(laplace_cdf 10 3.4 6.0 : 0.1541825839482907)
(laplace_cdf 10 3.4 6.5 : 0.1786085132208113)
(laplace_cdf 10 3.4 7.0 : 0.20690404958868505)
(laplace_cdf 10 3.4 7.5 : 0.2396822243476854)
(laplace_cdf 10 3.4 8.0 : 0.27765318650097526)
(laplace_cdf 10 3.4 8.5 : 0.3216395883505986)
(laplace_cdf 10 3.4 9.0 : 0.37259440850674025)
(laplace_cdf 10 3.4 9.5 : 0.4316215984556034)

(laplace_quantile 10 3.4 0.0001 : -18.95845685081521)
(laplace_quantile 10 3.4 0.001 : -11.129667534635452)
(laplace_quantile 10 3.4 0.01 : -3.3008782184556953)
(laplace_quantile 10 3.4 0.05 : 2.171210683820246)
(laplace_quantile 10 3.4 0.25 : 7.643299586096186)
(laplace_quantile 10 3.4 0.5 : 10.0)
(laplace_quantile 10 3.4 0.75 : 12.356700413903814)
(laplace_quantile 10 3.4 0.9 : 15.472088902275942)
(laplace_quantile 10 3.4 0.95 : 17.82878931617975)
(laplace_quantile 10 3.4 0.99 : 23.300878218455694)
(laplace_quantile 10 3.4 0.999 : 31.12966753463545)
(laplace_quantile 10 3.4 0.9999 : 38.95845685081558)
(laplace_quantile 10 3.4 0.99999 : 46.78724616701044)

laplace_mean 10 3.4: 10

|#
(displayln "Testing laplace")

(let ([mu 10]
      [b 3.4])
  (displayln (format "laplace mu:~a b:~a" mu b))
  (displayln "Some samples")
  (displayln (repeat (lambda () (laplace mu b)) 10))
  (for ([x (range 0 10 0.5)])
    (show2 "laplace_pdf" mu b x ":" (laplace_pdf mu b x))
    )
  (newline)
  (for ([x (range 0 10 0.5)])
    (show2 "laplace_cdf" mu b x ":" (laplace_cdf mu b x))
    )
  (newline)
  (for ([q ps])
    (show2 "laplace_quantile" mu b q ":" (laplace_quantile mu b q))
    )
  (newline)
  (displayln (format "laplace_mean ~a ~a: ~a" mu b (laplace_mean mu b)))
  )
(newline)

#|
  Extreme value dist1

Testing extreme_value_dist1
Samples:
(2.3069262312009475 1.2536931791869161 1.4884616859014437 1.0125817000230766 0.9102066813522441 -0.8050019824417939 0.37997516515670454 0.7483732394117593 -1.72537739527985 0.11586730447414498)


  Extreme value dist2

Testing extreme_value_dist2
extreme_value_dist2 a:1 b:2
(extreme_value_dist2_pdf  1 2 -10 : 6.595287109188505e-105)
(extreme_value_dist2_pdf  1 2 -8 : 3.62536528930063e-38)
(extreme_value_dist2_pdf  1 2 -6 : 6.87294137716775e-14)
(extreme_value_dist2_pdf  1 2 -4 : 3.1182885938309965e-5)
(extreme_value_dist2_pdf  1 2 -2 : 0.025353556804990365)
(extreme_value_dist2_pdf  1 2 0 : 0.15852096053897108)
(extreme_value_dist2_pdf  1 2 2 : 0.16535214944520904)
(extreme_value_dist2_pdf  1 2 4 : 0.08925325925656047)
(extreme_value_dist2_pdf  1 2 6 : 0.03780808995871326)
(extreme_value_dist2_pdf  1 2 8 : 0.014649566066640758)

(extreme_value_dist2_cdf  1 2 -10 : 5.390686197260365e-107)
(extreme_value_dist2_cdf  1 2 -8 : 8.054834089740903e-40)
(extreme_value_dist2_cdf  1 2 -6 : 4.150896920109045e-15)
(extreme_value_dist2_cdf  1 2 -4 : 5.119294298670733e-6)
(extreme_value_dist2_cdf  1 2 -2 : 0.011314286380459627)
(extreme_value_dist2_cdf  1 2 0 : 0.1922956455479649)
(extreme_value_dist2_cdf  1 2 2 : 0.545239211892605)
(extreme_value_dist2_cdf  1 2 4 : 0.8000107130043536)
(extreme_value_dist2_cdf  1 2 6 : 0.9211936551755158)
(extreme_value_dist2_cdf  1 2 8 : 0.9702540025910624)

(extreme_value_dist2_quantile  1 2 0.0001 : -3.4406536127356926)
(extreme_value_dist2_quantile  1 2 0.001 : -2.865289467832131)
(extreme_value_dist2_quantile  1 2 0.01 : -2.0543592516158022)
(extreme_value_dist2_quantile  1 2 0.05 : -1.1943774007298975)
(extreme_value_dist2_quantile  1 2 0.25 : 0.3467314800434381)
(extreme_value_dist2_quantile  1 2 0.5 : 1.7330258411633288)
(extreme_value_dist2_quantile  1 2 0.75 : 3.4917986474144764)
(extreme_value_dist2_quantile  1 2 0.9 : 5.500734654624891)
(extreme_value_dist2_quantile  1 2 0.95 : 6.940390498084327)
(extreme_value_dist2_quantile  1 2 0.99 : 10.200298453553158)
(extreme_value_dist2_quantile  1 2 0.999 : 14.814510141047432)
(extreme_value_dist2_quantile  1 2 0.9999 : 19.42058073978567)
(extreme_value_dist2_quantile  1 2 0.99999 : 24.025840929907893)

extreme_value_dist2_mean 1 2: 2.1544313298030655


|#
(displayln "Testing extreme_value_dist1")
(displayln "Samples:")
(displayln (repeat (lambda () (extreme_value_dist1)) 10))

(newline)
(displayln "Testing extreme_value_dist2")
(let ([a 1]
      [b 2])
  (displayln (format "extreme_value_dist2 a:~a b:~a" a b))
  
  (for ([q (range -10 10 2)])
    (let ([v (extreme_value_dist2_pdf a b q)])
      (show2 "extreme_value_dist2_pdf " a b q ":" v)
      )
    )
  (newline)
  (for ([q (range -10 10 2)])
    (let ([v (extreme_value_dist2_cdf a b q) ])
      (show2 "extreme_value_dist2_cdf " a b q ":" v)
      )
    )
  (newline)
  (for ([q ps])
    (let ([v (extreme_value_dist2_quantile a b q)])
      (show2 "extreme_value_dist2_quantile " a b q ":" v)
      )
    )
  (newline)
  (displayln (format "extreme_value_dist2_mean ~a ~a: ~a" a b (extreme_value_dist2_mean a b)))
  
  )
(newline)

#|
  Generalized extreme value distribution

xi: -1
generalized_extreme_value mu:2 sigma:2 xi:-1
(generalized_extreme_value_pdf  2 2 -1 -5 : 0.005554498269121153)
(generalized_extreme_value_pdf  2 2 -1 -3 : 0.01509869171115925)
(generalized_extreme_value_pdf  2 2 -1 -1 : 0.0410424993119494)
(generalized_extreme_value_pdf  2 2 -1 1 : 0.11156508007421491)
(generalized_extreme_value_pdf  2 2 -1 3 : 0.3032653298563167)

(generalized_extreme_value_cdf  2 2 -1 -5 : 0.011108996538242306)
(generalized_extreme_value_cdf  2 2 -1 -3 : 0.0301973834223185)
(generalized_extreme_value_cdf  2 2 -1 -1 : 0.0820849986238988)
(generalized_extreme_value_cdf  2 2 -1 1 : 0.22313016014842982)
(generalized_extreme_value_cdf  2 2 -1 3 : 0.6065306597126334)

(generalized_extreme_value_quantile  2 2 0.0001 -1 : -14.420680743952364)
(generalized_extreme_value_quantile  2 2 0.001 -1 : -9.815510557964274)
(generalized_extreme_value_quantile  2 2 0.01 -1 : -5.210340371976182)
(generalized_extreme_value_quantile  2 2 0.05 -1 : -1.9914645471079817)
(generalized_extreme_value_quantile  2 2 0.25 -1 : 1.2274112777602189)
(generalized_extreme_value_quantile  2 2 0.5 -1 : 2.613705638880109)
(generalized_extreme_value_quantile  2 2 0.75 -1 : 3.4246358550964384)
(generalized_extreme_value_quantile  2 2 0.9 -1 : 3.7892789686843473)
(generalized_extreme_value_quantile  2 2 0.95 -1 : 3.897413411224899)
(generalized_extreme_value_quantile  2 2 0.99 -1 : 3.979899328292997)
(generalized_extreme_value_quantile  2 2 0.999 -1 : 3.997998999332833)
(generalized_extreme_value_quantile  2 2 0.9999 -1 : 3.9997999899993335)
(generalized_extreme_value_quantile  2 2 0.99999 -1 : 3.9999799998999994)

generalized_extreme_value_mean 2 2 -1: 2

xi: 0
generalized_extreme_value mu:2 sigma:2 xi:0
(generalized_extreme_value_pdf  2 2 0 -5 : 6.872941377167748e-14)
(generalized_extreme_value_pdf  2 2 0 -3 : 3.1182885938309965e-5)
(generalized_extreme_value_pdf  2 2 0 -1 : 0.025353556804990365)
(generalized_extreme_value_pdf  2 2 0 1 : 0.15852096053897108)
(generalized_extreme_value_pdf  2 2 0 3 : 0.16535214944520904)

(generalized_extreme_value_cdf  2 2 0 -5 : 4.150896920109045e-15)
(generalized_extreme_value_cdf  2 2 0 -3 : 5.119294298670733e-6)
(generalized_extreme_value_cdf  2 2 0 -1 : 0.011314286380459627)
(generalized_extreme_value_cdf  2 2 0 1 : 0.1922956455479649)
(generalized_extreme_value_cdf  2 2 0 3 : 0.545239211892605)

(generalized_extreme_value_quantile  2 2 0.0001 0 : -2.4406536127356926)
(generalized_extreme_value_quantile  2 2 0.001 0 : -1.865289467832131)
(generalized_extreme_value_quantile  2 2 0.01 0 : -1.0543592516158022)
(generalized_extreme_value_quantile  2 2 0.05 0 : -0.1943774007298975)
(generalized_extreme_value_quantile  2 2 0.25 0 : 1.346731480043438)
(generalized_extreme_value_quantile  2 2 0.5 0 : 2.733025841163329)
(generalized_extreme_value_quantile  2 2 0.75 0 : 4.491798647414477)
(generalized_extreme_value_quantile  2 2 0.9 0 : 6.500734654624891)
(generalized_extreme_value_quantile  2 2 0.95 0 : 7.940390498084327)
(generalized_extreme_value_quantile  2 2 0.99 0 : 11.200298453553158)
(generalized_extreme_value_quantile  2 2 0.999 0 : 15.814510141047432)
(generalized_extreme_value_quantile  2 2 0.9999 0 : 20.42058073978567)
(generalized_extreme_value_quantile  2 2 0.99999 0 : 25.025840929907893)

generalized_extreme_value_mean 2 2 0: 3.1544313298030655

xi: 0.99
generalized_extreme_value mu:2 sigma:2 xi:0.99
(generalized_extreme_value_pdf  2 2 0.99 -5 : 0)
(generalized_extreme_value_pdf  2 2 0.99 -3 : 0)
(generalized_extreme_value_pdf  2 2 0.99 -1 : 0)
(generalized_extreme_value_pdf  2 2 0.99 1 : 0.26880660812775203)
(generalized_extreme_value_pdf  2 2 0.99 3 : 0.11444666989806275)

(generalized_extreme_value_cdf  2 2 0.99 -5 : 0)
(generalized_extreme_value_cdf  2 2 0.99 -3 : 0)
(generalized_extreme_value_cdf  2 2 0.99 -1 : 0)
(generalized_extreme_value_cdf  2 2 0.99 1 : 0.13616191036500847)
(generalized_extreme_value_cdf  2 2 0.99 3 : 0.5136645492754226)

(generalized_extreme_value_quantile  2 2 0.0001 0.99 : 0.20406317462994594)
(generalized_extreme_value_quantile  2 2 0.001 0.99 : 0.2779592480682682)
(generalized_extreme_value_quantile  2 2 0.01 0.99 : 0.4252301437486805)
(generalized_extreme_value_quantile  2 2 0.05 0.99 : 0.661597722302951)
(generalized_extreme_value_quantile  2 2 0.25 0.99 : 1.441833415755584)
(generalized_extreme_value_quantile  2 2 0.5 0.99 : 2.8836708188230045)
(generalized_extreme_value_quantile  2 2 0.75 0.99 : 6.915191851247599)
(generalized_extreme_value_quantile  2 2 0.9 0.99 : 18.727312232765755)
(generalized_extreme_value_quantile  2 2 0.95 0.99 : 38.21248431494493)
(generalized_extreme_value_quantile  2 2 0.99 0.99 : 191.95097689091884)
(generalized_extreme_value_quantile  2 2 0.999 0.99 : 1884.4086095567936)
(generalized_extreme_value_quantile  2 2 0.9999 0.99 : 18423.529172842984)
(generalized_extreme_value_quantile  2 2 0.99999 0.99 : 180049.78311881903)

generalized_extreme_value_mean 2 2 0.99: 200.85370731141518


|#
(displayln "Testing generalized_extreme_value_dist")
(let ([mu 2]
      [sigma 2])
  (for ([xi '(-1 0 0.99)])
    (show "xi" xi)
    (displayln (format "generalized_extreme_value mu:~a sigma:~a xi:~a" mu sigma xi))
    
    (for ([x (range -5 5 2)])
      (let ([v (generalized_extreme_value_pdf mu sigma xi x)])
        (show2 "generalized_extreme_value_pdf " mu sigma xi x ":" v)
        )
      )
    (newline)
    (for ([x (range -5 5 2)])
      (let ([v (generalized_extreme_value_cdf mu sigma xi x) ])
        (show2 "generalized_extreme_value_cdf " mu sigma xi x ":" v)
        )
      )
    (newline)
    (for ([q ps])
      (let ([v (generalized_extreme_value_quantile mu sigma xi q)])
        (show2 "generalized_extreme_value_quantile " mu sigma q xi ":" v)
        )
      )
    (newline)
    (displayln (format "generalized_extreme_value_mean ~a ~a ~a: ~a" mu sigma xi (generalized_extreme_value_mean mu sigma xi)))
    (newline)
    )
  )
(newline)

#|
  Bernoulli

Testing Bernoulli
bernoulli_dist p:4/5
(bernoulli_dist_pdf  4/5 0 : 1/5)
(bernoulli_dist_pdf  4/5 1 : 4/5)

(bernoulli_dist_cdf  4/5 0 : 1/5)
(bernoulli_dist_cdf  4/5 1 : 1/5)

(bernoulli_dist_quantile  4/5 0.0001 : 0)
(bernoulli_dist_quantile  4/5 0.001 : 0)
(bernoulli_dist_quantile  4/5 0.01 : 0)
(bernoulli_dist_quantile  4/5 0.05 : 0)
(bernoulli_dist_quantile  4/5 0.25 : 1)
(bernoulli_dist_quantile  4/5 0.5 : 1)
(bernoulli_dist_quantile  4/5 0.75 : 1)
(bernoulli_dist_quantile  4/5 0.9 : 1)
(bernoulli_dist_quantile  4/5 0.95 : 1)
(bernoulli_dist_quantile  4/5 0.99 : 1)
(bernoulli_dist_quantile  4/5 0.999 : 1)
(bernoulli_dist_quantile  4/5 0.9999 : 1)
(bernoulli_dist_quantile  4/5 0.99999 : 1)


|#
(displayln "\nTesting Bernoulli")
(let ([p 8/10])
  (displayln (format "bernoulli_dist p:~a" p))

  (for ([q '(0 1)])
    (let ([v (bernoulli_dist_pdf p q)])
      (show2 "bernoulli_dist_pdf " p q ":" v)
      )
    )
  (newline)
  (for ([q '(0 1)])
    (let ([v (bernoulli_dist_cdf p q)])
      (show2 "bernoulli_dist_cdf " p q ":" v)
      )
    )
  (newline)
  (for ([q ps])
    (let ([v (bernoulli_dist_quantile p q)])
      (show2 "bernoulli_dist_quantile " p q ":" v)
      )
    )
  )

#|
  Binomial

binomial_dist n:10 p:4/5
(binomial_dist_pdf  10 4/5 0 : 1/9765625 ( 1.024e-7 ))
(binomial_dist_pdf  10 4/5 1 : 8/1953125 ( 4.096e-6 ))
(binomial_dist_pdf  10 4/5 2 : 144/1953125 ( 7.3728e-5 ))
(binomial_dist_pdf  10 4/5 3 : 1536/1953125 ( 0.000786432 ))
(binomial_dist_pdf  10 4/5 4 : 10752/1953125 ( 0.005505024 ))
(binomial_dist_pdf  10 4/5 5 : 258048/9765625 ( 0.0264241152 ))
(binomial_dist_pdf  10 4/5 6 : 172032/1953125 ( 0.088080384 ))
(binomial_dist_pdf  10 4/5 7 : 393216/1953125 ( 0.201326592 ))
(binomial_dist_pdf  10 4/5 8 : 589824/1953125 ( 0.301989888 ))
(binomial_dist_pdf  10 4/5 9 : 524288/1953125 ( 0.268435456 ))
(binomial_dist_pdf  10 4/5 10 : 1048576/9765625 ( 0.1073741824 ))

(binomial_dist_cdf  10 4/5 0 : 1/9765625 ( 1.024e-7 ))
(binomial_dist_cdf  10 4/5 1 : 41/9765625 ( 4.1984e-6 ))
(binomial_dist_cdf  10 4/5 2 : 761/9765625 ( 7.79264e-5 ))
(binomial_dist_cdf  10 4/5 3 : 8441/9765625 ( 0.0008643584 ))
(binomial_dist_cdf  10 4/5 4 : 62201/9765625 ( 0.0063693824 ))
(binomial_dist_cdf  10 4/5 5 : 320249/9765625 ( 0.0327934976 ))
(binomial_dist_cdf  10 4/5 6 : 1180409/9765625 ( 0.1208738816 ))
(binomial_dist_cdf  10 4/5 7 : 3146489/9765625 ( 0.3222004736 ))
(binomial_dist_cdf  10 4/5 8 : 6095609/9765625 ( 0.6241903616 ))
(binomial_dist_cdf  10 4/5 9 : 8717049/9765625 ( 0.8926258176 ))
(binomial_dist_cdf  10 4/5 10 : 1 ( 1.0 ))

(binomial_dist_quantile  10 4/5 0.0001 : 3)
(binomial_dist_quantile  10 4/5 0.001 : 4)
(binomial_dist_quantile  10 4/5 0.01 : 5)
(binomial_dist_quantile  10 4/5 0.05 : 6)
(binomial_dist_quantile  10 4/5 0.25 : 7)
(binomial_dist_quantile  10 4/5 0.5 : 8)
(binomial_dist_quantile  10 4/5 0.75 : 9)
(binomial_dist_quantile  10 4/5 0.9 : 10)
(binomial_dist_quantile  10 4/5 0.95 : 10)
(binomial_dist_quantile  10 4/5 0.99 : 10)
(binomial_dist_quantile  10 4/5 0.999 : 10)
(binomial_dist_quantile  10 4/5 0.9999 : 10)
(binomial_dist_quantile  10 4/5 0.99999 : 10)

binomial_dist_mean 10 4/5: 8

|#
(displayln "\nTesting Binomial")
(let ([n 10]
      [p 8/10])
  (displayln (format "binomial_dist n:~a p:~a" n p))

  (for ([q (add1 n)])
    (let ([v (binomial_dist_pdf n p q)])
      (show2 "binomial_dist_pdf " n p q ":" v "(" (* 1.0 v) ")" )
      )
    )
  (newline)
  (for ([q (add1 n)])
    (let ([v (binomial_dist_cdf n p q)])
      (show2 "binomial_dist_cdf " n p q ":" v "(" (* 1.0 v) ")" )
      )
    )
  (newline)
  (for ([q ps])
    (let ([v (binomial_dist_quantile n p q)])
      (show2 "binomial_dist_quantile " n p q ":" v)
      )
    )
  (newline)
  (displayln (format "binomial_dist_mean ~a ~a: ~a" n p (binomial_dist_mean n p)))
  
  )

#|
  Hypergeometric
  - dist
  - pdf
  - cdf
  - quantile

  For a Mathematica compliant variant, see hypergeometric2 below.

Using (hypergeometric (N: total number of objects
                            K: total number of success objects
                            n: number of draws
                            k: number of successes

(K: 5 N: 50 k: 4 n: 10)
((hypergeometric_pdf 50 5 10 0 ): 82251/264845 :( 0.3105627820045687 ))
((hypergeometric_pdf 50 5 10 1 ): 45695/105938 :( 0.43133719722856767 ))
((hypergeometric_pdf 50 5 10 2 ): 11115/52969 :( 0.20983971757065453 ))
((hypergeometric_pdf 50 5 10 3 ): 2340/52969 :( 0.04417678264645358 ))
((hypergeometric_pdf 50 5 10 4 ): 30/7567 :( 0.003964583058015066 ))
((hypergeometric_pdf 50 5 10 5 ): 9/75670 :( 0.00011893749174045196 ))

((hypergeometric_cdf 50 5 10 0 ): 82251/264845 :( 0.3105627820045687 ))
((hypergeometric_cdf 50 5 10 1 ): 392977/529690 :( 0.7418999792331363 ))
((hypergeometric_cdf 50 5 10 2 ): 504127/529690 :( 0.9517396968037909 ))
((hypergeometric_cdf 50 5 10 3 ): 75361/75670 :( 0.9959164794502445 ))
((hypergeometric_cdf 50 5 10 4 ): 75661/75670 :( 0.9998810625082596 ))
((hypergeometric_cdf 50 5 10 5 ): 1 :( 1.0 ))

((hypergeometric_quantile 50 5 10 0.0001 ): 0)
((hypergeometric_quantile 50 5 10 0.001 ): 0)
((hypergeometric_quantile 50 5 10 0.01 ): 0)
((hypergeometric_quantile 50 5 10 0.05 ): 0)
((hypergeometric_quantile 50 5 10 0.25 ): 0)
((hypergeometric_quantile 50 5 10 0.5 ): 1)
((hypergeometric_quantile 50 5 10 0.75 ): 2)
((hypergeometric_quantile 50 5 10 0.9 ): 2)
((hypergeometric_quantile 50 5 10 0.95 ): 2)
((hypergeometric_quantile 50 5 10 0.99 ): 3)
((hypergeometric_quantile 50 5 10 0.999 ): 4)
((hypergeometric_quantile 50 5 10 0.9999 ): 5)
((hypergeometric_quantile 50 5 10 0.99999 ): 5)


|#
(displayln "\nTesting Hypergeometric")
(displayln  "Using (hypergeometric (N: total number of objects
                            K: total number of success objects
                            n: number of draws
                            k: number of successes")

(let ((K 5)  ;; total green marbles: 4 drawn + 1 not drawn
      (N 50) ;; total marbles: 5 green + 45 red marbles
      (k 4)  ;; drawn 4 green_marbles
      ; (k 5) ;; drawn 5 green_marbles
      (n 10))   ;; total drawn green + red marbles
  (show2 "K:" K "N:" N "k:" k "n:" n)
  
  (for* ([kk 6])
    (let ([res (hypergeometric_pdf N K n kk)])
      (show2 "(hypergeometric_pdf" N K n kk "):" res ":(" (exact->inexact res) ")")))
  (newline)
  (for* ([kk 6])
    (let ([res (hypergeometric_cdf N K n kk)])
      (show2 "(hypergeometric_cdf" N K n kk "):" res ":(" (exact->inexact res) ")")))  
  (newline)
  
  (for ([p ps])
    (show2 "(hypergeometric_quantile" N K n p "):" (hypergeometric_quantile N K n p)))

  (newline)
  )
(newline)


#|
  Hypergeometric2 

  This is the Mathematica compliant version.

((hypergeometric2 num-drawn: 10 num-successes: 40 num-objects: 100)

Frequency of 1000 random samples
(4 : 0.2606541129831516)
(3 : 0.22001982160555006)
(5 : 0.17443012884043607)
(6 : 0.13478691774033696)
(2 : 0.1110009910802775)
(1 : 0.04261645193260654)
(7 : 0.04162537165510406)
(8 : 0.009910802775024777)
(0 : 0.004955401387512388)
(mean: 4.0198216055500495)

Percentiles (random samples)

Exact calculations:
((hypergeometric2_pdf 10 40 100 0 ): 13874499/3185555660 ( 0.004355440771045891 ))
((hypergeometric2_pdf 10 40 100 1 ): 5440980/159277783 ( 0.03416031977290895 ))
((hypergeometric2_pdf 10 40 100 2 ): 36726615/318555566 ( 0.11529107923356768 ))
((hypergeometric2_pdf 10 40 100 3 ): 35109720/159277783 ( 0.22043074268556337 ))
((hypergeometric2_pdf 10 40 100 4 ): 6014165/22753969 ( 0.2643127886831524 ))
((hypergeometric2_pdf 10 40 100 5 ): 259811928/1251468295 ( 0.20760568129294876 ))
((hypergeometric2_pdf 10 40 100 6 ): 54127485/500587318 ( 0.10812795900674416 ))
((hypergeometric2_pdf 10 40 100 7 ): 64573140/1752055613 ( 0.03685564517523109 ))
((hypergeometric2_pdf 10 40 100 8 ): 5009985/637111132 ( 0.00786359670764629 ))
((hypergeometric2_pdf 10 40 100 9 ): 150960/159277783 ( 0.0009477781342549199 ))
((hypergeometric2_pdf 10 40 100 10 ): 1258/25689965 ( 4.896853693650419e-5 ))

((hypergeometric2_cdf 10 40 100 0 ): 13874499/3185555660 ( 0.004355440771045891 ))
((hypergeometric2_cdf 10 40 100 1 ): 122694099/3185555660 ( 0.038515760543954836 ))
((hypergeometric2_cdf 10 40 100 2 ): 489960249/3185555660 ( 0.15380683977752252 ))
((hypergeometric2_cdf 10 40 100 3 ): 170307807/455079380 ( 0.3742375824630859 ))
((hypergeometric2_cdf 10 40 100 4 ): 290591107/455079380 ( 0.6385503711462382 ))
((hypergeometric2_cdf 10 40 100 5 ): 605107127/715124740 ( 0.846156052439187 ))
((hypergeometric2_cdf 10 40 100 6 ): 4777024739/5005873180 ( 0.9542840114459312 ))
((hypergeometric2_cdf 10 40 100 7 ): 137275241/138502420 ( 0.9911396566211623 ))
((hypergeometric2_cdf 10 40 100 8 ): 795595117/796388915 ( 0.9990032533288086 ))
((hypergeometric2_cdf 10 40 100 9 ): 25688707/25689965 ( 0.9999510314630635 ))
((hypergeometric2_cdf 10 40 100 10 ): 1 ( 1.0 ))

((hypergeometric2_quantile 10 40 100 0.0001 ): 0)
((hypergeometric2_quantile 10 40 100 0.001 ): 0)
((hypergeometric2_quantile 10 40 100 0.01 ): 1)
((hypergeometric2_quantile 10 40 100 0.05 ): 2)
((hypergeometric2_quantile 10 40 100 0.25 ): 3)
((hypergeometric2_quantile 10 40 100 0.5 ): 4)
((hypergeometric2_quantile 10 40 100 0.75 ): 5)
((hypergeometric2_quantile 10 40 100 0.9 ): 6)
((hypergeometric2_quantile 10 40 100 0.95 ): 6)
((hypergeometric2_quantile 10 40 100 0.99 ): 7)
((hypergeometric2_quantile 10 40 100 0.999 ): 8)
((hypergeometric2_quantile 10 40 100 0.9999 ): 9)
((hypergeometric2_quantile 10 40 100 0.99999 ): 10)

hypergeometric2_mean 10 40 100: 4


|#
(displayln "\nTesting Hypergeometric2")
(let ({num-drawn 10}
      [num-successes 40]
      [num-objects 100])
  (show2 "(hypergeometric2 num-drawn:" num-drawn "num-successes:" num-successes "num-objects:" num-objects)
  (newline)
  
  
  (displayln "Frequency of 1000 random samples")
  (show-freq (repeat (lambda () (hypergeometric2 num-drawn num-successes num-objects)) 1000))
  
  (displayln "\nPercentiles (random samples)")
  (percentiles (repeat (lambda () (hypergeometric2 num-drawn num-successes num-objects)) 1000) ps)
  (newline)
  
  (displayln "\nExact calculations:")
  (for ([n 11])
    (let ([res (hypergeometric2_pdf num-drawn num-successes num-objects n)])
      (show2 "(hypergeometric2_pdf" num-drawn num-successes num-objects n "):" res "(" (exact->inexact res) ")")
      )
    )
  (newline)
  (for ([n 11])
    (let ([res (hypergeometric2_cdf num-drawn num-successes num-objects n)])
      (show2 "(hypergeometric2_cdf" num-drawn num-successes num-objects n "):" res "(" (exact->inexact res) ")")
      )
    )
  (newline)
  (for ([p ps])
    (let ([res (hypergeometric2_quantile num-drawn num-successes num-objects p)])
      (show2 "(hypergeometric2_quantile" num-drawn num-successes num-objects p "):" res)
      )
    )
  (newline)
  (displayln (format "hypergeometric2_mean ~a ~a ~a: ~a" num-drawn num-successes num-objects (hypergeometric2_mean num-drawn num-successes num-objects)))
  )


#|
  Negative binomial
(negative_binomial n p)
(negative_binomial n: 3 p: 0.7)
((negative_binomial_pdf 3 0.7 0 ): 0.3429999999999999)
((negative_binomial_pdf 3 0.7 1 ): 0.3087)
((negative_binomial_pdf 3 0.7 2 ): 0.18522)
((negative_binomial_pdf 3 0.7 3 ): 0.09261000000000003)
((negative_binomial_pdf 3 0.7 4 ): 0.04167450000000002)
((negative_binomial_pdf 3 0.7 5 ): 0.017503290000000008)
((negative_binomial_pdf 3 0.7 6 ): 0.007001316000000005)
((negative_binomial_pdf 3 0.7 7 ): 0.002700507600000002)
((negative_binomial_pdf 3 0.7 8 ): 0.0010126903500000009)
((negative_binomial_pdf 3 0.7 9 ): 0.00037131979500000035)
((negative_binomial_pdf 3 0.7 10 ): 0.00013367512620000017)
((negative_binomial_pdf 3 0.7 11 ): 4.739390838000007e-5)
((negative_binomial_pdf 3 0.7 12 ): 1.6587867933000026e-5)

((negative_binomial_cdf 3 0.7 0 ): 0.3429999999999999)
((negative_binomial_cdf 3 0.7 1 ): 0.6517)
((negative_binomial_cdf 3 0.7 2 ): 0.8369199999999999)
((negative_binomial_cdf 3 0.7 3 ): 0.92953)
((negative_binomial_cdf 3 0.7 4 ): 0.9712045)
((negative_binomial_cdf 3 0.7 5 ): 0.9887077900000001)
((negative_binomial_cdf 3 0.7 6 ): 0.9957091060000001)
((negative_binomial_cdf 3 0.7 7 ): 0.9984096136)
((negative_binomial_cdf 3 0.7 8 ): 0.99942230395)
((negative_binomial_cdf 3 0.7 9 ): 0.999793623745)
((negative_binomial_cdf 3 0.7 10 ): 0.9999272988712)
((negative_binomial_cdf 3 0.7 11 ): 0.9999746927795801)
((negative_binomial_cdf 3 0.7 12 ): 0.9999912806475131)

((negative_binomial_quantile 3 0.7 0.0001 ): 0)
((negative_binomial_quantile 3 0.7 0.001 ): 0)
((negative_binomial_quantile 3 0.7 0.01 ): 0)
((negative_binomial_quantile 3 0.7 0.05 ): 0)
((negative_binomial_quantile 3 0.7 0.5 ): 1)
((negative_binomial_quantile 3 0.7 0.9 ): 3)
((negative_binomial_quantile 3 0.7 0.95 ): 4)
((negative_binomial_quantile 3 0.7 0.99 ): 6)
((negative_binomial_quantile 3 0.7 0.999 ): 8)
((negative_binomial_quantile 3 0.7 0.9999 ): 10)
((negative_binomial_quantile 3 0.7 0.99999 ): 12)

negative_binomial_mean 3 0.7: 1.285714285714286

|#
(displayln "\nNegative binomial")
(displayln "(negative_binomial n p)")

(let ([n 3]
      [p 0.7])
  (show2 "negative_binomial n:" n "p:" p)
  (for ([k 13])
    (show2 "(negative_binomial_pdf" n p k "):" (negative_binomial_pdf n p k))
    )
  (newline)
  (for ([k 13])
    (show2 "(negative_binomial_cdf" n p k "):" (negative_binomial_cdf n p k))
    )
  (newline)
  (define ps '(0.0001 0.001 0.01 0.05  0.5 0.9 0.95 0.99 0.999 0.9999 0.99999))
  (for ([q ps])
    (show2 "(negative_binomial_quantile" n p q "):" (negative_binomial_quantile n p q))
    )
  (newline)
  (displayln (format "negative_binomial_mean ~a ~a: ~a" n p (negative_binomial_mean n p)))
  )
(newline)

#|
  Beta-binomial 

(beta_binomial n a b)
(beta_binomial n: 12 a: 10 b: 3)
((9 8 5 9 6 12 10 7 10 11))
((beta_binomial_pdf 12 10 3 0 ): 1/29716 3.3651904697805894e-5)
((beta_binomial_pdf 12 10 3 1 ): 15/52003 0.00028844489740976483)
((beta_binomial_pdf 12 10 3 2 ): 1815/1352078 0.0013423781764069825)
((beta_binomial_pdf 12 10 3 3 ): 3025/676039 0.004474593921356608)
((beta_binomial_pdf 12 10 3 4 ): 2475/208012 0.0118983520181528)
((beta_binomial_pdf 12 10 3 5 ): 198/7429 0.02665230852066227)
((beta_binomial_pdf 12 10 3 6 ): 385/7429 0.05182393323462108)
((beta_binomial_pdf 12 10 3 7 ): 660/7429 0.08884102840220756)
((beta_binomial_pdf 12 10 3 8 ): 825/6118 0.13484798953906504)
((beta_binomial_pdf 12 10 3 9 ): 550/3059 0.17979731938542007)
((beta_binomial_pdf 12 10 3 10 ): 33/161 0.20496894409937888)
((beta_binomial_pdf 12 10 3 11 ): 30/161 0.18633540372670807)
((beta_binomial_pdf 12 10 3 12 ): 5/46 0.10869565217391304)

((beta_binomial_cdf 12 10 3 0 ): 1/29716 3.3651904697805894e-5)
((beta_binomial_cdf 12 10 3 1 ): 67/208012 0.0003220968021075707)
((beta_binomial_cdf 12 10 3 2 ): 643/386308 0.0016644749785145532)
((beta_binomial_cdf 12 10 3 3 ): 1277/208012 0.006139068899871161)
((beta_binomial_cdf 12 10 3 4 ): 134/7429 0.01803742091802396)
((beta_binomial_cdf 12 10 3 5 ): 332/7429 0.04468972943868623)
((beta_binomial_cdf 12 10 3 6 ): 717/7429 0.09651366267330731)
((beta_binomial_cdf 12 10 3 7 ): 81/437 0.1853546910755149)
((beta_binomial_cdf 12 10 3 8 ): 1959/6118 0.3202026806145799)
((beta_binomial_cdf 12 10 3 9 ): 1/2 0.5)
((beta_binomial_cdf 12 10 3 10 ): 227/322 0.7049689440993789)
((beta_binomial_cdf 12 10 3 11 ): 41/46 0.8913043478260869)
((beta_binomial_cdf 12 10 3 12 ): 1 1.0)

((beta_binomial_quantile 12 10 3 0.0001 ): 1)
((beta_binomial_quantile 12 10 3 0.001 ): 2)
((beta_binomial_quantile 12 10 3 0.01 ): 4)
((beta_binomial_quantile 12 10 3 0.05 ): 6)
((beta_binomial_quantile 12 10 3 0.5 ): 9)
((beta_binomial_quantile 12 10 3 0.9 ): 12)
((beta_binomial_quantile 12 10 3 0.95 ): 12)
((beta_binomial_quantile 12 10 3 0.99 ): 12)
((beta_binomial_quantile 12 10 3 0.999 ): 12)
((beta_binomial_quantile 12 10 3 0.9999 ): 12)
((beta_binomial_quantile 12 10 3 0.99999 ): 12)

beta_binomial_mean 12 10 3: 120/13

|#
(displayln "\nBeta binomial")
(displayln "(beta_binomial n a b)")

(let ([n 12]
      [a 10]
      [b 3])
  
  (show2 "beta_binomial n:" n "a:" a "b:" b)
  (show2 (for/list ([i 10]) (beta_binomial n a b)))
  (for ([k 13])
    (let ([v (beta_binomial_pdf n a b k)])
      (show2 "(beta_binomial_pdf" n a b k "):" v (* 1.0 v))
      )
    )
  (newline)
  (for* ([k 13])
    (let ([v (beta_binomial_cdf n a b k)])
      (show2 "(beta_binomial_cdf" n a b k "):" v (* 1.0 v))
      )
    )
  (newline)
  (define ps '(0.0001 0.001 0.01 0.05  0.5 0.9 0.95 0.99 0.999 0.9999 0.99999))
  (for ([q ps])
    (show2 "(beta_binomial_quantile" n a b q "):" (beta_binomial_quantile n a b q))
    )
  (newline)
  (displayln (format "beta_binomial_mean ~a ~a ~a: ~a" n a b (beta_binomial_mean n a b)))  
  )

(newline)

#|
  Multinomial distribution

Samples:
((0 4 1) : 0.23471400394477318)
((0 5 0) : 0.16962524654832348)
((1 3 1) : 0.15779092702169625)
((0 3 2) : 0.1282051282051282)
((1 4 0) : 0.10552268244575937)
((1 2 2) : 0.07396449704142012)
((0 2 3) : 0.038461538461538464)
((2 3 0) : 0.03747534516765286)
((2 2 1) : 0.021696252465483234)
((2 1 2) : 0.009861932938856016)
((1 1 3) : 0.008875739644970414)
((3 2 0) : 0.006903353057199211)
((0 1 4) : 0.004930966469428008)
((3 1 1) : 0.0019723865877712033)

(multinomial_pdf 10 1/2 1/3   4 6): 35/1944
(multinomial_mean 10 1/2 1/3): (5 10/3)
(multinomial_variance 10 1/2 1/3): (5/2 20/9)

(multinomial_pdf 10 2/10 8/10   3 7): 393216/1953125
(multinomial_mean 10 2/10 8/10): (2 8)
(multinomial_variance 10 2/10 8/10): (8/5 8/5)

(multinomial_pdf 10 1/2 1/3 1/6 8 1 1): 5/256
(multinomial_mean 10 (1/2 1/3 1/6) (8 1 1)): (5 10/3 5/3)
(multinomial_variance 10 (1/2 1/3 1/6) (8 1 1)): (5/2 20/9 25/18)

(multinomial_pdf 10 (1/2 1/3 1/12 1/12) (6 2 1 1)): 35/1152
(multinomial_mean 10 (1/2 1/3 1/12 1/12)): (5 10/3 5/6 5/6)
(multinomial_variance 10 (1/2 1/3 1/12 1/12)): (5/2 20/9 55/72 55/72)


|#
(displayln "Testing multinomial_dist")
(displayln "Samples:")
(show-freq (repeat (lambda () (multinomial_dist 5 '(7/10 2/10 1/10))) 1000))
(show "(multinomial_pdf 10 1/2 1/3   4 6)" (multinomial_pdf 10 (list 1/2 1/3) (list 4 6)))
(show "(multinomial_mean 10 1/2 1/3)" (multinomial_mean 10 (list 1/2 1/3)))
(show "(multinomial_variance 10 1/2 1/3)" (multinomial_variance 10 (list 1/2 1/3)))
(newline)
(show "(multinomial_pdf 10 2/10 8/10   3 7)" (multinomial_pdf 10 (list 2/10 8/10)   (list 3 7)))
(show "(multinomial_mean 10 2/10 8/10)" (multinomial_mean 10 (list 2/10 8/10)))
(show "(multinomial_variance 10 2/10 8/10)" (multinomial_variance 10 (list 2/10 8/10)))

(newline)
(show "(multinomial_pdf 10 1/2 1/3 1/6 8 1 1)" (multinomial_pdf 10 (list 1/2 1/3 1/6) (list 8 1 1)))
(show "(multinomial_mean 10 (1/2 1/3 1/6) (8 1 1))" (multinomial_mean 10 (list 1/2 1/3 1/6)))
(show "(multinomial_variance 10 (1/2 1/3 1/6) (8 1 1))" (multinomial_variance 10 (list 1/2 1/3 1/6)))
(newline)
(show "(multinomial_pdf 10 (1/2 1/3 1/12 1/12) (6 2 1 1))" (multinomial_pdf 10 (list 1/2 1/3 1/12 1/12) (list 6 2 1 1)))
(show "(multinomial_mean 10 (1/2 1/3 1/12 1/12))" (multinomial_mean 10 (list 1/2 1/3 1/12 1/12)))
(show "(multinomial_variance 10 (1/2 1/3 1/12 1/12))" (multinomial_variance 10 (list 1/2 1/3 1/12 1/12)))

#|
  Cauchy distribution

  The PDF, CDF, and quantiles for cauchy_dist* and the built-in cauchy are
  almost identical.

0: 0.12732395447351627
20: 0.0017441637599111817
40: 0.00041745558843775827
60: 0.00018267425319012378
80: 0.00010194071615173441
100: 6.492807469327703e-5
120: 4.4943153714619226e-5
140: 3.29428084019447e-5
160: 2.517776438076256e-5
180: 1.9866430718289325e-5
200: 1.6074227303814702e-5

cauchy_dist_cdf(1,2,k):
0: 0.35241638234956674
20: 0.9666166335694749
40: 0.983690703075438
60: 0.9892139644949202
80: 0.9919432428281476
100: 0.993570371843725
120: 0.9946507576353544
140: 0.9954203176619714
160: 0.9959963132082352
180: 0.9964436129470915
200: 0.9968010133708612
(cauchy_quantile(1 2 0.001: -635.6176779710831)
(cauchy_quantile(1 2 0.9: 7.155367074350505)
(cauchy_quantile(1 2 0.99: 64.6410319075477)
(cauchy_quantile(1 2 0.999: 637.6176779710831)
(cauchy_quantile(1 2 0.9999: 6367.197514233658)

built-in cauchy-dist
PDF
0: 0.12732395447351627
20: 0.0017441637599111817
40: 0.00041745558843775827
60: 0.00018267425319012378
80: 0.00010194071615173441
100: 6.492807469327703e-5
120: 4.4943153714619226e-5
140: 3.29428084019447e-5
160: 2.517776438076256e-5
180: 1.9866430718289325e-5
200: 1.6074227303814702e-5

CDF
0: 0.35241638234956674
20: 0.9666166335694748
40: 0.9836907030754379
60: 0.9892139644949202
80: 0.9919432428281476
100: 0.9935703718437249
120: 0.9946507576353544
140: 0.9954203176619714
160: 0.9959963132082352
180: 0.9964436129470915
200: 0.9968010133708612
(cauchy quantile 1 2 0.001: -635.6176779711009)
(cauchy quantile 1 2 0.9: 7.155367074350509)
(cauchy quantile 1 2 0.99: 64.64103190754786)
(cauchy quantile 1 2 0.999: 637.6176779711003)
(cauchy quantile 1 2 0.9999: 6367.197514237003)

|#
(displayln "Testing cauchy")
(displayln "\ncauchy_dist_pdf(1,2,k):")
(for ([k 11]) (show (* k 20) (cauchy_dist_pdf 1 2 (* k 20))))
(displayln "\ncauchy_dist_cdf(1,2,k):")
(for ([k 11]) (show (* k 20) (cauchy_dist_cdf 1 2 (* k 20))))
(show2 "cauchy_quantile(1 2 0.001:" (cauchy_dist_quantile 1 2 0.001))
(show2 "cauchy_quantile(1 2 0.9:" (cauchy_dist_quantile 1 2 0.9))
(show2 "cauchy_quantile(1 2 0.99:" (cauchy_dist_quantile 1 2 0.99))
(show2 "cauchy_quantile(1 2 0.999:" (cauchy_dist_quantile 1 2 0.999))
(show2 "cauchy_quantile(1 2 0.9999:" (cauchy_dist_quantile 1 2 0.9999))

(displayln "\nbuilt-in cauchy-dist")
(displayln "PDF")
(for ([k 11]) (show (* k 20) (dist-pdf (cauchy-dist 1 2) (* k 20))))
(displayln "\nCDF")
(for ([k 11]) (show (* k 20) (dist-cdf (cauchy-dist 1 2) (* k 20))))
(show2 "cauchy quantile 1 2 0.001:" (dist-inv-cdf (cauchy-dist 1 2) 0.001))
(show2 "cauchy quantile 1 2 0.9:" (dist-inv-cdf (cauchy-dist 1 2) 0.9))
(show2 "cauchy quantile 1 2 0.99:" (dist-inv-cdf (cauchy-dist 1 2) 0.99))
(show2 "cauchy quantile 1 2 0.999:" (dist-inv-cdf (cauchy-dist 1 2) 0.999))
(show2 "cauchy quantile 1 2 0.9999:" (dist-inv-cdf (cauchy-dist 1 2) 0.9999))


#| 

  Chi distribution

(samples (0.41889852740897304 1.6342007478717782 0.7437151264279772 1.9109156444740063 1.110676636039009 3.0566907167182173 1.6226146713438685 0.7895593532847491 2.5626989000239346 0.6937892924610575 0.5276841532524051 0.7052233673408668 0.533779392199519 1.689831563892851 0.7237444389305819 1.6205807599550766 1.4158409975264992 1.7493451439076546 0.33188360103803827 2.320324438645181))
Min: 0.33188360103803827 Mean: 1.3080998736371119 Max: 3.0566907167182173 Variance: 0.5667750842885395 Stddev: 0.7528446614598123
PDF
(chi_dist pdf 2  0 : 0)
(chi_dist pdf 2  0.2 : 0.19603973466135105)
(chi_dist pdf 2  0.4 : 0.36924653855465434)
(chi_dist pdf 2  0.6000000000000001 : 0.5011621268467633)
(chi_dist pdf 2  0.8 : 0.5809192296589527)
(chi_dist pdf 2  1.0 : 0.6065306597126334)
(chi_dist pdf 2  1.2 : 0.584102707151966)
(chi_dist pdf 2  1.4 : 0.5254355383919593)
(chi_dist pdf 2  1.5999999999999999 : 0.4448596807251107)
(chi_dist pdf 2  1.7999999999999998 : 0.3562176583505065)
(chi_dist pdf 2  1.9999999999999998 : 0.27067056647322546)
(chi_dist pdf 2  2.1999999999999997 : 0.19562755841065002)
(chi_dist pdf 2  2.4 : 0.13472343080192092)
(chi_dist pdf 2  2.6 : 0.08852338230995827)
(chi_dist pdf 2  2.8000000000000003 : 0.05555506528423676)
(chi_dist pdf 2  3.0000000000000004 : 0.03332698961472687)

CDF
(chi_dist cdf 2  0 : 0.0)
(chi_dist cdf 2  0.2 : 0.019801326693244702)
(chi_dist cdf 2  0.4 : 0.07688365361336424)
(chi_dist cdf 2  0.6000000000000001 : 0.16472978858872805)
(chi_dist cdf 2  0.8 : 0.27385096292630917)
(chi_dist cdf 2  1.0 : 0.39346934028736663)
(chi_dist cdf 2  1.2 : 0.5132477440400285)
(chi_dist cdf 2  1.4 : 0.6246889011486005)
(chi_dist cdf 2  1.5999999999999999 : 0.7219626995468059)
(chi_dist cdf 2  1.7999999999999998 : 0.8021013009163854)
(chi_dist cdf 2  1.9999999999999998 : 0.8646647167633873)
(chi_dist cdf 2  2.1999999999999997 : 0.9110783825406136)
(chi_dist cdf 2  2.4 : 0.9438652371658662)
(chi_dist cdf 2  2.6 : 0.9659525452654006)
(chi_dist cdf 2  2.8000000000000003 : 0.9801589052556298)
(chi_dist cdf 2  3.0000000000000004 : 0.9888910034617577)

Quantile
(chi_dist quantile 2 0.0001 : 0.014142489192829214)
(chi_dist quantile 2 0.001 : 0.044732545950439254)
(chi_dist quantile 2 0.01 : 0.1417768376961282)
(chi_dist quantile 2 0.05 : 0.3202914122719878)
(chi_dist quantile 2 0.25 : 0.7585276164411016)
(chi_dist quantile 2 0.5 : 1.1774100225153146)
(chi_dist quantile 2 0.75 : 1.6651092223153867)
(chi_dist quantile 2 0.9 : 2.1459660262889475)
(chi_dist quantile 2 0.95 : 2.4477468306806527)
(chi_dist quantile 2 0.99 : 3.0348542587692)
(chi_dist quantile 2 0.999 : 3.7169221888589696)
(chi_dist quantile 2 0.9999 : 4.291932052678364)
(chi_dist quantile 2 0.99999 : 4.798525910409705)

(chi_dist_mean 2: 1.2533141373155003

|#

(displayln "\nChi distribution")
(let ([k 2])
  (let ([samples (repeat (lambda () (chi_dist k)) 20)])
    (show2 "samples" samples)
    (show-stats samples)
    )

  (displayln "PDF")
  (for ([x (range 0 3.1 0.2)])
    (show2 "chi_dist pdf 2 " x ":" (chi_dist_pdf k x)))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 3.1 0.2)])
    (show2 "chi_dist cdf 2 " x ":" (chi_dist_cdf k x)))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "chi_dist quantile" k q ":" (chi_dist_quantile k q)))
  (newline)
  
  (displayln (format "(chi_dist_mean ~a: ~a" k (chi_dist_mean k)))

  )
(newline)
  
#| 
  Chi squared distribution

(samples (1.3992275482236274 1.4523068075990992 0.5016402450696001 2.3521256216443462 0.9285036596501592 1.8819218646847449 3.562791957276462 2.757294638202717 0.1520107015199498 1.2521349067914473 0.024521924670065583 2.8210750027759706 0.9591222947130295 5.583938055926046 0.30922598920750094 3.8690962884682047 0.7395941585771506 0.23047030657380488 0.13178061390485332 4.379120030818845))
Min: 0.024521924670065583 Mean: 1.764395130814881 Max: 5.583938055926046 Variance: 2.4399635971017775 Stddev: 1.5620382828540975
PDF
(chi_squared_dist pdf 2  0 : 1/2)
(chi_squared_dist pdf 2  0.2 : 0.45241870901797976)
(chi_squared_dist pdf 2  0.4 : 0.4093653765389909)
(chi_squared_dist pdf 2  0.6000000000000001 : 0.37040911034085894)
(chi_squared_dist pdf 2  0.8 : 0.33516002301781966)
(chi_squared_dist pdf 2  1.0 : 0.3032653298563167)
(chi_squared_dist pdf 2  1.2 : 0.2744058180470132)
(chi_squared_dist pdf 2  1.4 : 0.24829265189570476)
(chi_squared_dist pdf 2  1.5999999999999999 : 0.2246644820586108)
(chi_squared_dist pdf 2  1.7999999999999998 : 0.20328482987029958)
(chi_squared_dist pdf 2  1.9999999999999998 : 0.1839397205857212)
(chi_squared_dist pdf 2  2.1999999999999997 : 0.1664355418490398)
(chi_squared_dist pdf 2  2.4 : 0.15059710595610107)
(chi_squared_dist pdf 2  2.6 : 0.1362658965170063)
(chi_squared_dist pdf 2  2.8000000000000003 : 0.12329848197080322)
(chi_squared_dist pdf 2  3.0000000000000004 : 0.1115650800742149)

CDF
(chi_squared_dist cdf 2  0 : 0.0)
(chi_squared_dist cdf 2  0.2 : 0.09516258196404045)
(chi_squared_dist cdf 2  0.4 : 0.18126924692201823)
(chi_squared_dist cdf 2  0.6000000000000001 : 0.25918177931828223)
(chi_squared_dist cdf 2  0.8 : 0.3296799539643608)
(chi_squared_dist cdf 2  1.0 : 0.39346934028736663)
(chi_squared_dist cdf 2  1.2 : 0.4511883639059736)
(chi_squared_dist cdf 2  1.4 : 0.5034146962085906)
(chi_squared_dist cdf 2  1.5999999999999999 : 0.5506710358827784)
(chi_squared_dist cdf 2  1.7999999999999998 : 0.593430340259401)
(chi_squared_dist cdf 2  1.9999999999999998 : 0.6321205588285576)
(chi_squared_dist cdf 2  2.1999999999999997 : 0.6671289163019205)
(chi_squared_dist cdf 2  2.4 : 0.698805788087798)
(chi_squared_dist cdf 2  2.6 : 0.7274682069659875)
(chi_squared_dist cdf 2  2.8000000000000003 : 0.7534030360583938)
(chi_squared_dist cdf 2  3.0000000000000004 : 0.7768698398515704)

Quantile
(chi_squared_dist quantile 2 0.0001 : 0.00020001000078336716)
(chi_squared_dist quantile 2 0.001 : 3.5371064329904316e-5)
(chi_squared_dist quantile 2 0.01 : 0.020100671706963703)
(chi_squared_dist quantile 2 0.05 : 0.10258658877520908)
(chi_squared_dist quantile 2 0.25 : 0.5753641449035967)
(chi_squared_dist quantile 2 0.5 : 1.386294361119913)
(chi_squared_dist quantile 2 0.75 : 2.7725887222394556)
(chi_squared_dist quantile 2 0.9 : 4.605170185990036)
(chi_squared_dist quantile 2 0.95 : 5.991464547108341)
(chi_squared_dist quantile 2 0.99 : 9.210340371996072)
(chi_squared_dist quantile 2 0.999 : 13.81551055793348)
(chi_squared_dist quantile 2 0.9999 : 18.42068074308372)
(chi_squared_dist quantile 2 0.99999 : 23.025850922406036)

(chi_squared_dist_mean 2: 2

|#

(displayln "\nChi squared distribution")
(let ([k 2])
  (let ([samples (repeat (lambda () (chi_squared_dist k)) 20)])
    (show2 "samples" samples)
    (show-stats samples)
    )

  (displayln "PDF")
  (for ([x (range 0 3.1 0.2)])
    (show2 "chi_squared_dist pdf 2 " x ":" (chi_squared_dist_pdf k x)))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 3.1 0.2)])
    (show2 "chi_squared_dist cdf 2 " x ":" (chi_squared_dist_cdf k x)))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "chi_squared_dist quantile" k q ":" (chi_squared_dist_quantile k q)))
  (newline)

  (displayln (format "(chi_squared_dist_mean ~a: ~a" k (chi_squared_dist_mean k)))

  )
(newline)

  
#| 
  Chi squared inverse distribution

(samples (2.023862973452834 4.3762518681572615 0.15696905689014656 2.5383801127405556 4.281401356400447 0.8561321798182652 8.58333626182188 0.24276490375110038 0.32702770788146923 0.6588818581403505 0.3969000620483207 0.3058194887758092 0.6338632380155802 0.32263930148566394 3.5727749760217735 0.47385761019117806 1.017094076559395 0.16405473112693728 0.2604230526410157 0.39440106418679327))
Min: 0.15696905689014656 Mean: 1.5793417940053391 Max: 8.58333626182188 Variance: 4.409859788136825 Stddev: 2.099966615957698

|#

(displayln "\nChi squared inverse distribution")
(let ([k 2])
  (let ([samples (repeat (lambda () (chi_squared_inverse_dist k)) 20)])
    (show2 "samples" samples)
    (show-stats samples)
    ))
(newline)
   
#|
  Exponential distribution

  Note that the parameter lambda is the inverse of the lambda parameter
  for the built-in exponential distribution.

exponential_dist lambda:1/3000
Some samples
(5158.275326948456 6589.015420131808 3821.4512963248567 1352.4403487004556 167.659105916373 3091.646676930309 7803.319959463557 481.20975557858037 7165.887485598066 2008.7504259416983)
PDF
(exponential_dist_pdf 1/3000 0 : 1/3000)
(exponential_dist_pdf 1/3000 300 : 0.00030161247267865315)
(exponential_dist_pdf 1/3000 600 : 0.00027291025102599395)
(exponential_dist_pdf 1/3000 900 : 0.00024693940689390594)
(exponential_dist_pdf 1/3000 1200 : 0.0002234400153452131)
(exponential_dist_pdf 1/3000 1500 : 0.0002021768865708778)
(exponential_dist_pdf 1/3000 1800 : 0.00018293721203134212)
(exponential_dist_pdf 1/3000 2100 : 0.0001655284345971365)
(exponential_dist_pdf 1/3000 2400 : 0.00014977632137240719)
(exponential_dist_pdf 1/3000 2700 : 0.00013552321991353305)
(exponential_dist_pdf 1/3000 3000 : 0.0001226264803904808)
(exponential_dist_pdf 1/3000 3300 : 0.00011095702789935985)
(exponential_dist_pdf 1/3000 3600 : 0.00010039807063740071)
(exponential_dist_pdf 1/3000 3900 : 9.084393101133753e-5)
(exponential_dist_pdf 1/3000 4200 : 8.219898798053549e-5)
(exponential_dist_pdf 1/3000 4500 : 7.437672004947661e-5)
(exponential_dist_pdf 1/3000 4800 : 6.729883933155179e-5)

CDF
(exponential_dist_cdf 1/3000 0 : 0)
(exponential_dist_cdf 1/3000 1000 : 0.28346868942621073)
(exponential_dist_cdf 1/3000 2000 : 0.486582880967408)
(exponential_dist_cdf 1/3000 3000 : 0.6321205588285577)
(exponential_dist_cdf 1/3000 4000 : 0.7364028618842733)
(exponential_dist_cdf 1/3000 5000 : 0.8111243971624382)
(exponential_dist_cdf 1/3000 6000 : 0.8646647167633873)
(exponential_dist_cdf 1/3000 7000 : 0.9030280321355949)
(exponential_dist_cdf 1/3000 8000 : 0.9305165487771985)
(exponential_dist_cdf 1/3000 9000 : 0.950212931632136)
(exponential_dist_cdf 1/3000 10000 : 0.9643260066527476)
(exponential_dist_cdf 1/3000 11000 : 0.9744384667934926)
(exponential_dist_cdf 1/3000 12000 : 0.9816843611112658)
(exponential_dist_cdf 1/3000 13000 : 0.986876271263059)
(exponential_dist_cdf 1/3000 14000 : 0.9905964374485048)
(exponential_dist_cdf 1/3000 15000 : 0.9932620530009145)

Quantile
(exponential_dist_quantile 1/3000 0.0001 : 0.30001500100004197)
(exponential_dist_quantile 1/3000 0.001 : 3.0015010007506033)
(exponential_dist_quantile 1/3000 0.01 : 30.151007560504354)
(exponential_dist_quantile 1/3000 0.05 : 153.87988316265174)
(exponential_dist_quantile 1/3000 0.25 : 863.0462173553427)
(exponential_dist_quantile 1/3000 0.5 : 2079.441541679836)
(exponential_dist_quantile 1/3000 0.75 : 4158.883083359672)
(exponential_dist_quantile 1/3000 0.9 : 6907.7552789821375)
(exponential_dist_quantile 1/3000 0.95 : 8987.19682066197)
(exponential_dist_quantile 1/3000 0.99 : 13815.510557964273)
(exponential_dist_quantile 1/3000 0.999 : 20723.265836946408)
(exponential_dist_quantile 1/3000 0.9999 : 27631.02111592888)
(exponential_dist_quantile 1/3000 0.99999 : 34538.77639492434)

(exponential_dist_mean 1/3000: 3000

|#
(displayln "Testing exponential")

(let ([lambda_ 1/3000])
  (displayln (format "exponential_dist lambda:~a" lambda_))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (exponential_dist lambda_)) 10))
  (displayln "PDF")
  (for ([x (range 0 5001 300)])
    (show2 "exponential_dist_pdf" lambda_ x ":" (exponential_dist_pdf lambda_ x))
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 0 15001 1000)])
    (show2 "exponential_dist_cdf" lambda_ x ":" (exponential_dist_cdf lambda_ x))
    )
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "exponential_dist_quantile" lambda_ q ":" (exponential_dist_quantile lambda_ q))
    )
  (newline)
  (displayln (format "(exponential_dist_mean ~a: ~a" lambda_ (exponential_dist_mean lambda_)))
  )
(newline)

#|
  Erlang distribution

erlang_dist k:7 lambda:2
Some samples
(4.665341121415921 5.864618326769306 2.2326907051825176 4.987927491291187 3.244081963117813 3.4161835751118472 5.012341012096445 2.754465197479914 4.063974818310454 4.479455416163807)
PDF
(erlang_pdf 7 2 0 : 0)
(erlang_pdf 7 2 0.5 : 0.0010218873365873398)
(erlang_pdf 7 2 1.0 : 0.024059605908731147)
(erlang_pdf 7 2 1.5 : 0.1008188134449245)
(erlang_pdf 7 2 2.0 : 0.2083912691340422)
(erlang_pdf 7 2 2.5 : 0.2924456162797512)
(erlang_pdf 7 2 3.0 : 0.32124628209596007)
(erlang_pdf 7 2 3.5 : 0.2980055593486758)
(erlang_pdf 7 2 4.0 : 0.24427643091354462)
(erlang_pdf 7 2 4.5 : 0.18218063803785853)
(erlang_pdf 7 2 5.0 : 0.12611091600690236)
(erlang_pdf 7 2 5.5 : 0.0821891159824122)
(erlang_pdf 7 2 6.0 : 0.0509625549434455)
(erlang_pdf 7 2 6.5 : 0.030306050901613375)

CDF
(erlang_cdf 7 2 0 : 0.0)
(erlang_cdf 7 2 1 : 0.004533805526248867)
(erlang_cdf 7 2 2 : 0.11067397840257369)
(erlang_cdf 7 2 3 : 0.39369721758740867)
(erlang_cdf 7 2 4 : 0.6866257224636023)
(erlang_cdf 7 2 5 : 0.8698585791175171)
(erlang_cdf 7 2 6 : 0.9541776931113488)
(erlang_cdf 7 2 7 : 0.9857720816557385)
(erlang_cdf 7 2 8 : 0.9959939553448722)
(erlang_cdf 7 2 9 : 0.9989565544897424)

Quantile
(erlang_quantile_est 7 2 0 : 0.647286629114478)
(erlang_quantile_est 7 2 0.1 : 1.963399761077428)
(erlang_quantile_est 7 2 0.2 : 2.32108280031428)
(erlang_quantile_est 7 2 0.30000000000000004 : 2.678035705410335)
(erlang_quantile_est 7 2 0.4 : 3.014100054865774)
(erlang_quantile_est 7 2 0.5 : 3.2732477033167324)
(erlang_quantile_est 7 2 0.6 : 3.6709713843282636)
(erlang_quantile_est 7 2 0.7 : 4.0601300526201225)
(erlang_quantile_est 7 2 0.7999999999999999 : 4.5048910988300435)
(erlang_quantile_est 7 2 0.8999999999999999 : 5.289604169418393)
(erlang_quantile_est 7 2 0.9999999999999999 : 9.758561932956805)


Mean
(erlang_mean 7 2 : 7/2)

|#
(displayln "Testing erlang")
(let ([k 7]
      [lambda_ 2])
  (displayln (format "erlang_dist k:~a lambda:~a" k lambda_))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (erlang k lambda_)) 10))
  (displayln "PDF")
  (for ([x (range 0 7 0.5)])
    (show2 "erlang_pdf" k lambda_ x ":" (erlang_pdf k lambda_ x))
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 0 10)])
    (show2 "erlang_cdf" k lambda_ x ":" (erlang_cdf k lambda_ x))
    )
  (newline)
  (displayln "Quantile")
  (for ([x (range 0 1 0.10)])
    (show2 "erlang_quantile_est" k lambda_ x ":" (erlang_quantile_est k lambda_ x))
    )
  (newline)
  (displayln "Mean")
  (show2 "erlang_mean" k lambda_ ":" (erlang_mean k lambda_))
  
)
(newline)


#|
  Inverse exponential distribution 

inverse_exponential lambda:2
Some samples
(4.277338664601171 2.547301278451777 0.495839016984241 0.5050872293074674 1.4583440493191686 0.8193983974199585 4.228735213650534 2.313437466849595 7.096395556389943 6.106998731203505)


|#
(displayln "Testing inverse_exponential ")
(let ([lambda_ 2])
  (displayln (format "inverse_exponential lambda:~a" lambda_))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (inverse_exponential lambda_)) 10))
)  
(newline)


#|
  Shifted exponential distribution

shifted_exponential lambda:2 t: 1
Some samples
(1.1112233697214224 1.1314699603761091 1.03385669473826 1.3578744519231019 1.2734633895867642 1.0199122410738377 1.756235768630606 1.2773208772511477 1.4910982361413445 1.5184726961372235)

|#
(displayln "Testing shifted_exponential ")
(let ([lambda_ 2]
      [t 1])
  (displayln (format "shifted_exponential lambda:~a t: ~a" lambda_ t))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (shifted_exponential lambda_ t)) 10))
)  
(newline)


#|
  Frechet distribution

frechet_dist alpha:1.71 beta: 6.3
Some samples
(20.932033878131765 5.546820124065232 3.544173978796456 25.697852556945882 3.4435109452564174 5.4304286420470165 22.804429082373503 3.3715772856691633 6.312331402311772 8.30677594023978)
PDF
(frechet_dist_pdf 1.71 6.3 0 : 0)
(frechet_dist_pdf 1.71 6.3 10 : 0.04929263705131147)
(frechet_dist_pdf 1.71 6.3 20 : 0.010323708179001271)
(frechet_dist_pdf 1.71 6.3 30 : 0.0036876958679609487)
(frechet_dist_pdf 1.71 6.3 40 : 0.0017372906013402249)
(frechet_dist_pdf 1.71 6.3 50 : 0.0009618056973768046)
(frechet_dist_pdf 1.71 6.3 60 : 0.0005913896014216475)
(frechet_dist_pdf 1.71 6.3 70 : 0.00039136394618117035)
(frechet_dist_pdf 1.71 6.3 80 : 0.0002734427059252889)
(frechet_dist_pdf 1.71 6.3 90 : 0.00019919100487527289)

CDF
(frechet_dist_cdf 1.71 6.3 1 : 7.801888672264093e-11)
(frechet_dist_cdf 1.71 6.3 3 : 0.028545037005238263)
(frechet_dist_cdf 1.71 6.3 5 : 0.22657423356511802)
(frechet_dist_cdf 1.71 6.3 7 : 0.43381757570698315)
(frechet_dist_cdf 1.71 6.3 9 : 0.5807717157111013)
(frechet_dist_cdf 1.71 6.3 11 : 0.6800703689348212)
(frechet_dist_cdf 1.71 6.3 13 : 0.7484477793632525)
(frechet_dist_cdf 1.71 6.3 15 : 0.7970330422940604)
(frechet_dist_cdf 1.71 6.3 17 : 0.8326440360821282)
(frechet_dist_cdf 1.71 6.3 19 : 0.8594805131236631)

Quantile
(frechet_dist_quantile 1.71 6.3 0.0001 : 1.7196364539931845)
(frechet_dist_quantile 1.71 6.3 0.001 : 2.0346992812147655)
(frechet_dist_quantile 1.71 6.3 0.01 : 2.579155791845932)
(frechet_dist_quantile 1.71 6.3 0.05 : 3.3165292002101556)
(frechet_dist_quantile 1.71 6.3 0.25 : 5.204561467081288)
(frechet_dist_quantile 1.71 6.3 0.5 : 7.80593759842106)
(frechet_dist_quantile 1.71 6.3 0.75 : 13.054667453980567)
(frechet_dist_quantile 1.71 6.3 0.9 : 23.48950952698412)
(frechet_dist_quantile 1.71 6.3 0.95 : 35.78418082618781)
(frechet_dist_quantile 1.71 6.3 0.99 : 92.82308596705612)
(frechet_dist_quantile 1.71 6.3 0.999 : 357.766337226965)
(frechet_dist_quantile 1.71 6.3 0.9999 : 1375.6537513476383)
(frechet_dist_quantile 1.71 6.3 0.99999 : 5288.297607257013)

Mean
13.451693556415941

|#
(displayln "Testing Frechet ")
(let ([alpha 1.71]
      [beta 6.3])
  (displayln (format "frechet_dist alpha:~a beta: ~a" alpha beta))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (frechet_dist alpha beta)) 10))
  (displayln "PDF")
  (for ([x (range 0 100 10)])
    (show2 "frechet_dist_pdf" alpha beta x ":" (frechet_dist_pdf alpha beta x))
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 1 20 2)])
    (show2 "frechet_dist_cdf" alpha beta x ":" (frechet_dist_cdf alpha beta x))
    )
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "frechet_dist_quantile" alpha beta q ":" (frechet_dist_quantile alpha beta q))
    )
  (newline)
  (displayln "Mean")
  (displayln (frechet_dist_mean alpha beta))
)
(newline)


#|
  Gamma distribution

gamma_dist a:10 b: 3
Some samples
(37.68583030797071 26.384490117091506 21.540903765526252 27.848926487043315 35.08815146921854 28.967720595312525 29.467194767876656 27.139300574382176 26.53716057397943 35.713656389580756)
PDF
(gamma_dist_pdf 10 3 0 : 0)
(gamma_dist_pdf 10 3 10 : 0.0016648539732503228)
(gamma_dist_pdf 10 3 20 : 0.030408698657729907)
(gamma_dist_pdf 10 3 30 : 0.04170334524037777)
(gamma_dist_pdf 10 3 40 : 0.019813958536490765)
(gamma_dist_pdf 10 3 50 : 0.005266390927236572)
(gamma_dist_pdf 10 3 60 : 0.0009693844197241893)
(gamma_dist_pdf 10 3 70 : 0.00013847420133298463)
(gamma_dist_pdf 10 3 80 : 1.6430399328622334e-5)
(gamma_dist_pdf 10 3 90 : 1.691891652848335e-6)

CDF
(gamma_dist_cdf 10 3 1 : 3.4481690733463094e-12)
(gamma_dist_cdf 10 3 3 : 1.1142547833872067e-7)
(gamma_dist_cdf 10 3 5 : 1.0119673258770563e-5)
(gamma_dist_cdf 10 3 7 : 0.0001613574169603394)
(gamma_dist_cdf 10 3 9 : 0.0011024881301154796)
(gamma_dist_cdf 10 3 11 : 0.004560158519939894)
(gamma_dist_cdf 10 3 13 : 0.0135436791845929)
(gamma_dist_cdf 10 3 15 : 0.03182805730620482)
(gamma_dist_cdf 10 3 17 : 0.0628841472262335)
(gamma_dist_cdf 10 3 19 : 0.1087764727793476)

Quantile
(gamma_dist_quantile_est 10 3 0.0001 : 13.32274156828414)
(gamma_dist_quantile_est 10 3 0.001 : 8.003090982876158)
(gamma_dist_quantile_est 10 3 0.01 : 13.091339717148164)
(gamma_dist_quantile_est 10 3 0.05 : 14.464879156738446)
(gamma_dist_quantile_est 10 3 0.25 : 23.37988466460702)
(gamma_dist_quantile_est 10 3 0.5 : 28.382522425616582)
(gamma_dist_quantile_est 10 3 0.75 : 36.259263987321894)
(gamma_dist_quantile_est 10 3 0.9 : 40.50970580240406)
(gamma_dist_quantile_est 10 3 0.95 : 48.66344053039671)
(gamma_dist_quantile_est 10 3 0.99 : 53.741935683572954)
(gamma_dist_quantile_est 10 3 0.999 : 62.36181798659508)
(gamma_dist_quantile_est 10 3 0.9999 : 55.5396035570371)
(gamma_dist_quantile_est 10 3 0.99999 : 57.032499553414674)

Mean
30


|#
(displayln "Testing Gamma ")
(let ([a 10]
      [b 3])
  (displayln (format "gamma_dist a:~a b: ~a" a b))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (gamma_dist a b)) 10))
  (displayln "PDF")
  (for ([x (range 0 100 10)])
    (show2 "gamma_dist_pdf" a b x ":" (gamma_dist_pdf a b x))
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 1 20 2)])
    (show2 "gamma_dist_cdf" a b x ":" (gamma_dist_cdf a b x))
    )
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "gamma_dist_quantile_est" a b q ":" (gamma_dist_quantile_est a b q #:num-samples 100))
    )
  (newline)
  (displayln "Mean")
  (displayln (gamma_dist_mean a b))
)
(newline)


#|
  Geometric distribution.

  Note: In gamble_distributions.rkt there are three different implementation of 
        generators of the geometric distributions. 

geometric_dist p:1/10
Samples geometric_dist
(1 1 13 11 23 51 7 2 18 22)
Samples geometric_dist2
(3 11 3 0 3 35 7 13 4 6)
Samples geometric_dist3
(0 2 5 0 18 15 7 39 16 2)
PDF
(geometric_dist_pdf 1/10 0 : 1/10 ( 0.1 ))
(geometric_dist_pdf 1/10 5 : 59049/1000000 ( 0.059049 ))
(geometric_dist_pdf 1/10 10 : 3486784401/100000000000 ( 0.03486784401 ))
(geometric_dist_pdf 1/10 15 : 205891132094649/10000000000000000 ( 0.0205891132094649 ))
(geometric_dist_pdf 1/10 20 : 12157665459056928801/1000000000000000000000 ( 0.01215766545905693 ))
(geometric_dist_pdf 1/10 25 : 717897987691852588770249/100000000000000000000000000 ( 0.007178979876918526 ))
(geometric_dist_pdf 1/10 30 : 42391158275216203514294433201/10000000000000000000000000000000 ( 0.004239115827521621 ))
(geometric_dist_pdf 1/10 35 : 2503155504993241601315571986085849/1000000000000000000000000000000000000 ( 0.0025031555049932416 ))

CDF
(geometric_dist_cdf 1/10 0 : 1/10 ( 0.1 ))
(geometric_dist_cdf 1/10 5 : 468559/1000000 ( 0.468559 ))
(geometric_dist_cdf 1/10 10 : 68618940391/100000000000 ( 0.68618940391 ))
(geometric_dist_cdf 1/10 15 : 8146979811148159/10000000000000000 ( 0.814697981114816 ))
(geometric_dist_cdf 1/10 20 : 890581010868487640791/1000000000000000000000 ( 0.8905810108684876 ))
(geometric_dist_cdf 1/10 25 : 93538918110773326701067759/100000000000000000000000000 ( 0.9353891811077333 ))
(geometric_dist_cdf 1/10 30 : 9618479575523054168371350101191/10000000000000000000000000000000 ( 0.9618479575523055 ))
(geometric_dist_cdf 1/10 35 : 977471600455060825588159852125227359/1000000000000000000000000000000000000 ( 0.9774716004550609 ))
(geometric_dist_cdf 1/10 40 : 98669720535270886690155251108142550321591/100000000000000000000000000000000000000000 ( 0.9866972053527089 ))
(geometric_dist_cdf 1/10 45 : 9921448327887210588166977422684709453939626959/10000000000000000000000000000000000000000000000 ( 0.992144832788721 ))
(geometric_dist_cdf 1/10 50 : 995361602313411898020671849832109408545681032301991/1000000000000000000000000000000000000000000000000000 ( 0.9953616023134119 ))

Quantile
(geometric_dist_quantile 1/10 0.0001 : 0)
(geometric_dist_quantile 1/10 0.001 : 0)
(geometric_dist_quantile 1/10 0.01 : 0)
(geometric_dist_quantile 1/10 0.05 : 0)
(geometric_dist_quantile 1/10 0.25 : 2)
(geometric_dist_quantile 1/10 0.5 : 6)
(geometric_dist_quantile 1/10 0.75 : 13)
(geometric_dist_quantile 1/10 0.9 : 21)
(geometric_dist_quantile 1/10 0.95 : 28)
(geometric_dist_quantile 1/10 0.99 : 43)
(geometric_dist_quantile 1/10 0.999 : 65)
(geometric_dist_quantile 1/10 0.9999 : 87)
(geometric_dist_quantile 1/10 0.99999 : 109)

Mean
9

|#
(displayln "Testing Geometric ")
(let ([p 1/10])
  (displayln (format "geometric_dist p:~a" p))
  (displayln "Samples geometric_dist")   
  (displayln (repeat (lambda () (geometric_dist p)) 10))
  (displayln "Samples geometric_dist2")   
  (displayln (repeat (lambda () (geometric_dist2 p)) 10))
  (displayln "Samples geometric_dist3")   
  (displayln (repeat (lambda () (geometric_dist3 p)) 10))
  
  (displayln "PDF")
  (for ([x (range 0 40 5)])
    (let ([v (geometric_dist_pdf p x)])
    (show2 "geometric_dist_pdf" p x ":" v "(" (* 1.0 v) ")" )
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 51 5)])
    (let ([v (geometric_dist_cdf p x)])
    (show2 "geometric_dist_cdf" p x ":" v "(" (* 1.0 v) ")")
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "geometric_dist_quantile" p q ":" (geometric_dist_quantile p q))
    )
  (newline)
  (displayln "Mean")
  (displayln (geometric_dist_mean p))
)
(newline)


#|
  Gaussian/Normal distribution

gaussian_dist mean:100 std:15
Samples gaussian_dist
(118.9192315011815 88.08166622197291 100.39322159947436 117.73022701629718 135.07759687481214 117.2228706196434 98.21869253554523 120.2246400381544 104.91306455213402 116.8563427609313)
PDF
(gaussian_dist_pdf 100 15 60 : 0.0007597324015864961)
(gaussian_dist_pdf 100 15 70 : 0.0035993977675458705)
(gaussian_dist_pdf 100 15 80 : 0.010934004978399576)
(gaussian_dist_pdf 100 15 90 : 0.02129653370149015)
(gaussian_dist_pdf 100 15 100 : 0.02659615202676218)
(gaussian_dist_pdf 100 15 110 : 0.02129653370149015)
(gaussian_dist_pdf 100 15 120 : 0.010934004978399576)
(gaussian_dist_pdf 100 15 130 : 0.0035993977675458705)

CDF
(gaussian_dist_cdf 100 15 60 : 0.0038303805675897365)
(gaussian_dist_cdf 100 15 70 : 0.022750131948179212)
(gaussian_dist_cdf 100 15 80 : 0.09121121972586786)
(gaussian_dist_cdf 100 15 90 : 0.2524925375469229)
(gaussian_dist_cdf 100 15 100 : 1/2)
(gaussian_dist_cdf 100 15 110 : 0.7475074624530771)
(gaussian_dist_cdf 100 15 120 : 0.9087887802741321)
(gaussian_dist_cdf 100 15 130 : 0.9772498680518208)

Quantile
Quantile
(gaussian_dist_quantile 100 15 0.0001 : 44.214752714991725)
(gaussian_dist_quantile 100 15 0.001 : 53.646515407549195)
(gaussian_dist_quantile 100 15 0.01 : 65.10478188943722)
(gaussian_dist_quantile 100 15 0.05 : 75.32719559572882)
(gaussian_dist_quantile 100 15 0.25 : 89.88265374705966)
(gaussian_dist_quantile 100 15 0.5 : 99.99999999999892)
(gaussian_dist_quantile 100 15 0.75 : 110.11734625294523)
(gaussian_dist_quantile 100 15 0.9 : 119.22327348317584)
(gaussian_dist_quantile 100 15 0.95 : 124.67280440428432)
(gaussian_dist_quantile 100 15 0.99 : 134.89521811061292)
(gaussian_dist_quantile 100 15 0.999 : 146.35348459284904)
(gaussian_dist_quantile 100 15 0.9999 : 155.78524728181716)
(gaussian_dist_quantile 100 15 0.99999 : 163.9733619090839)

Quantile (compare with built-in inv-cdf)
(gaussian_dist_quantile 100 15 0.0001 : 44.2147527181648)
(gaussian_dist_quantile 100 15 0.001 : 53.6465154074828)
(gaussian_dist_quantile 100 15 0.01 : 65.1047818893874)
(gaussian_dist_quantile 100 15 0.05 : 75.32719559572791)
(gaussian_dist_quantile 100 15 0.25 : 89.88265374705878)
(gaussian_dist_quantile 100 15 0.5 : 100.0)
(gaussian_dist_quantile 100 15 0.75 : 110.11734625294122)
(gaussian_dist_quantile 100 15 0.9 : 119.22327348316901)
(gaussian_dist_quantile 100 15 0.95 : 124.67280440427207)
(gaussian_dist_quantile 100 15 0.99 : 134.8952181106126)
(gaussian_dist_quantile 100 15 0.999 : 146.3534845925172)
(gaussian_dist_quantile 100 15 0.9999 : 155.7852472818356)
(gaussian_dist_quantile 100 15 0.99999 : 163.9733619088576)

Mean
100


|#
(displayln "Testing Gaussian/normal ")
(let ([mean 100]
      [std 15])
  (displayln (format "gaussian_dist mean:~a std:~a" mean std))
  (displayln "Samples gaussian_dist")   
  (displayln (repeat (lambda () (gaussian_dist mean std)) 10))
  
  (displayln "PDF")
  (for ([x (range 60 140 10)])
    (let ([v (gaussian_dist_pdf mean std x)])
    (show2 "gaussian_dist_pdf" mean std x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 60 140 10)])
    (let ([v (gaussian_dist_cdf mean std x)])
    (show2 "gaussian_dist_cdf" mean std x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (begin 
    (for ([q ps])
      (show2 "gaussian_dist_quantile" mean std q ":" (gaussian_dist_quantile mean std q))
      )
    (newline)  
    (displayln "Quantile (compare with built-in inv-cdf)")
    (for ([q ps])
      (show2 "gaussian_dist_quantile" mean std q ":" (dist-inv-cdf (normal-dist mean std) q))
      )
    )
  
  (newline)
  (displayln "Mean")
  (displayln (gaussian_dist_mean mean std))
)
(newline)


#|
  Gumbel distribution

gumbel_dist mean:10 std:1
Samples gumbel_dist
(8.710008173487504 7.728848587960806 9.571492593350964 10.205771503211317 7.950692805842373 11.114013366266114 
7.855579943562281 11.286222005025806 10.82980150694825 10.242947560061848)
PDF
(gumbel_dist_pdf 10 1 0 : 4.539786865564983e-5)
(gumbel_dist_pdf 10 1 1 : 0.00012339457504666066)
(gumbel_dist_pdf 10 1 2 : 0.000335350111601355)
(gumbel_dist_pdf 10 1 3 : 0.0009110508158482205)
(gumbel_dist_pdf 10 1 4 : 0.0024726155730149077)
(gumbel_dist_pdf 10 1 5 : 0.006692699677535514)
(gumbel_dist_pdf 10 1 6 : 0.01798322969671364)
(gumbel_dist_pdf 10 1 7 : 0.04736900967790791)
(gumbel_dist_pdf 10 1 8 : 0.11820495159314313)
(gumbel_dist_pdf 10 1 9 : 0.2546463800435825)
(gumbel_dist_pdf 10 1 10 : 0.36787944117144233)
(gumbel_dist_pdf 10 1 11 : 0.1793740787340172)
(gumbel_dist_pdf 10 1 12 : 0.004566281420127915)

CDF
(gumbel_dist_cdf 10 1 0 : 4.5398899201298804e-5)
(gumbel_dist_cdf 10 1 1 : 0.0001234021894100934)
(gumbel_dist_cdf 10 1 2 : 0.000335406366606561)
(gumbel_dist_cdf 10 1 3 : 0.000911466327542132)
(gumbel_dist_cdf 10 1 4 : 0.002475682607247509)
(gumbel_dist_cdf 10 1 5 : 0.006715297932158526)
(gumbel_dist_cdf 10 1 6 : 0.018148926938333476)
(gumbel_dist_cdf 10 1 7 : 0.04856800709954656)
(gumbel_dist_cdf 10 1 8 : 0.12657698150688335)
(gumbel_dist_cdf 10 1 9 : 0.3077993724446536)
(gumbel_dist_cdf 10 1 10 : 0.6321205588285577)
(gumbel_dist_cdf 10 1 11 : 0.9340119641546875)
(gumbel_dist_cdf 10 1 12 : 0.9993820210106689)

Quantile
(gumbel_dist_quantile 10 1 0.0001 : 0.7897096301071649)
(gumbel_dist_quantile 10 1 0.001 : 3.092744929476284)
(gumbel_dist_quantile 10 1 0.01 : 5.399850773223421)
(gumbel_dist_quantile 10 1 0.05 : 7.029804750957837)
(gumbel_dist_quantile 10 1 0.25 : 8.754100676292762)
(gumbel_dist_quantile 10 1 0.5 : 9.633487079418336)
(gumbel_dist_quantile 10 1 0.75 : 10.326634259978281)
(gumbel_dist_quantile 10 1 0.9 : 10.834032445247956)
(gumbel_dist_quantile 10 1 0.95 : 11.09718870036495)
(gumbel_dist_quantile 10 1 0.99 : 11.527179625807902)
(gumbel_dist_quantile 10 1 0.999 : 11.932644733916066)
(gumbel_dist_quantile 10 1 0.9999 : 12.220326806367858)
(gumbel_dist_quantile 10 1 0.99999 : 12.443470357682452)

Mean
9.422784335098466

|#
(displayln "Testing Gumbel ")
(let ([mu 10]
      [sigma 1])
  (displayln (format "gumbel_dist mean:~a std:~a" mu sigma))
  (displayln "Samples gumbel_dist")   
  (displayln (repeat (lambda () (gumbel_dist mu sigma)) 10))
  
  (displayln "PDF")
  (for ([x (range 13)])
    (let ([v (gumbel_dist_pdf mu sigma x)])
    (show2 "gumbel_dist_pdf" mu sigma x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 13)])
    (let ([v (gumbel_dist_cdf mu sigma x)])
    (show2 "gumbel_dist_cdf" mu sigma x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "gumbel_dist_quantile" mu sigma q ":" (gumbel_dist_quantile mu sigma q))
    )
  (newline)
  (displayln "Mean")
  (displayln (gumbel_dist_mean mu sigma))
)
(newline)


#|
  Kumaraswamy distribution

Samples:
(0.8482616407359497 0.7287512291177678 0.7744021213322309 0.7233394313803091 0.48104219036370865 0.9559240978514312 0.8582886777652794 0.697006041181637 0.782592016573617 0.789400315125527)
PDF
(kumaraswamy_dist_pdf 5 2 0 : 0)
(kumaraswamy_dist_pdf 5 2 0.1 : 0.00099999)
(kumaraswamy_dist_pdf 5 2 0.2 : 0.015994880000000003)
(kumaraswamy_dist_pdf 5 2 0.30000000000000004 : 0.08080317000000005)
(kumaraswamy_dist_pdf 5 2 0.4 : 0.25337856000000003)
(kumaraswamy_dist_pdf 5 2 0.5 : 0.60546875)
(kumaraswamy_dist_pdf 5 2 0.6 : 1.19522304)
(kumaraswamy_dist_pdf 5 2 0.7 : 1.9974639299999997)
(kumaraswamy_dist_pdf 5 2 0.7999999999999999 : 2.7538227199999996)
(kumaraswamy_dist_pdf 5 2 0.8999999999999999 : 2.686795110000001)
(kumaraswamy_dist_pdf 5 2 0.9999999999999999 : 5.551115123125781e-15)

CDF
(kumaraswamy_dist_cdf 5 2 0 : 0)
(kumaraswamy_dist_cdf 5 2 0.1 : 1.9999899999900705e-5)
(kumaraswamy_dist_cdf 5 2 0.2 : 0.000639897599999939)
(kumaraswamy_dist_cdf 5 2 0.30000000000000004 : 0.004854095100000122)
(kumaraswamy_dist_cdf 5 2 0.4 : 0.020375142400000046)
(kumaraswamy_dist_cdf 5 2 0.5 : 0.0615234375)
(kumaraswamy_dist_cdf 5 2 0.6 : 0.14947338239999985)
(kumaraswamy_dist_cdf 5 2 0.7 : 0.30789247509999995)
(kumaraswamy_dist_cdf 5 2 0.7999999999999999 : 0.5479858175999999)
(kumaraswamy_dist_cdf 5 2 0.8999999999999999 : 0.8323015598999998)
(kumaraswamy_dist_cdf 5 2 0.9999999999999999 : 1.0)

Quantile
(kumaraswamy_dist_quantile 5 2 0.0001 : 0.13797365603854203)
(kumaraswamy_dist_quantile 5 2 0.001 : 0.21868335278532866)
(kumaraswamy_dist_quantile 5 2 0.01 : 0.3467464049018456)
(kumaraswamy_dist_quantile 5 2 0.05 : 0.47939630267353195)
(kumaraswamy_dist_quantile 5 2 0.25 : 0.6689666868752515)
(kumaraswamy_dist_quantile 5 2 0.5 : 0.7822433203748376)
(kumaraswamy_dist_quantile 5 2 0.75 : 0.8705505632961241)
(kumaraswamy_dist_quantile 5 2 0.9 : 0.9267920340127743)
(kumaraswamy_dist_quantile 5 2 0.95 : 0.9506405707268322)
(kumaraswamy_dist_quantile 5 2 0.99 : 0.9791483623609768)
(kumaraswamy_dist_quantile 5 2 0.999 : 0.9935938923578883)
(kumaraswamy_dist_quantile 5 2 0.9999 : 0.997991951661426)
(kumaraswamy_dist_quantile 5 2 0.99999 : 0.9993667429467064)

Mean
0.7575757575757573

|#
(displayln "Testing Kumaraswamy")
(let ([a 5]
      [b 2])  
  (displayln (format "kumaraswamy_dist ~a ~a" a b))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (kumaraswamy_dist a b)) 10))
  
  (displayln "PDF")
  (for ([x (range 0 1 0.1)])
    (let ([v (kumaraswamy_dist_pdf a b x)])
    (show2 "kumaraswamy_dist_pdf" a b x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 1 0.1)])
    (let ([v (kumaraswamy_dist_cdf a b x)])
    (show2 "kumaraswamy_dist_cdf" a b x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "kumaraswamy_dist_quantile" a b q ":" (kumaraswamy_dist_quantile a b q))
    )
  (newline)
  (displayln "Mean")
  (displayln (kumaraswamy_dist_mean a b))
)
(newline)


#|
  Logistic distribution

Testing Logistic (0 1)

logistic01_dist
Samples:
(-1.6807748033574388 -0.7982355307446657 1.3132279786767391 -0.81997148420762 -0.5141509798953625 -0.19998624527600403 -6.065593398370094 0.5173660523359996 -2.7427725719188394 0.6814184797774626)
PDF
(logistic01_dist_pdf 0 : 1/4)
(logistic01_dist_pdf 0.5 : 0.2350037122015945)
(logistic01_dist_pdf 1.0 : 0.19661193324148188)
(logistic01_dist_pdf 1.5 : 0.14914645207033284)
(logistic01_dist_pdf 2.0 : 0.1049935854035065)
(logistic01_dist_pdf 2.5 : 0.07010371654510818)
(logistic01_dist_pdf 3.0 : 0.045176659730912144)
(logistic01_dist_pdf 3.5 : 0.028453023879735556)
(logistic01_dist_pdf 4.0 : 0.017662706213291114)
(logistic01_dist_pdf 4.5 : 0.010866229722225234)

CDF
(logistic01_dist_cdf 0 : 1/2)
(logistic01_dist_cdf 0.5 : 0.6224593312018546)
(logistic01_dist_cdf 1.0 : 0.7310585786300049)
(logistic01_dist_cdf 1.5 : 0.8175744761936437)
(logistic01_dist_cdf 2.0 : 0.8807970779778823)
(logistic01_dist_cdf 2.5 : 0.9241418199787566)
(logistic01_dist_cdf 3.0 : 0.9525741268224334)
(logistic01_dist_cdf 3.5 : 0.9706877692486436)
(logistic01_dist_cdf 4.0 : 0.9820137900379085)
(logistic01_dist_cdf 4.5 : 0.9890130573694068)

Quantile
(logistic01_dist_quantile 0.0001 : -9.21024036697585)
(logistic01_dist_quantile 0.001 : -6.906754778648554)
(logistic01_dist_quantile 0.01 : -4.59511985013459)
(logistic01_dist_quantile 0.05 : -2.9444389791664403)
(logistic01_dist_quantile 0.25 : -1.0986122886681098)
(logistic01_dist_quantile 0.5 : -0.0)
(logistic01_dist_quantile 0.75 : 1.09861228866811)
(logistic01_dist_quantile 0.9 : 2.197224577336219)
(logistic01_dist_quantile 0.95 : 2.9444389791664416)
(logistic01_dist_quantile 0.99 : 4.595119850134584)
(logistic01_dist_quantile 0.999 : 6.906754778648465)
(logistic01_dist_quantile 0.9999 : 9.210240366976679)
(logistic01_dist_quantile 0.99999 : 11.512915464924033)

Mean
0

logistic_dist 3 1
Samples:
(4.57429284045633 3.46670918603689 3.44460332941786 9.394184769920836 2.754677460745312 -0.225786686888084 5.309726560238014 4.162671782401409 1.4647740164760126 3.4449556689428684)
PDF
(logistic_dist_pdf 3 1 -10 : 2.2603191888376427e-6)
(logistic_dist_pdf 3 1 -8 : 1.6701142910603434e-5)
(logistic_dist_pdf 3 1 -6 : 0.0001233793497648489)
(logistic_dist_pdf 3 1 -4 : 0.0009102211801218265)
(logistic_dist_pdf 3 1 -2 : 0.006648056670790155)
(logistic_dist_pdf 3 1 0 : 0.04517665973091213)
(logistic_dist_pdf 3 1 2 : 0.19661193324148185)
(logistic_dist_pdf 3 1 4 : 0.19661193324148188)
(logistic_dist_pdf 3 1 6 : 0.045176659730912144)
(logistic_dist_pdf 3 1 8 : 0.006648056670790156)

CDF
(logistic_dist_cdf 3 1 -10 : 2.2603242979035746e-6)
(logistic_dist_cdf 3 1 -8 : 1.670142184809518e-5)
(logistic_dist_cdf 3 1 -6 : 0.00012339457598623172)
(logistic_dist_cdf 3 1 -4 : 0.0009110511944006454)
(logistic_dist_cdf 3 1 -2 : 0.0066928509242848554)
(logistic_dist_cdf 3 1 0 : 0.04742587317756678)
(logistic_dist_cdf 3 1 2 : 0.2689414213699951)
(logistic_dist_cdf 3 1 4 : 0.7310585786300049)
(logistic_dist_cdf 3 1 6 : 0.9525741268224334)
(logistic_dist_cdf 3 1 8 : 0.9933071490757153)

Quantile
(logistic_dist_quantile 3 1 0.0001 : -6.2102403669758495)
(logistic_dist_quantile 3 1 0.001 : -3.906754778648554)
(logistic_dist_quantile 3 1 0.01 : -1.5951198501345898)
(logistic_dist_quantile 3 1 0.05 : 0.05556102083355974)
(logistic_dist_quantile 3 1 0.25 : 1.9013877113318902)
(logistic_dist_quantile 3 1 0.5 : 3.0)
(logistic_dist_quantile 3 1 0.75 : 4.09861228866811)
(logistic_dist_quantile 3 1 0.9 : 5.19722457733622)
(logistic_dist_quantile 3 1 0.95 : 5.944438979166439)
(logistic_dist_quantile 3 1 0.99 : 7.595119850134589)
(logistic_dist_quantile 3 1 0.999 : 9.906754778648553)
(logistic_dist_quantile 3 1 0.9999 : 12.21024036697596)
(logistic_dist_quantile 3 1 0.99999 : 14.51291546492478)

Mean
3

|#
(displayln "Testing Logistic (0 1)")
(let ([a 3]
      [b 1])
  (newline)
  (displayln (format "logistic01_dist"))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (logistic01_dist)) 10))
  
  (displayln "PDF")
  (for ([x (range 0 5 0.5)])
    (let ([v (logistic01_dist_pdf x)])
    (show2 "logistic01_dist_pdf" x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 5 0.5)])
    (let ([v (logistic01_dist_cdf x)])
    (show2 "logistic01_dist_cdf" x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "logistic01_dist_quantile" q ":" (logistic01_dist_quantile q))
    )
  (newline)
  (displayln "Mean")
  (displayln (logistic01_dist_mean))


  (newline)
  (displayln (format "logistic_dist ~a ~a" a b))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (logistic_dist a b)) 10))
  
  (displayln "PDF")
  (for ([x (range -10 10 2)])
    (let ([v (logistic_dist_pdf a b x)])
    (show2 "logistic_dist_pdf" a b x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range -10 10 2)])
    (let ([v (logistic_dist_cdf a b x)])
    (show2 "logistic_dist_cdf" a b x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "logistic_dist_quantile" a b q ":" (logistic_dist_quantile a b q))
    )
  (newline)
  (displayln "Mean")
  (displayln (logistic_dist_mean a b))
)
(newline)


#|
  Discrete uniform

discrete_uniform_dist 5 10
Samples:
(8 6 9 10 10 6 10 7 9 10)
PDF
(discrete_uniform_dist_pdf 5 10 3 : 0)
(discrete_uniform_dist_pdf 5 10 3.5 : 0)
(discrete_uniform_dist_pdf 5 10 4.0 : 0)
(discrete_uniform_dist_pdf 5 10 4.5 : 0)
(discrete_uniform_dist_pdf 5 10 5.0 : 1/6)
(discrete_uniform_dist_pdf 5 10 5.5 : 1/6)
(discrete_uniform_dist_pdf 5 10 6.0 : 1/6)
(discrete_uniform_dist_pdf 5 10 6.5 : 1/6)
(discrete_uniform_dist_pdf 5 10 7.0 : 1/6)
(discrete_uniform_dist_pdf 5 10 7.5 : 1/6)
(discrete_uniform_dist_pdf 5 10 8.0 : 1/6)
(discrete_uniform_dist_pdf 5 10 8.5 : 1/6)
(discrete_uniform_dist_pdf 5 10 9.0 : 1/6)
(discrete_uniform_dist_pdf 5 10 9.5 : 1/6)
(discrete_uniform_dist_pdf 5 10 10.0 : 1/6)
(discrete_uniform_dist_pdf 5 10 10.5 : 0)
(discrete_uniform_dist_pdf 5 10 11.0 : 0)
(discrete_uniform_dist_pdf 5 10 11.5 : 0)
(discrete_uniform_dist_pdf 5 10 12.0 : 0)
(discrete_uniform_dist_pdf 5 10 12.5 : 0)

CDF
(discrete_uniform_dist_cdf 5 10 3 : 0)
(discrete_uniform_dist_cdf 5 10 3.5 : 0)
(discrete_uniform_dist_cdf 5 10 4.0 : 0)
(discrete_uniform_dist_cdf 5 10 4.5 : 0)
(discrete_uniform_dist_cdf 5 10 5.0 : 0.16666666666666666)
(discrete_uniform_dist_cdf 5 10 5.5 : 0.16666666666666666)
(discrete_uniform_dist_cdf 5 10 6.0 : 0.3333333333333333)
(discrete_uniform_dist_cdf 5 10 6.5 : 0.3333333333333333)
(discrete_uniform_dist_cdf 5 10 7.0 : 0.5)
(discrete_uniform_dist_cdf 5 10 7.5 : 0.5)
(discrete_uniform_dist_cdf 5 10 8.0 : 0.6666666666666666)
(discrete_uniform_dist_cdf 5 10 8.5 : 0.6666666666666666)
(discrete_uniform_dist_cdf 5 10 9.0 : 0.8333333333333334)
(discrete_uniform_dist_cdf 5 10 9.5 : 0.8333333333333334)
(discrete_uniform_dist_cdf 5 10 10.0 : 1.0)
(discrete_uniform_dist_cdf 5 10 10.5 : 1)
(discrete_uniform_dist_cdf 5 10 11.0 : 1)
(discrete_uniform_dist_cdf 5 10 11.5 : 1)
(discrete_uniform_dist_cdf 5 10 12.0 : 1)
(discrete_uniform_dist_cdf 5 10 12.5 : 1)

Quantile
(discrete_uniform_dist_quantile 5 10 0.0001 : 5.0)
(discrete_uniform_dist_quantile 5 10 0.001 : 5.0)
(discrete_uniform_dist_quantile 5 10 0.01 : 5.0)
(discrete_uniform_dist_quantile 5 10 0.05 : 5.0)
(discrete_uniform_dist_quantile 5 10 0.25 : 6.0)
(discrete_uniform_dist_quantile 5 10 0.5 : 7.0)
(discrete_uniform_dist_quantile 5 10 0.75 : 9.0)
(discrete_uniform_dist_quantile 5 10 0.9 : 10.0)
(discrete_uniform_dist_quantile 5 10 0.95 : 10.0)
(discrete_uniform_dist_quantile 5 10 0.99 : 10.0)
(discrete_uniform_dist_quantile 5 10 0.999 : 10.0)
(discrete_uniform_dist_quantile 5 10 0.9999 : 10.0)
(discrete_uniform_dist_quantile 5 10 0.99999 : 10.0)

Mean
15/2


|#
(displayln "Testing discrete_uniform_dist")
(let ([a 5]
      [b 10])
  (newline)
  (displayln (format "discrete_uniform_dist ~a ~a" a b))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (discrete_uniform_dist a b)) 10))
  
  (displayln "PDF")
  (for ([x (range (- a 2) (+ b 3) 0.5)])
    (let ([v (discrete_uniform_dist_pdf a b x)])
    (show2 "discrete_uniform_dist_pdf" a b x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range (- a 2) (+ 3 b) 0.5)])
    (let ([v (discrete_uniform_dist_cdf a b x)])
    (show2 "discrete_uniform_dist_cdf" a b x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "discrete_uniform_dist_quantile" a b q ":" (discrete_uniform_dist_quantile a b q))
    )
  (newline)
  (displayln "Mean")
  (displayln (discrete_uniform_dist_mean a b))
)
(newline)


#|
  Negative hypergeometric 

Samples:
(8 5 10 9 8 10 12 7 0 2)
PDF
(negative_hypergeometric_pdf 3 10 20 0 : 6/203 ( 0.029556650246305417 ))
(negative_hypergeometric_pdf 3 10 20 2 : 760/7917 ( 0.09599595806492359 ))
(negative_hypergeometric_pdf 3 10 20 4 : 323/2639 ( 0.12239484653277757 ))
(negative_hypergeometric_pdf 3 10 20 6 : 10336/95381 ( 0.10836539772072006 ))
(negative_hypergeometric_pdf 3 10 20 8 : 3876/51359 ( 0.07546875912693005 ))
(negative_hypergeometric_pdf 3 10 20 10 : 2992/70035 ( 0.042721496394659816 ))
(negative_hypergeometric_pdf 3 10 20 12 : 13/667 ( 0.019490254872563718 ))
(negative_hypergeometric_pdf 3 10 20 14 : 32/4669 ( 0.006853715999143285 ))
(negative_hypergeometric_pdf 3 10 20 16 : 102/60697 ( 0.0016804784420976326 ))
(negative_hypergeometric_pdf 3 10 20 18 : 152/667667 ( 0.0002276583985729413 ))
(negative_hypergeometric_pdf 3 10 20 20 : 1/130065 ( 7.688463460577404e-6 ))

CDF
(negative_hypergeometric_cdf 3 10 20 0 : 6/203 ( 0.029556650246305417 ))
(negative_hypergeometric_cdf 3 10 20 2 : 1514/7917 ( 0.19123405330301882 ))
(negative_hypergeometric_cdf 3 10 20 4 : 485/1131 ( 0.4288240495137047 ))
(negative_hypergeometric_cdf 3 10 20 6 : 939109/1430715 ( 0.6563913847272168 ))
(negative_hypergeometric_cdf 3 10 20 8 : 57761/70035 ( 0.8247447704719069 ))
(negative_hypergeometric_cdf 3 10 20 10 : 21611/23345 ( 0.9257228528592847 ))
(negative_hypergeometric_cdf 3 10 20 12 : 9754/10005 ( 0.974912543728136 ))
(negative_hypergeometric_cdf 3 10 20 14 : 3026/3045 ( 0.99376026272578 ))
(negative_hypergeometric_cdf 3 10 20 16 : 909568/910455 ( 0.9990257618443525 ))
(negative_hypergeometric_cdf 3 10 20 18 : 110048/110055 ( 0.9999363954386443 ))
(negative_hypergeometric_cdf 3 10 20 20 : 1 ( 1.0 ))

Quantile
(negative_hypergeometric_quantile 3 10 20 0.0001 : 0)
(negative_hypergeometric_quantile 3 10 20 0.001 : 0)
(negative_hypergeometric_quantile 3 10 20 0.01 : 0)
(negative_hypergeometric_quantile 3 10 20 0.05 : 1)
(negative_hypergeometric_quantile 3 10 20 0.25 : 3)
(negative_hypergeometric_quantile 3 10 20 0.5 : 5)
(negative_hypergeometric_quantile 3 10 20 0.75 : 8)
(negative_hypergeometric_quantile 3 10 20 0.9 : 10)
(negative_hypergeometric_quantile 3 10 20 0.95 : 11)
(negative_hypergeometric_quantile 3 10 20 0.99 : 14)
(negative_hypergeometric_quantile 3 10 20 0.999 : 16)
(negative_hypergeometric_quantile 3 10 20 0.9999 : 18)
(negative_hypergeometric_quantile 3 10 20 0.99999 : 19)

Mean
(60/11 5.454545454545454)


|#
(displayln "Testing negative_hypergeometric")
(let ([w 3]
      [wtot 10]
      [btot 20])
  (newline)
  (displayln (format "negative_hypergeometric ~a ~a ~a" w wtot btot))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (negative_hypergeometric w wtot btot)) 10))
  
  (displayln "PDF")
  (for ([x (range 0 21 2)])
    (let ([v (negative_hypergeometric_pdf w wtot btot x)])
    (show2 "negative_hypergeometric_pdf" w wtot btot x ":" v "(" (* 1.0 v) ")")
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 21 2)])
    (let ([v (negative_hypergeometric_cdf w wtot btot x)])
    (show2 "negative_hypergeometric_cdf" w wtot btot x ":" v "(" (* 1.0 v) ")")
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "negative_hypergeometric_quantile" w wtot btot q ":" (negative_hypergeometric_quantile w wtot btot q))
    )
  (newline)
  (displayln "Mean")
  (displayln (list (negative_hypergeometric_mean w wtot btot) (* 1.0 (negative_hypergeometric_mean w wtot btot))))
)
(newline)


#|
  Dirichlet distribution

Samples
(p: 1 gen: ((0.3984528582775535 0.01564436525613553 0.585902776466311)))
(p: 2 gen: ((0.8905598398355751 0.10492868433809728 0.004511475826327719)))
(p: 3 gen: ((0.48891136909512695 0.31081135988382275 0.2002772710210503)))
(p: 4 gen: ((0.3000171717924038 0.49728052692440194 0.20270230128319439)))
(p: 5 gen: ((0.7315593603176798 0.11561792066900238 0.15282271901331781)))
(p: 6 gen: ((0.08928296869845251 0.011582924234059183 0.8991341070674883)))
(p: 7 gen: ((0.3374445012869317 0.1666181268991926 0.49593737181387565)))
(p: 8 gen: ((0.1722301951586905 0.30318612687449387 0.5245836779668157)))
(p: 9 gen: ((0.8229028773960518 0.020394361592193277 0.15670276101175498)))
(p: 10 gen: ((0.7959093433626195 0.00023332223784290382 0.2038573343995376)))


|#
(displayln "Testing dirichlet_dist")
(displayln "Samples")
(for ([p (range 1 11)])
  (show2 "p:" p "gen:"(repeat (lambda () (dirichlet_dist (ones-list 3 1))) 1))
  )

(newline)

#|
  Pareto type I distribution

pareto_dist x:1000 alpha:2
Samples:
(2140.907129866442 5220.041778186123 1572.4856410110974 3740.520433511243 1071.56374234293 1087.2018336486742 1225.8448291907134 1173.0377873803477 1058.7806821759589 1636.3718241216607)
PDF
(pareto_dist_pdf 1000 2 1000 : 1/500 ( 0.002 ))
(pareto_dist_pdf 1000 2 2000 : 1/4000 ( 0.00025 ))
(pareto_dist_pdf 1000 2 3000 : 1/13500 ( 7.407407407407407e-5 ))
(pareto_dist_pdf 1000 2 4000 : 1/32000 ( 3.125e-5 ))
(pareto_dist_pdf 1000 2 5000 : 1/62500 ( 1.6e-5 ))
(pareto_dist_pdf 1000 2 6000 : 1/108000 ( 9.259259259259259e-6 ))
(pareto_dist_pdf 1000 2 7000 : 1/171500 ( 5.830903790087464e-6 ))
(pareto_dist_pdf 1000 2 8000 : 1/256000 ( 3.90625e-6 ))
(pareto_dist_pdf 1000 2 9000 : 1/364500 ( 2.7434842249657065e-6 ))
(pareto_dist_pdf 1000 2 10000 : 1/500000 ( 2e-6 ))

CDF
(pareto_dist_cdf 1000 2 1000 : 0 ( 0 ))
(pareto_dist_cdf 1000 2 2000 : 3/4 ( 0.75 ))
(pareto_dist_cdf 1000 2 3000 : 8/9 ( 0.8888888888888888 ))
(pareto_dist_cdf 1000 2 4000 : 15/16 ( 0.9375 ))
(pareto_dist_cdf 1000 2 5000 : 24/25 ( 0.96 ))
(pareto_dist_cdf 1000 2 6000 : 35/36 ( 0.9722222222222222 ))
(pareto_dist_cdf 1000 2 7000 : 48/49 ( 0.9795918367346939 ))
(pareto_dist_cdf 1000 2 8000 : 63/64 ( 0.984375 ))
(pareto_dist_cdf 1000 2 9000 : 80/81 ( 0.9876543209876543 ))
(pareto_dist_cdf 1000 2 10000 : 99/100 ( 0.99 ))

Quantile
(pareto_dist_quantile 1000 2 0.0001 : 1000.0500037503126)
(pareto_dist_quantile 1000 2 0.001 : 1000.5003753127737)
(pareto_dist_quantile 1000 2 0.01 : 1005.0378152592122)
(pareto_dist_quantile 1000 2 0.05 : 1025.9783520851543)
(pareto_dist_quantile 1000 2 0.25 : 1154.7005383792514)
(pareto_dist_quantile 1000 2 0.5 : 1414.213562373095)
(pareto_dist_quantile 1000 2 0.75 : 2000.0)
(pareto_dist_quantile 1000 2 0.9 : 3162.2776601683795)
(pareto_dist_quantile 1000 2 0.95 : 4472.135954999578)
(pareto_dist_quantile 1000 2 0.99 : 9999.999999999996)
(pareto_dist_quantile 1000 2 0.999 : 31622.776601683778)
(pareto_dist_quantile 1000 2 0.9999 : 100000.0000000055)
(pareto_dist_quantile 1000 2 0.99999 : 316227.76601755753)

Mean
(2000 2000.0)

|#
(displayln "Testing pareto_dist")

(let ([xm 1000]
      [alpha 2])
  (newline)
  (displayln (format "pareto_dist x:~a alpha:~a" xm alpha))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (pareto_dist xm alpha)) 10))
  
  (displayln "PDF")
  (for ([x (range xm 11000 1000)])
    (let ([v (pareto_dist_pdf xm alpha x)])
    (show2 "pareto_dist_pdf" xm alpha x ":" v "(" (* 1.0 v) ")")
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range xm 11000 1000)])
    (let ([v (pareto_dist_cdf xm alpha x)])
    (show2 "pareto_dist_cdf" xm alpha x ":" v "(" (* 1.0 v) ")")
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "pareto_dist_quantile" xm alpha q ":" (pareto_dist_quantile xm alpha q))
    )
  (newline)
  (displayln "Mean")
  (displayln (list (pareto_dist_mean xm alpha) (* 1.0 (pareto_dist_mean xm alpha))))
)
(newline)


#|
  Pareto II distribution

pareto2 k:2 alpha:3 mu:1

dist: (1.2632417895966719 2.727661989263551 1.0021153001795136 5.8012100215408235 1.2160642491981286 1.9498671983007942 1.3136170029558851 1.9053503506503735 2.341523992471838 2.3503434509056875)

PDF
(pareto2_pdf 2 3 1 0): 0 (0)
(pareto2_pdf 2 3 1 1): 3/2 (1.5)
(pareto2_pdf 2 3 1 2): 8/27 (0.2962962962962963)
(pareto2_pdf 2 3 1 3): 3/32 (0.09375)
(pareto2_pdf 2 3 1 4): 24/625 (0.0384)
(pareto2_pdf 2 3 1 5): 1/54 (0.018518518518518517)
(pareto2_pdf 2 3 1 6): 24/2401 (0.009995835068721367)
(pareto2_pdf 2 3 1 7): 3/512 (0.005859375)
(pareto2_pdf 2 3 1 8): 8/2187 (0.003657978966620942)
(pareto2_pdf 2 3 1 9): 3/1250 (0.0024)

CDF
(pareto2_cdf 2 3 1 0): 0 (0) 
(pareto2_cdf 2 3 1 1): 0 (0) 
(pareto2_cdf 2 3 1 2): 19/27 (0.7037037037037037) 
(pareto2_cdf 2 3 1 3): 7/8 (0.875) 
(pareto2_cdf 2 3 1 4): 117/125 (0.936) 
(pareto2_cdf 2 3 1 5): 26/27 (0.9629629629629629) 
(pareto2_cdf 2 3 1 6): 335/343 (0.9766763848396501) 
(pareto2_cdf 2 3 1 7): 63/64 (0.984375) 
(pareto2_cdf 2 3 1 8): 721/729 (0.9890260631001372) 
(pareto2_cdf 2 3 1 9): 124/125 (0.992) 

Quantile
(pareto2_quantile 2 3 1 0.0001): 1.000066671111457
(pareto2_quantile 2 3 1 0.001): 1.0006671114570787
(pareto2_quantile 2 3 1 0.01): 1.0067114596959716
(pareto2_quantile 2 3 1 0.05): 1.034489536382202
(pareto2_quantile 2 3 1 0.25): 1.2012848325964178
(pareto2_quantile 2 3 1 0.5): 1.5198420997897464
(pareto2_quantile 2 3 1 0.75): 2.1748021039363987
(pareto2_quantile 2 3 1 0.9): 3.3088693800637676
(pareto2_quantile 2 3 1 0.95): 4.428835233189811
(pareto2_quantile 2 3 1 0.99): 8.283177667225555
(pareto2_quantile 2 3 1 0.999): 18.999999999999993
(pareto2_quantile 2 3 1 0.9999): 42.08869380063925
(pareto2_quantile 2 3 1 0.99999): 91.83177667239639
(pareto2_quantile 2 3 1 0.99999999): 927.3177651676976

mean: 2
variance: 3

|#

(displayln "Testing pareto2 ")
(let ([k 2]
      [alpha 3]
      [mu 1])
  (displayln (format "pareto2 k:~a alpha:~a mu:~a" k alpha mu))
  (newline)
  (show "dist" (for/list ([i 10]) (pareto2_dist k alpha mu)))
  (newline)
  (displayln "PDF")
  (for ([x (range 0 10)])
    (let ([v (pareto2_pdf k alpha mu x)])
      (displayln (format "(pareto2_pdf ~a ~a ~a ~a): ~a (~a)" k alpha mu x v (* 1.0 v) ))
      )
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 0 10)])
    (let ({v (pareto2_cdf k alpha mu x)})
      (displayln (format "(pareto2_cdf ~a ~a ~a ~a): ~a (~a) " k alpha mu x v (* 1.0 v) ))
      ))
  (newline)
  
  (displayln "Quantile")
  (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.99999999))
  (for ([q ps])
    (displayln (format "(pareto2_quantile ~a ~a ~a ~a): ~a" k alpha mu q (pareto2_quantile k alpha mu q)))
    )
  (newline)
  
  (show "mean" (pareto2_mean k alpha mu))
  (show "variance" (pareto2_variance k alpha mu))
  
  )

(newline)

#|
  Pareto type III

pareto3 k:2 gamma:0.5 mu:0.3

dist: (4.358298095028898 4.850388602561011 1.6513611820505187 4.301961336128339 2.964820991142011 2.081532288301443 1.6904694719638436 1.4168097482949646 5.805097887304018 4.635756816604069)

PDF
(pareto3_pdf 2 0.5 0.3 0): 0 (0)
(pareto3_pdf 2 0.5 0.3 0.5): 0.09802960494069209 (0.09802960494069209)
(pareto3_pdf 2 0.5 0.3 1.0): 0.27777639991865116 (0.27777639991865116)
(pareto3_pdf 2 0.5 0.3 1.5): 0.32439446366782015 (0.32439446366782015)
(pareto3_pdf 2 0.5 0.3 2.0): 0.2864840611643471 (0.2864840611643471)
(pareto3_pdf 2 0.5 0.3 2.5): 0.22522061382854572 (0.22522061382854572)
(pareto3_pdf 2 0.5 0.3 3.0): 0.16945947917884327 (0.16945947917884327)
(pareto3_pdf 2 0.5 0.3 3.5): 0.12624668602449185 (0.12624668602449185)
(pareto3_pdf 2 0.5 0.3 4.0): 0.09458800055346757 (0.09458800055346757)
(pareto3_pdf 2 0.5 0.3 4.5): 0.07175047235727636 (0.07175047235727636)

CDF
(pareto3_cdf 2 0.5 0.3 0): 0 (0) 
(pareto3_cdf 2 0.5 0.3 0.5): 0.00990099009900991 (0.00990099009900991) 
(pareto3_cdf 2 0.5 0.3 1.0): 0.10913140311804015 (0.10913140311804015) 
(pareto3_cdf 2 0.5 0.3 1.5): 0.2647058823529411 (0.2647058823529411) 
(pareto3_cdf 2 0.5 0.3 2.0): 0.41944847605224966 (0.41944847605224966) 
(pareto3_cdf 2 0.5 0.3 2.5): 0.5475113122171946 (0.5475113122171946) 
(pareto3_cdf 2 0.5 0.3 3.0): 0.6457041629760851 (0.6457041629760851) 
(pareto3_cdf 2 0.5 0.3 3.5): 0.7191011235955056 (0.7191011235955056) 
(pareto3_cdf 2 0.5 0.3 4.0): 0.7738835500282646 (0.7738835500282646) 
(pareto3_cdf 2 0.5 0.3 4.5): 0.8151571164510166 (0.8151571164510166) 

Quantile
(pareto3_quantile 2 0.5 0.3 0.0001): 0.32000100007499793
(pareto3_quantile 2 0.5 0.3 0.001): 0.36327719971683603
(pareto3_quantile 2 0.5 0.3 0.01): 0.501007563051843
(pareto3_quantile 2 0.5 0.3 0.05): 0.7588314677411232
(pareto3_quantile 2 0.5 0.3 0.25): 1.4547005383792515
(pareto3_quantile 2 0.5 0.3 0.5): 2.3
(pareto3_quantile 2 0.5 0.3 0.75): 3.764101615137754
(pareto3_quantile 2 0.5 0.3 0.9): 6.300000000000001
(pareto3_quantile 2 0.5 0.3 0.95): 9.017797887081343
(pareto3_quantile 2 0.5 0.3 0.99): 20.199748742132392
(pareto3_quantile 2 0.5 0.3 0.999): 63.5139225171164
(pareto3_quantile 2 0.5 0.3 0.9999): 200.28999974999851
(pareto3_quantile 2 0.5 0.3 0.99999): 632.7523697495491
(pareto3_quantile 2 0.5 0.3 0.99999999): 20000.299849752406

mean: 3.4415926535897934
variance: +nan.0

|#
(displayln "Testing pareto3 ")
(let ([k 2]
      [gamma 0.5]
      [mu 0.3])
  (displayln (format "pareto3 k:~a gamma:~a mu:~a" k gamma mu))
  (newline)
  (show "dist" (for/list ([i 10]) (pareto3_dist k gamma mu)))
  (newline)
  (displayln "PDF")
  (for ([x (range 0 5 0.5)])
    (let ([v (pareto3_pdf k gamma mu x)])
      (displayln (format "(pareto3_pdf ~a ~a ~a ~a): ~a (~a)" k gamma mu x v (* 1.0 v) ))
      )
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 0 5 0.5)])
    (let ({v (pareto3_cdf k gamma mu x)})
      (displayln (format "(pareto3_cdf ~a ~a ~a ~a): ~a (~a) " k gamma mu x v (* 1.0 v) ))
      ))
  (newline)
  
  (displayln "Quantile")
  (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.99999999))
  (for ([q ps])
    (displayln (format "(pareto3_quantile ~a ~a ~a ~a): ~a" k gamma mu q (pareto3_quantile k gamma mu q)))
    )
  (newline)
  
  (show "mean" (pareto3_mean k gamma mu))
  (show "variance" (pareto3_variance k gamma mu))
  
  )

(newline)


#|
  Pareto type IV

pareto4 k:2 alpha:3 gamma:1 mu:1

dist: (1.2026180268299456 1.0189109609577476 1.2075721293370445 1.6882692298793796 2.5886557754548343 1.1085857305671687 1.378355498696192 1.2075130649480972 1.8973525436808245 1.1036318940568637)

PDF
(pareto4_pdf 2 3 1 1 0): 0 (0)
(pareto4_pdf 2 3 1 1 0.5): 0 (0)
(pareto4_pdf 2 3 1 1 1.0): 1.5 (1.5)
(pareto4_pdf 2 3 1 1 1.5): 0.6144000000000001 (0.6144000000000001)
(pareto4_pdf 2 3 1 1 2.0): 0.2962962962962963 (0.2962962962962963)
(pareto4_pdf 2 3 1 1 2.5): 0.15993336109954187 (0.15993336109954187)
(pareto4_pdf 2 3 1 1 3.0): 0.09375 (0.09375)
(pareto4_pdf 2 3 1 1 3.5): 0.058527663465935076 (0.058527663465935076)
(pareto4_pdf 2 3 1 1 4.0): 0.038400000000000004 (0.038400000000000004)
(pareto4_pdf 2 3 1 1 4.5): 0.026227716686018712 (0.026227716686018712)

CDF
(pareto4_cdf 2 3 1 1 0): 0 (0) 
(pareto4_cdf 2 3 1 1 0.5): 0 (0) 
(pareto4_cdf 2 3 1 1 1.0): 0.0 (0.0) 
(pareto4_cdf 2 3 1 1 1.5): 0.488 (0.488) 
(pareto4_cdf 2 3 1 1 2.0): 0.7037037037037037 (0.7037037037037037) 
(pareto4_cdf 2 3 1 1 2.5): 0.8134110787172012 (0.8134110787172012) 
(pareto4_cdf 2 3 1 1 3.0): 0.875 (0.875) 
(pareto4_cdf 2 3 1 1 3.5): 0.9122085048010974 (0.9122085048010974) 
(pareto4_cdf 2 3 1 1 4.0): 0.9359999999999999 (0.9359999999999999) 
(pareto4_cdf 2 3 1 1 4.5): 0.9519158527422991 (0.9519158527422991) 

Quantile
(pareto4_quantile 2 3 1 1 0.0001): 1.000066671111457
(pareto4_quantile 2 3 1 1 0.001): 1.0006671114570787
(pareto4_quantile 2 3 1 1 0.01): 1.0067114596959716
(pareto4_quantile 2 3 1 1 0.05): 1.034489536382202
(pareto4_quantile 2 3 1 1 0.25): 1.2012848325964178
(pareto4_quantile 2 3 1 1 0.5): 1.5198420997897464
(pareto4_quantile 2 3 1 1 0.75): 2.1748021039363987
(pareto4_quantile 2 3 1 1 0.9): 3.3088693800637676
(pareto4_quantile 2 3 1 1 0.95): 4.428835233189811
(pareto4_quantile 2 3 1 1 0.99): 8.283177667225555
(pareto4_quantile 2 3 1 1 0.999): 18.999999999999993
(pareto4_quantile 2 3 1 1 0.9999): 42.08869380063925
(pareto4_quantile 2 3 1 1 0.99999): 91.83177667239639
(pareto4_quantile 2 3 1 1 0.99999999): 927.3177651676976

mean: 2
variance: 0

|#
(displayln "Testing pareto4 ")
(let ([k 2]
      [alpha 3]
      [gamma 1]
      [mu 1]
      [x 1.5])
  (displayln (format "pareto4 k:~a alpha:~a gamma:~a mu:~a" k alpha gamma mu))
  (newline)
  (show "dist" (for/list ([i 10]) (pareto4_dist k alpha gamma mu)))
  (newline)
  (displayln "PDF")
  (for ([x (range 0 5 0.5)])
    (let ([v (pareto4_pdf k alpha gamma mu x)])
      (displayln (format "(pareto4_pdf ~a ~a ~a ~a ~a): ~a (~a)" k alpha gamma mu x v (* 1.0 v) ))
      )
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 0 5 0.5)])
    (let ({v (pareto4_cdf k alpha gamma mu x)})
      (displayln (format "(pareto4_cdf ~a ~a ~a ~a ~a): ~a (~a) " k alpha gamma mu x v (* 1.0 v) ))
      ))
  (newline)
  
  (displayln "Quantile")
  (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.99999999))
  (for ([q ps])
    (displayln (format "(pareto4_quantile ~a ~a ~a ~a ~a): ~a" k alpha gamma mu q (pareto4_quantile k alpha gamma mu q)))
    )
  (newline)
  
  (show "mean" (pareto4_mean k alpha gamma mu))
  (show "variance" (pareto4_variance k alpha gamma mu))
  
  )

(newline)


#|
  Poisson distribution

poisson_dist lambda:4
Samples:
(1 3 6 4 6 3 6 5 2 4)
PDF
(poisson_dist_pdf 4 0 : 0.01831563888873418)
(poisson_dist_pdf 4 1 : 0.07326255555493671)
(poisson_dist_pdf 4 2 : 0.14652511110987343)
(poisson_dist_pdf 4 3 : 0.19536681481316456)
(poisson_dist_pdf 4 4 : 0.19536681481316456)
(poisson_dist_pdf 4 5 : 0.15629345185053165)
(poisson_dist_pdf 4 6 : 0.1041956345670211)
(poisson_dist_pdf 4 7 : 0.059540362609726345)
(poisson_dist_pdf 4 8 : 0.029770181304863173)
(poisson_dist_pdf 4 9 : 0.013231191691050298)
(poisson_dist_pdf 4 10 : 0.0052924766764201195)
(poisson_dist_pdf 4 11 : 0.0019245369732436798)

CDF
(poisson_dist_cdf 4 0 : 0.01831563888873418)
(poisson_dist_cdf 4 1 : 0.0915781944436709)
(poisson_dist_cdf 4 2 : 0.2381033055535443)
(poisson_dist_cdf 4 3 : 0.43347012036670884)
(poisson_dist_cdf 4 4 : 0.6288369351798734)
(poisson_dist_cdf 4 5 : 0.785130387030405)
(poisson_dist_cdf 4 6 : 0.8893260215974261)
(poisson_dist_cdf 4 7 : 0.9488663842071525)
(poisson_dist_cdf 4 8 : 0.9786365655120157)
(poisson_dist_cdf 4 9 : 0.991867757203066)
(poisson_dist_cdf 4 10 : 0.997160233879486)
(poisson_dist_cdf 4 11 : 0.9990847708527297)

Quantile
(poisson_dist_quantile 4 0.0001 : 0)
(poisson_dist_quantile 4 0.001 : 0)
(poisson_dist_quantile 4 0.01 : 0)
(poisson_dist_quantile 4 0.05 : 1)
(poisson_dist_quantile 4 0.25 : 3)
(poisson_dist_quantile 4 0.5 : 4)
(poisson_dist_quantile 4 0.75 : 5)
(poisson_dist_quantile 4 0.9 : 7)
(poisson_dist_quantile 4 0.95 : 8)
(poisson_dist_quantile 4 0.99 : 9)
(poisson_dist_quantile 4 0.999 : 11)
(poisson_dist_quantile 4 0.9999 : 13)
(poisson_dist_quantile 4 0.99999 : 15)

Mean
4

|#
(displayln "Testing poisson_dist")

(let ([lambda_ 4])
  (newline)
  (displayln (format "poisson_dist lambda:~a" lambda_))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (poisson_dist lambda_)) 10))
  
  (displayln "PDF")
  (for ([x (range 0 12)])
    (let ([v (poisson_dist_pdf lambda_ x)])
    (show2 "poisson_dist_pdf" lambda_ x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 12)])
    (let ([v (poisson_dist_cdf lambda_ x)])
    (show2 "poisson_dist_cdf" lambda_ x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "poisson_dist_quantile" lambda_ q ":" (poisson_dist_quantile lambda_ q))
    )
  (newline)
  (displayln "Mean")
  (displayln (poisson_dist_mean lambda_))
)
(newline)


#|
  Triangular distribution
|#
(displayln "Testing triangular_dist")

(let ([min-val 1]
      [max-val 1.4]
      [mode 1.25]
      )
  (newline)
  (displayln (format "triangular_dist min-val:~a max-val:~a mode:~a" min-val max-val mode))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (triangular_dist min-val max-val mode)) 10))
  
  (displayln "PDF")
  (for ([x (range 1 1.405 0.05)])
    (let ([v (triangular_dist_pdf min-val max-val mode x)])
    (show2 "triangular_dist_pdf" min-val max-val mode x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 1 1.405 0.05)])
    (let ([v (triangular_dist_cdf min-val max-val mode x)])
    (show2 "triangular_dist_cdf" min-val max-val mode x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "triangular_dist_quantile" min-val max-val mode q ":" (triangular_dist_quantile min-val max-val mode q))
    )
  (newline)
  (displayln "Mean")
  (displayln (triangular_dist_mean min-val max-val mode))
)
(newline)


#|
  Zipf distribution
  Zipf1 distribution: only pdf, cdf, mean (sampling and quantile are too slow)

Testing Zipf and Zipf1

zipf n:15 s:0.3
Samples:
(1 1 4 1 8 1 2 4 1 1)

PDF
(zipf_pdf 15 0.3 0 : 0)
(zipf_pdf 15 0.3 1 : 0.40531035255802167)
(zipf_pdf 15 0.3 2 : 0.16460715256662187)
(zipf_pdf 15 0.3 3 : 0.0971695218411239)
(zipf_pdf 15 0.3 4 : 0.06685127706480755)
(zipf_pdf 15 0.3 5 : 0.05001804248785701)
(zipf_pdf 15 0.3 6 : 0.03946308848412122)
(zipf_pdf 15 0.3 7 : 0.03229685582099085)
(zipf_pdf 15 0.3 8 : 0.027150054997682287)
(zipf_pdf 15 0.3 9 : 0.023295521358490365)
(zipf_pdf 15 0.3 10 : 0.020313637435904926)
(zipf_pdf 15 0.3 11 : 0.01794639432721853)
(zipf_pdf 15 0.3 12 : 0.016026994094422813)
(zipf_pdf 15 0.3 13 : 0.014443130614994144)
(zipf_pdf 15 0.3 14 : 0.013116599267685825)

CDF
(zipf_cdf 15 0.3 0 : 0)
(zipf_cdf 15 0.3 1 : 0.40531035255802167)
(zipf_cdf 15 0.3 2 : 0.5699175051246436)
(zipf_cdf 15 0.3 3 : 0.6670870269657674)
(zipf_cdf 15 0.3 4 : 0.733938304030575)
(zipf_cdf 15 0.3 5 : 0.783956346518432)
(zipf_cdf 15 0.3 6 : 0.8234194350025532)
(zipf_cdf 15 0.3 7 : 0.8557162908235441)
(zipf_cdf 15 0.3 8 : 0.8828663458212263)
(zipf_cdf 15 0.3 9 : 0.9061618671797167)
(zipf_cdf 15 0.3 10 : 0.9264755046156217)
(zipf_cdf 15 0.3 11 : 0.9444218989428402)
(zipf_cdf 15 0.3 12 : 0.960448893037263)
(zipf_cdf 15 0.3 13 : 0.9748920236522571)
(zipf_cdf 15 0.3 14 : 0.988008622919943)

Quantile
(zipf_quantile 15 0.3 0.0001 : 1)
(zipf_quantile 15 0.3 0.001 : 1)
(zipf_quantile 15 0.3 0.01 : 1)
(zipf_quantile 15 0.3 0.05 : 1)
(zipf_quantile 15 0.3 0.25 : 1)
(zipf_quantile 15 0.3 0.5 : 2)
(zipf_quantile 15 0.3 0.75 : 5)
(zipf_quantile 15 0.3 0.9 : 9)
(zipf_quantile 15 0.3 0.95 : 12)
(zipf_quantile 15 0.3 0.99 : 15)
(zipf_quantile 15 0.3 0.999 : 15)
(zipf_quantile 15 0.3 0.9999 : 15)
(zipf_quantile 15 0.3 0.99999 : 15)

Mean
3.5773795828075943


Testing zipf1
zipf s2: 5
Samples:
(1 1 1 1 1 1 1 1 1 1)

PDF
(zipf1_pdf 5 0 : 0)
(zipf1_pdf 5 1 : 0.9829525922645803)
(zipf1_pdf 5 2 : 0.015358634254134067)
(zipf1_pdf 5 3 : 0.001348357465383512)
(zipf1_pdf 5 4 : 0.0002399786602208448)
(zipf1_pdf 5 5 : 6.290896590493313e-5)
(zipf1_pdf 5 6 : 2.1068085396617375e-5)
(zipf1_pdf 5 7 : 8.354959177422506e-6)
(zipf1_pdf 5 8 : 3.7496665659507e-6)
(zipf1_pdf 5 9 : 1.8495987179472045e-6)
(zipf1_pdf 5 10 : 9.829525922645802e-7)
(zipf1_pdf 5 11 : 5.548511128121359e-7)
(zipf1_pdf 5 12 : 3.291888343221465e-7)
(zipf1_pdf 5 13 : 2.0364439369044442e-7)
(zipf1_pdf 5 14 : 1.3054623714722666e-7)

CDF
(zipf1_cdf 5 0 : 0)
(zipf1_cdf 5 1 : 0.9829525922645803)
(zipf1_cdf 5 2 : 0.9983112265187144)
(zipf1_cdf 5 3 : 0.9996595839840978)
(zipf1_cdf 5 4 : 0.9998995626443187)
(zipf1_cdf 5 5 : 0.9999624716102237)
(zipf1_cdf 5 6 : 0.9999835396956203)
(zipf1_cdf 5 7 : 0.9999918946547977)
(zipf1_cdf 5 8 : 0.9999956443213637)
(zipf1_cdf 5 9 : 0.9999974939200815)
(zipf1_cdf 5 10 : 0.999998476872674)
(zipf1_cdf 5 11 : 0.9999990317237868)
(zipf1_cdf 5 12 : 0.9999993609126211)
(zipf1_cdf 5 13 : 0.9999995645570148)
(zipf1_cdf 5 14 : 0.9999996951032518)


Quantile
(zipf1_quantile 5 0.0001 : 1)
(zipf1_quantile 5 0.001 : 1)
(zipf1_quantile 5 0.01 : 1)
(zipf1_quantile 5 0.05 : 1)
(zipf1_quantile 5 0.25 : 1)
(zipf1_quantile 5 0.5 : 1)
(zipf1_quantile 5 0.75 : 1)
(zipf1_quantile 5 0.9 : 1)
(zipf1_quantile 5 0.95 : 1)
(zipf1_quantile 5 0.99 : 2)
(zipf1_quantile 5 0.999 : 3)
(zipf1_quantile 5 0.9999 : 5)
(zipf1_quantile 5 0.99999 : 7)

Mean
1.0192508249092675


|#
(displayln "Testing Zipf and Zipf1")

(let ([n 15]
      [s 0.3]
      [s2 5]
      )
  (newline)
  (displayln (format "zipf n:~a s:~a" n s))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (zipf n s)) 10))
  (newline)
  (displayln "PDF")
  (for ([x (range 0 15)])
    (let ([v (zipf_pdf n s x)])
    (show2 "zipf_pdf" n s x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 15)])
    (let ([v (zipf_cdf n s x)])
    (show2 "zipf_cdf" n s x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "zipf_quantile" n s q ":" (zipf_quantile n s q))
    )
  (newline)
  (displayln "Mean")
  (displayln (zipf_mean n s))

  (newline)
  ;;; Zipf1
  (newline)
  (displayln "Testing zipf1")
  (displayln (format "zipf s2: ~a" s2))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (zipf1 s2)) 10))
  (newline)
  (displayln "PDF")
  (for ([x (range 0 15)])
    (let ([v (zipf1_pdf s2 x)])
    (show2 "zipf1_pdf" s2 x ":" v)
    ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 15)])
    (let ([v (zipf1_cdf s2 x)])
    (show2 "zipf1_cdf" s2 x ":" v)
    ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "zipf1_quantile" s2 q ":" (zipf1_quantile s2 q))
    )
  (newline)
  (displayln "Mean")
  (displayln (zipf1_mean s2))
  
  
)
(newline)

#|
  Chinese restraurant process (CRP)

crp theta: 0.1 n: 100

PDF
crp_pdf 0.1 100 0: 0
crp_pdf 0.1 100 1: 0.6005322939093096
crp_pdf 0.1 100 2: 0.3109182397102609
crp_pdf 0.1 100 3: 0.07557805231073238
crp_pdf 0.1 100 4: 0.011589434101126067
crp_pdf 0.1 100 5: 0.0012683507188841299
crp_pdf 0.1 100 6: 0.0001061240506210572
crp_pdf 0.1 100 7: 7.095334026313765e-6
crp_pdf 0.1 100 8: 3.909522685585095e-7
crp_pdf 0.1 100 9: 1.816280270559394e-8

CDF
crp_cdf 0.1 100 0: 0
crp_cdf 0.1 100 1: 0.6005322939093096
crp_cdf 0.1 100 2: 0.9114505336195705
crp_cdf 0.1 100 3: 0.9870285859303028
crp_cdf 0.1 100 4: 0.998618020031429
crp_cdf 0.1 100 5: 0.9998863707503131
crp_cdf 0.1 100 6: 0.9999924948009341
crp_cdf 0.1 100 7: 0.9999995901349604
crp_cdf 0.1 100 8: 0.999999981087229
crp_cdf 0.1 100 9: 0.9999999992500317

Quantile
crp_quantile 0.1 100 0.0001: 1
crp_quantile 0.1 100 0.001: 1
crp_quantile 0.1 100 0.01: 1
crp_quantile 0.1 100 0.05: 1
crp_quantile 0.1 100 0.25: 1
crp_quantile 0.1 100 0.5: 1
crp_quantile 0.1 100 0.75: 2
crp_quantile 0.1 100 0.9: 2
crp_quantile 0.1 100 0.95: 3
crp_quantile 0.1 100 0.99: 4
crp_quantile 0.1 100 0.999: 5
crp_quantile 0.1 100 0.9999: 6
crp_quantile 0.1 100 0.99999: 6

crp_mean 0.1 100: 1.502492130512861


  Also, see gamble_chinese_restaurant_process2.rkt

|#
(displayln "Testing CRP")
(let* ([theta 0.1]
       [n 100]
       ; The probability of higher values (e.g. > 10) is very small
       [max-k (+ 3 (crp_quantile theta n 0.999999999))])
  (displayln (format  "crp theta: ~a n: ~a" theta n))  
  (newline)
  (displayln "PDF")
  (for ([k max-k])
    (displayln (format  "crp_pdf ~a ~a ~a: ~a" theta n k (crp_pdf theta n k)))
    )
  (newline)
  (displayln "CDF")
  (for ([k max-k])
    (displayln (format  "crp_cdf ~a ~a ~a: ~a" theta n k (crp_cdf theta n k)))
    )
  (newline)
  (displayln "Quantile")
  (for ([q (append ps (list 0.999999 0.9999999 0.99999999 0.999999999))])
    (displayln (format  "crp_quantile ~a ~a ~a: ~a" theta n q (crp_quantile theta n q)))
    )
  (newline)
  (displayln (format  "crp_mean ~a ~a: ~a" theta n (crp_mean theta n)))

  )
(newline)


#|
  Weibull distribution
|#
(displayln "Testing Weibull")
(let ([lambda_ 1000]
      [k 2])
  (newline)
  (displayln (format "weibull lambda_:~a k:~a" lambda_ k))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (weibull lambda_ k)) 10))
  (newline)
  (displayln "PDF")
  (for ([x (range 0 2100 100)])
    (let ([v (weibull_pdf lambda_ k x)])
      (show2 "weibull_pdf" lambda_ k x ":" v)
      ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 2100 100)])
    (let ([v (weibull_cdf lambda_ k x)])
      (show2 "weibull_cdf" lambda_ k x ":" v)
      ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "weibull_quantile" lambda_ k q ":" (weibull_quantile lambda_ k q))
    )
  (newline)
  (displayln "Mean")
  (displayln (weibull_mean lambda_ k))
)
(newline)

#|
  Pascal distribution

pascal n:10 p:1/2
Dist
(19 22 30 26 23 20 24 25 16 16)

PDF
(pascal_pdf 10 1/2 9: 0 (0)
(pascal_pdf 10 1/2 11: 5/1024 (0.0048828125)
(pascal_pdf 10 1/2 13: 55/2048 (0.02685546875)
(pascal_pdf 10 1/2 15: 1001/16384 (0.06109619140625)
(pascal_pdf 10 1/2 17: 715/8192 (0.0872802734375)
(pascal_pdf 10 1/2 19: 12155/131072 (0.09273529052734375)
(pascal_pdf 10 1/2 21: 20995/262144 (0.08008956909179688)
(pascal_pdf 10 1/2 23: 124355/2097152 (0.05929708480834961)
(pascal_pdf 10 1/2 25: 81719/2097152 (0.03896665573120117)
(pascal_pdf 10 1/2 27: 1562275/67108864 (0.023279711604118347)
(pascal_pdf 10 1/2 29: 1726725/134217728 (0.012865103781223297)
(pascal_pdf 10 1/2 31: 7153575/1073741824 (0.006662285886704922)
(pascal_pdf 10 1/2 33: 876525/268435456 (0.0032653100788593292)
(pascal_pdf 10 1/2 35: 6556407/4294967296 (0.0015265324618667364)
(pascal_pdf 10 1/2 37: 5883955/8589934592 (0.0006849825149402022)
(pascal_pdf 10 1/2 39: 20376455/68719476736 (0.0002965164458146319)
(pascal_pdf 10 1/2 41: 8544965/68719476736 (0.00012434560630936176)
(pascal_pdf 10 1/2 43: 222945905/4398046511104 (5.069202984486765e-5)
(pascal_pdf 10 1/2 45: 177232627/8796093022208 (2.0149016904724704e-5)
(pascal_pdf 10 1/2 47: 550858165/70368744177664 (7.828165351497773e-6)
(pascal_pdf 10 1/2 49: 104819165/35184372088832 (2.9791398503675737e-6)

CDF
(pascal_cdf 10 1/2 9): 0
(pascal_cdf 10 1/2 11): 0.005859374999999996
(pascal_cdf 10 1/2 13): 0.046142578125
(pascal_cdf 10 1/2 15): 0.15087890624999994
(pascal_cdf 10 1/2 17): 0.3145294189453128
(pascal_cdf 10 1/2 19): 0.5000000000000003
(pascal_cdf 10 1/2 21): 0.6681880950927734
(pascal_cdf 10 1/2 23): 0.7975635528564452
(pascal_cdf 10 1/2 25): 0.8852385282516483
(pascal_cdf 10 1/2 27): 0.9389609396457672
(pascal_cdf 10 1/2 29): 0.9692858271300793
(pascal_cdf 10 1/2 31): 0.9852753132581711
(pascal_cdf 10 1/2 33): 0.9932345065753907
(pascal_cdf 10 1/2 35): 0.9970059397164732
(pascal_cdf 10 1/2 37): 0.9987183960038237
(pascal_cdf 10 1/2 39): 0.9994674901827239
(pascal_cdf 10 1/2 41): 0.9997845714788127
(pascal_cdf 10 1/2 43): 0.9999149224126995
(pascal_cdf 10 1/2 45): 0.9999671266837709
(pascal_cdf 10 1/2 47): 0.9999875479846878
(pascal_cdf 10 1/2 49): 0.999995368226795

Quantiles
(pascal_quantile 10 1/2 0.0001): 10
(pascal_quantile 10 1/2 0.001): 11
(pascal_quantile 10 1/2 0.01): 12
(pascal_quantile 10 1/2 0.05): 14
(pascal_quantile 10 1/2 0.5): 19
(pascal_quantile 10 1/2 0.9): 26
(pascal_quantile 10 1/2 0.95): 28
(pascal_quantile 10 1/2 0.99): 33
(pascal_quantile 10 1/2 0.999): 38
(pascal_quantile 10 1/2 0.9999): 43
(pascal_quantile 10 1/2 0.99999): 48

(pascal_mean 10 1/2): 20
(pascal_variance 10 1/2): 20

|#

(displayln "Testing pascal_dist")
(let ([n 10]
      [p 1/2])
  (displayln (format "pascal n:~a p:~a" n p))
  (displayln "Dist")
  (displayln (for/list ([i 10]) (pascal_dist n p)))
  (newline)
  (displayln "PDF")
  (for ([k (range (sub1 n) (* n 5) 2)])
    (let ([v (pascal_pdf n p k)])
      (displayln (format "(pascal_pdf ~a ~a ~a: ~a (~a)" n p k v (* 1.0 v) ))
      )
    )
  (newline)
  (displayln "CDF")
  (for ([k (range (sub1 n) (* n 5) 2)])
    (displayln (format "(pascal_cdf ~a ~a ~a): ~a" n p k (pascal_cdf n p k)))
    )
  (newline)
  (displayln "Quantiles")
  (define ps '(0.0001 0.001 0.01 0.05  0.5 0.9 0.95 0.99 0.999 0.9999 0.99999))
  (for ([q ps])
    (displayln (format "(pascal_quantile ~a ~a ~a): ~a" n p q (pascal_quantile n p q)))
    )
  (newline)
  (displayln (format "(pascal_mean ~a ~a): ~a" n p (pascal_mean n p)))
  (displayln (format "(pascal_variance ~a ~a): ~a" n p (pascal_variance n p)))  
  )
(newline)


#|
  Birthday/coincidences

pbirthday
(pbirthday 23): 38093904702297390785243708291056390518886454060947061/750918832685153501254262074252231475
63269805908203125
(* 1.0 (pbirthday 23)): 0.5072972343239854
(* 1.0 (pbirthday 22)): 0.4756953076625501
(* 1.0 (pbirthday 100)): 0.9999996927510721
(pbirthday 30 3 20): 0.5985778029003122
(pbirthday 365 3 50): 0.13167863385753809
((* 1.0 (pbirthday 365 2  0 )) 0)
((* 1.0 (pbirthday 365 2  1 )) 0)
((* 1.0 (pbirthday 365 2  2 )) 0.0027397260273972603)
((* 1.0 (pbirthday 365 2  3 )) 0.008204165884781385)
((* 1.0 (pbirthday 365 2  4 )) 0.016355912466550306)
((* 1.0 (pbirthday 365 2  5 )) 0.02713557369979359)
((* 1.0 (pbirthday 365 2  6 )) 0.04046248364911149)
((* 1.0 (pbirthday 365 2  7 )) 0.056235703095975406)
((* 1.0 (pbirthday 365 2  8 )) 0.07433529235166902)
((* 1.0 (pbirthday 365 2  9 )) 0.09462383388916669)

qbirthday
(qbirthday): 23
(qbirthday 365 3 0.75): 274.0

(qbirthday 365 2 0.01): 4
(qbirthday 365 2 0.025): 5
(qbirthday 365 2 0.1): 10
(qbirthday 365 2 0.05): 7
(qbirthday 365 2 0.25): 15
(qbirthday 365 2 0.5): 23
(qbirthday 365 2 0.75): 32
(qbirthday 365 2 0.84): 37
(qbirthday 365 2 0.9): 41
(qbirthday 365 2 0.95): 47
(qbirthday 365 2 0.975): 52
(qbirthday 365 2 0.99): 57
(qbirthday 365 2 0.999): 70

rbirthday
(16 : 0.1)
(33 : 0.1)
(17 : 0.1)
(36 : 0.1)
(22 : 0.1)
(23 : 0.1)
(10 : 0.1)
(28 : 0.1)
(13 : 0.1)
(79 : 0.1)
(mean: 27.75)

   Also, see gamble_birthday_probabilities.rkt
|#
(let ([x 1])
  (displayln "pbirthday")
  (show "(pbirthday 23)" (pbirthday 365 2 23)) ; Using default classes = 365 and coincident = 2
  (show "(* 1.0 (pbirthday 23))" (* 1.0 (pbirthday 365 2 23)))
  (show "(* 1.0 (pbirthday 22))" (* 1.0 (pbirthday 365 2 22)))
  (show "(* 1.0 (pbirthday 100))" (* 1.0 (pbirthday 365 2 100)))
  (show "(pbirthday 30 3 20)" (pbirthday 30 3 20)) ; Example with coincident = 3
  (show "(pbirthday 365 3 50)" (pbirthday 365 3 50))
  
  (for ([n 10])
    (show2 "(* 1.0 (pbirthday 365 2 " n "))" (* 1.0 (pbirthday 365 2 n)))
    )
  
  (newline)
  (displayln "qbirthday")
  (show "(qbirthday)" (qbirthday 365 2 0.5)) ; prob = 0.5, classes = 365, coincident = 2
  (show "(qbirthday 365 3 0.75)" (qbirthday 0.75 365 3)) ; More specific case
  (newline)
  (for ([q (list 0.01 0.025 0.1 0.05 0.25 0.5 0.75 0.84 0.9 0.95 0.975 0.99 0.999)])
    (displayln (format "(qbirthday 365 2 ~a): ~a"
                       q
                       (qbirthday 365 2 q))))
  (newline)
  (displayln "rbirthday")
  
  (define rs (repeat (lambda () (rbirthday 365 2)) 10))
  rs
  (show-freq rs)
  (newline)
  )

(newline)


#|
  Matching distribution

(matching 25): 

Samples
(matching_dist: (1 3 1 0 0 0 2 0 0 0)): 
PDF:
(matching_pdf 25 0): 79253545592131482810517/215433472824041472000000 (0.36787944117144233)
(matching_pdf 25 1): 9923922230666898717143/26976017466662584320000 (0.36787944117144233)
(matching_pdf 25 2): 39299278806015611311/213653030899875840000 (0.18393972058572117)
(matching_pdf 25 3): 6563440628747948887/107047688359772160000 (0.061313240195240384)
(matching_pdf 25 4): 72289643288657479/4716086969696256000 (0.015328310048810096)
(matching_pdf 25 5): 4282366656425369/1396881535795200000 (0.003065662009762019)
(matching_pdf 25 6): 92079694567171/180214963568640000 (0.0005109436682936699)
(matching_pdf 25 7): 138547156531409/1898115498639360000 (7.299195261338141e-5)
(matching_pdf 25 8): 8178130767479/896332318801920000 (9.123994076672673e-6)
(matching_pdf 25 9): 15549624751/15338307059712000 (1.0137771196303048e-6)
(matching_pdf 25 10): 34361893981/338949196185600000 (1.0137771196301731e-7)
(matching_pdf 25 11): 2467007773/267682954936320000 (9.21615563302073e-9)
(matching_pdf 25 12): 63633137/82854247956480000 (7.68012969394447e-10)
(matching_pdf 25 13): 1456321/24650850631680000 (5.907792074843906e-11)
(matching_pdf 25 14): 1468457/347987841417216000 (4.219851458084165e-12)
(matching_pdf 25 15): 16481/58583811686400000 (2.8132344969670177e-13)
(matching_pdf 25 16): 1517/86277977210880000 (1.7582702435085604e-14)
(matching_pdf 25 17): 163/157596891217920000 (1.0342843614510691e-15)
(matching_pdf 25 18): 103/1792664637603840000 (5.745636849158504e-17)
(matching_pdf 25 19): 53/17516894458871808000 (3.0256504727158993e-18)
(matching_pdf 25 20): 1/6635187295027200000 (1.507116461881127e-19)
(matching_pdf 25 21): 1/136242512457891840000 (7.339852898771723e-21)
(matching_pdf 25 22): 1/3372002183332823040000 (2.9655971308168577e-22)
(matching_pdf 25 23): 1/51704033477769953280000 (1.934085085315342e-23)
(matching_pdf 25 24): 0 (0)
(matching_pdf 25 25): 1/15511210043330985984000000 (6.446950284384474e-26)
CDF:
(matching_cdf 25 0): 79253545592131482810517/215433472824041472000000 (0.36787944117144233)
(matching_cdf 25 1): 671324150898054913218497/912424120195940352000000 (0.7357588823428847)
(matching_cdf 25 2): 14265638206583666905893049/15511210043330985984000000 (0.9196986029286058)
(matching_cdf 25 3): 15216680753689244699619349/15511210043330985984000000 (0.9810118431238462)
(matching_cdf 25 4): 15454441390465639148050349/15511210043330985984000000 (0.9963401531726563)
(matching_cdf 25 5): 15501993517820918037739079/15511210043330985984000000 (0.9994058151824183)
(matching_cdf 25 6): 15509918872380131186011679/15511210043330985984000000 (0.999916758850712)
(matching_cdf 25 7): 5170350355296196735739593/5170403347776995328000000 (0.9999897508033253)
(matching_cdf 25 8): 5170397530025715861603193/5170403347776995328000000 (0.999998874797402)
(matching_cdf 25 9): 2585201385831164548925159/2585201673888497664000000 (0.9999998885745217)
(matching_cdf 25 10): 7755604943738985632110397/7755605021665492992000000 (0.9999999899522336)
(matching_cdf 25 11): 7755605015215848540016697/7755605021665492992000000 (0.9999999991683892)
(matching_cdf 25 12): 7755605021172253782156497/7755605021665492992000000 (0.9999999999364022)
(matching_cdf 25 13): 7755605021630438800982647/7755605021665492992000000 (0.9999999999954802)
(matching_cdf 25 14): 158277653503329924533503/158277653503377408000000 (0.9999999999997)
(matching_cdf 25 15): 7755605021665348135700827/7755605021665492992000000 (0.9999999999999813)
(matching_cdf 25 16): 369314524841213547628387/369314524841213952000000 (0.9999999999999989)
(matching_cdf 25 17): 738629049682427859209249/738629049682427904000000 (0.9999999999999999)
(matching_cdf 25 18): 5170403347776995311537343/5170403347776995328000000 (1.0)
(matching_cdf 25 19): 15511210043330985981543529/15511210043330985984000000 (1.0)
(matching_cdf 25 20): 15511210043330985983881249/15511210043330985984000000 (1.0)
(matching_cdf 25 21): 1193170003333152767999623/1193170003333152768000000 (1.0)
(matching_cdf 25 22): 2215887149047283711999957/2215887149047283712000000 (1.0)
(matching_cdf 25 23): 15511210043330985983999999/15511210043330985984000000 (1.0)
(matching_cdf 25 24): 15511210043330985983999999/15511210043330985984000000 (1.0)
(matching_cdf 25 25): 1 (1.0)
Quantile:
(matching_cumulative 25 0.0001): 0 
(matching_cumulative 25 0.001): 0 
(matching_cumulative 25 0.01): 0 
(matching_cumulative 25 0.05): 0 
(matching_cumulative 25 0.25): 0 
(matching_cumulative 25 0.5): 1 
(matching_cumulative 25 0.75): 2 
(matching_cumulative 25 0.9): 2 
(matching_cumulative 25 0.95): 3 
(matching_cumulative 25 0.99): 4 
(matching_cumulative 25 0.999): 5 
(matching_cumulative 25 0.9999): 6 
(matching_cumulative 25 0.99999): 8 
(matching_cumulative 25 0.999999): 9 
(matching_cumulative 25 0.9999999): 10 
(matching_cumulative 25 0.99999999): 11 

(matching_mean 25): 1

|#
(displayln "Testing matching distribution")
(let ([n 25])
  (displayln (format "(matching ~a): " n))
  (newline)
  (displayln "Samples")
  (displayln (format "(matching_dist: ~a): " (repeat (lambda () (matching_dist n)) 10)))
  (displayln "PDF:")
  (for ([i (add1 n)])
    (let ([v_pdf (matching_pdf n i)])
      (displayln (format "(matching_pdf ~a ~a): ~a (~a)" n i v_pdf (* 1.0 v_pdf)))
      ))
  (displayln "CDF:") 
  (for ([i (add1 n)])
    (let ([v_cdf (matching_cdf n i)])
      (displayln (format "(matching_cdf ~a ~a): ~a (~a)" n i v_cdf (* 1.0 v_cdf)))
      )
    )
  (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.999999 0.9999999 0.99999999))
  (displayln "Quantile:") 
  (for ([q ps])
    (displayln (format "(matching_cumulative ~a ~a): ~a " n q (matching_quantile n q)))
    )
  (newline)
  (displayln (format "(matching_mean ~a): ~a " n (matching_mean n))) 
  (displayln (format "(matching_variance ~a): ~a " n (matching_variance n))) 
  )
(newline)

#|
  Coupon collector distribution

Testing coupon_collector distribution
(coupon_collector 6 6): 

Samples
(coupon_collector_dist 6 6): (11 13 14 23 11 9 11 28 12 19) 
PDF:
(coupon_collector_pdf 6 6 0): 0 (0)
(coupon_collector_pdf 6 6 2): 0 (0)
(coupon_collector_pdf 6 6 4): 0 (0)
(coupon_collector_pdf 6 6 6): 5/324 (0.015432098765432098)
(coupon_collector_pdf 6 6 8): 175/2916 (0.06001371742112483)
(coupon_collector_pdf 6 6 10): 11585/139968 (0.08276891860996799)
(coupon_collector_pdf 6 6 12): 616825/7558272 (0.08160926201121103)
(coupon_collector_pdf 6 6 14): 37542505/544195584 (0.06898715480940029)
(coupon_collector_pdf 6 6 16): 43909775/816293376 (0.05379165909095898)
(coupon_collector_pdf 6 6 18): 28263758255/705277476864 (0.04007466448620215)
(coupon_collector_pdf 6 6 20): 368973211775/12694994583552 (0.029064463899264068)
(coupon_collector_pdf 6 6 22): 234028553605/11284439629824 (0.02073904963667656)
(coupon_collector_pdf 6 6 24): 120521110230125/8226356490141696 (0.014650606301168097)
(coupon_collector_pdf 6 6 26): 12183424870553755/1184595334580404224 (0.010284883381605794)
(coupon_collector_pdf 6 6 28): 51115172632007575/7107572007482425344 (0.007191650338286631)
CDF:
(coupon_collector_cdf 6 6 0): 0 (0)
(coupon_collector_cdf 6 6 2): 0 (0)
(coupon_collector_cdf 6 6 4): 0 (0)
(coupon_collector_cdf 6 6 6): 5/324 (0.015432098765432098)
(coupon_collector_cdf 6 6 8): 665/5832 (0.11402606310013717)
(coupon_collector_cdf 6 6 10): 38045/139968 (0.2718121284865112)
(coupon_collector_cdf 6 6 12): 1654565/3779136 (0.43781568062117904)
(coupon_collector_cdf 6 6 14): 317181865/544195584 (0.5828453488516364)
(coupon_collector_cdf 6 6 16): 2279105465/3265173504 (0.6980043976860594)
(coupon_collector_cdf 6 6 18): 553436255195/705277476864 (0.7847071164895858)
(coupon_collector_cdf 6 6 20): 2691299309615/3173748645888 (0.847987540885421)
(coupon_collector_cdf 6 6 22): 30241729508375/33853318889472 (0.8933165344027713)
(coupon_collector_cdf 6 6 24): 15225590090211325/16452712980283392 (0.9254151645663163)
(coupon_collector_cdf 6 6 26): 1122975934870626655/1184595334580404224 (0.9479827432111204)
(coupon_collector_cdf 6 6 28): 3425060881328615165/3553786003741212672 (0.9637780321389404)
Quantile:
(coupon_collector_cumulative 6 6 0.0001): 6 
(coupon_collector_cumulative 6 6 0.001): 6 
(coupon_collector_cumulative 6 6 0.01): 6 
(coupon_collector_cumulative 6 6 0.05): 7 
(coupon_collector_cumulative 6 6 0.25): 10 
(coupon_collector_cumulative 6 6 0.5): 13 
(coupon_collector_cumulative 6 6 0.75): 18 
(coupon_collector_cumulative 6 6 0.9): 23 
(coupon_collector_cumulative 6 6 0.95): 27 
(coupon_collector_cumulative 6 6 0.99): 36 
(coupon_collector_cumulative 6 6 0.999): 48 
(coupon_collector_cumulative 6 6 0.9999): 61 
(coupon_collector_cumulative 6 6 0.99999): 73 
(coupon_collector_cumulative 6 6 0.999999): 86 
(coupon_collector_cumulative 6 6 0.9999999): 99 
(coupon_collector_cumulative 6 6 0.99999999): 111 

(coupon_collector_mean 6 6): 14.7 
(coupon_collector_variance 6 6): 38.99 


|#
(displayln "Testing coupon_collector distribution")
(let ([m 6]
      [k 6])
  (displayln (format "(coupon_collector ~a ~a): " m k))
  (newline)
  (displayln "Samples")
  (displayln (format "(coupon_collector_dist ~a ~a): ~a " m k (repeat (lambda () (coupon_collector_dist m k)) 10)))
  (displayln "PDF:")
  (for ([n (range 0 30 2)])
    (let ([v_pdf (coupon_collector_pdf m k n)])
      (displayln (format "(coupon_collector_pdf ~a ~a ~a): ~a (~a)" m k n v_pdf (* 1.0 v_pdf)))
      ))
  (displayln "CDF:") 
  (for ([n (range 0 30 2)])
    (let ([v_cdf (coupon_collector_cdf m k n)])
      (displayln (format "(coupon_collector_cdf ~a ~a ~a): ~a (~a)" m k n v_cdf (* 1.0 v_cdf)))
      )
    )
  (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.999999 0.9999999 0.99999999))
  (displayln "Quantile:") 
  (for ([q ps])
    (displayln (format "(coupon_collector_cumulative ~a ~a ~a): ~a " m k q (coupon_collector_quantile m k q)))
    )
  (newline)
  (displayln (format "(coupon_collector_mean ~a ~a): ~a " m k (* 1.0 (coupon_collector_mean m k))))
  (displayln (format "(coupon_collector_variance ~a ~a): ~a " m k (* 1.0 (coupon_collector_variance m k))))
  )
(newline)


#|
  Records: num records (number of records)

PDF
(k 1 1/10 0.1)
(k 2 7129/25200 0.2828968253968254)
(k 3 1303/4032 0.32316468253968256)
(k 4 4523/22680 0.1994268077601411)
(k 5 19/256 0.07421875)
(k 6 3013/172800 0.017436342592592594)
(k 7 1/384 0.0026041666666666665)
(k 8 29/120960 0.00023974867724867725)
(k 9 1/80640 1.240079365079365e-5)
(k 10 1/3628800 2.755731922398589e-7)
'(1/10 7129/25200 1303/4032 4523/22680 19/256 3013/172800 1/384 29/120960 1/80640 1/3628800)
'(0.1 0.2828968253968254 0.32316468253968256 0.1994268077601411 0.07421875 0.017436342592592594 0.0026041666666666665 0.00023974867724867725 1.240079365079365e-5 2.755731922398589e-7)
sum: 1
CDF
(0 0 0)
(1 1/10 0.1)
(2 9649/25200 0.3828968253968254)
(3 71171/100800 0.7060615079365079)
(4 821459/907200 0.905488315696649)
(5 3555161/3628800 0.979707065696649)
(6 1809217/1814400 0.9971434082892416)
(7 906971/907200 0.9997475749559083)
(8 1814377/1814400 0.999987323633157)
(9 3628799/3628800 0.9999997244268077)
(10 1 1.0)
'(0 0.1 0.3828968253968254 0.7060615079365079 0.905488315696649 0.979707065696649 0.9971434082892416 0.9997475749559083 0.999987323633157 0.9999997244268077 1.0)

Quantiles
num_records_quantile 10 3 0.1: 2 
num_records_quantile 10 3 0.2: 2 
num_records_quantile 10 3 0.3: 2 
num_records_quantile 10 3 0.5: 3 
num_records_quantile 10 3 0.75: 4 
num_records_quantile 10 3 0.84: 4 
num_records_quantile 10 3 0.9: 4 
num_records_quantile 10 3 0.95: 5 
num_records_quantile 10 3 0.99: 6 
num_records_quantile 10 3 0.999: 7 
num_records_quantile 10 3 0.9999: 8 
num_records_quantile 10 3 0.99999: 9 
num_records_quantile 10 3 0.999999: 9 

Generate
(3 : 0.32856667775191223)
(2 : 0.2803458596607915)
(4 : 0.19055537080146326)
(1 : 0.1084137013634852)
(5 : 0.0734951779181909)
(6 : 0.017292983039574328)
(7 : 0.0013302294645826404)
(mean: 2.8979048885932825)

|#
(displayln "Testing records: num_records")
(let ([n 10])
  (displayln "PDF")
  (define num_records_10k (for/list ([k (range 1 (add1 n))])
                            (let ([t (num_records_pdf n k)]) 
                              (show2 "k" k t (* 1.0 t) )
                              t
                              )))
  num_records_10k
  (map exact->inexact num_records_10k)
  (show "sum" (sum num_records_10k))
  
  (displayln "CDF")
  (for/list ([k (range 0 (add1 n))])
    (let ([t (num_records_cdf n k)])
      (show2 k t (* 1.0 t))
      (* 1.0 t)
      ))
  (newline)
  (displayln "Quantiles")
  (for ([q '(0.1 0.2 0.3 0.5 0.75 0.84 0.90 0.95 0.99 0.999 0.9999 0.99999 0.999999)])
    (displayln (format "num_records_quantile ~a ~a: ~a " n q (num_records_quantile n q)))
    )
  
  (newline)
  (displayln "Generate")
  (show-freq (repeat (lambda () (num_records n)) 3000))

  )


#|
  Record: kth_record

k: 2
k_record_pdf n k: (0 1/2 1/6 1/12 1/20 1/30 1/42 1/56 1/72 1/90 1/110 1/132 1/156 1/182 1/210 1/240 1/272 1/306 1/342 1/380)
k_record_pdf n k: (0 0.5 0.16666666666666666 0.08333333333333333 0.05 0.03333333333333333 0.023809523809523808 0.017857142857142856 0.013888888888888888 0.011111111111111112 0.00909090909090909 0.007575757575757576 0.00641025641025641 0.005494505494505495 0.004761904761904762 0.004166666666666667 0.003676470588235294 0.0032679738562091504 0.0029239766081871343 0.002631578947368421)

k: 3
k_record_pdf n k: (0 0 1/6 1/8 11/120 5/72 137/2520 7/160 121/3360 761/25200 7129/277200 671/30240 83711/4324320 6617/388080 1145993/75675600 1171733/86486400 1195757/98017920 143327/12972960 42142223/4190266080 751279/81681600)
k_record_pdf n k: (0 0 0.16666666666666666 0.125 0.09166666666666666 0.06944444444444445 0.054365079365079366 0.04375 0.03601190476190476 0.0301984126984127 0.02571789321789322 0.02218915343915344 0.01935818810818811 0.017050608122036695 0.0151434940720655 0.01354817636067636 0.012199371298636005 0.011048133964800632 0.010057171118832626 0.00919765283735872)

k: 4
k_record_pdf n k: (0 0 0 1/24 1/20 7/144 5/112 29/720 469/12960 29531/907200 1303/44352 16103/604800 190553/7862400 128977/5821200 9061/445500 30946717/1651104000 13215487/762361600 58433327/3632428800 344499373/23005382400 784809203/56137536000)
k_record_pdf n k: (0 0 0 0.041666666666666664 0.05 0.04861111111111111 0.044642857142857144 0.04027777777777778 0.036188271604938274 0.03255180776014109 0.029378607503607504 0.026625330687830687 0.024235983923483924 0.022156428227856798 0.02033894500561167 0.018743045259414306 0.01733493266187594 0.016086571882702835 0.01497472926161836 0.013980114891398155)

k: 5
k_record_pdf n k: (0 0 0 0 1/120 1/72 17/1008 7/384 967/51840 89/4800 4523/249480 7645/435456 341747/20217600 412009/25401600 9301169/598752000 406841/27371520 35118025721/2470051584000 4446371981/326918592000 80847323107/6211453248000 2263547729/181621440000)
k_record_pdf n k: (0 0 0 0 0.008333333333333333 0.013888888888888888 0.016865079365079364 0.018229166666666668 0.018653549382716048 0.018541666666666668 0.018129709796376462 0.017556308788947678 0.016903440566635012 0.01621980505165029 0.01553425959328737 0.014863661206977179 0.014217527256710117 0.013600853820513212 0.013015846675338286 0.012462998470885375)


k_record_quantile 10 6 0.1: 2 
k_record_quantile 10 6 0.2: 2 
k_record_quantile 10 6 0.3: 2 
k_record_quantile 10 6 0.5: 2 
k_record_quantile 10 6 0.75: 4 
k_record_quantile 10 6 0.84: 7 
k_record_quantile 10 6 0.9: 11 
k_record_quantile 10 6 0.95: 20 
k_record_quantile 10 6 0.99: 100 
k_record_quantile 10 6 0.999: 1000 

|#
(newline)
(displayln "Testing record: k_record")


; Distribution of L(2), i.e. the second record
; P(L(2)=k)
(define (record-2nd k)
  (/ 1 (* (- k 1) k)))

(newline)
; Note that we keep k constant and vary n
(for ([k (range 2 6)])
  (show "k" k)
  (define k_record_pdf_nk (map (lambda (n) (k_record_pdf n k)) (range 1 21)))
  (show "k_record_pdf n k" k_record_pdf_nk)
  (show "k_record_pdf n k" (map (lambda (n) (* 1.0 n)) k_record_pdf_nk))
  (newline)
  ;; (define k_record_cdf_nk (map (lambda (n) (k_record_cdf n k)) (range 1 21)))
  ;; (show "k_record_cdf n k" k_record_cdf_nk)
  ;; (show "k_record_cdf n k" (map (lambda (n) (* 1.0 n)) k_record_cdf_nk))
  ;; (newline)
  )

(newline)

; Quantiles
; This is very slow!
#|
(0.1 k_record_quantile 10 3 0.5 3)
(0.2 k_record_quantile 10 3 0.5 4)
(0.3 k_record_quantile 10 3 0.5 5)
(0.5 k_record_quantile 10 3 0.5 7)
(0.75 k_record_quantile 10 3 0.5 18)
(0.84 k_record_quantile 10 3 0.5 32)
(0.9 k_record_quantile 10 3 0.5 56)
(0.95 k_record_quantile 10 3 0.5 129)
(0.99 k_record_quantile 10 3 0.5 830)

- (k_record_quantile-f 10 3 0.99)) takes about 2s
- (k_record_quantile   10 3 0.99)) takes about 11s
- (k_record_quantile-f 10 2 0.999)) takes about 2s
|#
(let ([n 10]
      [k 6])
  (for ([q '(0.1 0.2 0.3 0.5 0.75 0.84 0.90 0.95 0.99 0.999)]) ; 0.9999)])
    ; (show2 q "k_record_quantile 10 3 0.5" (k_record_quantile 10 3 q)))
    (displayln (format "k_record_quantile ~a ~a: ~a " n q (k_record_quantile-f n q)))
    )
  )
(newline)


#|
  Multivariate hypergeometric

multivariate_hypergeometric_dist n:5 balls:(12 23 9)
Generate: ((2 2 1) (2 1 2) (2 2 1) (0 3 2) (1 3 1) (0 5 0) (1 2 2) (0 5 0) (3 1 1) (2 3 0))

PDF
multivariate_hypergeometric_pdf 5 (12 23 9) (2 1 2): 621/12341
multivariate_hypergeometric_pdf 5 (12 23 9) (0 4 1): 1035/14104
multivariate_hypergeometric_pdf 5 (12 23 9) (3 2 0): 1265/24682
multivariate_hypergeometric_pdf 5 (12 23 9) (1 4 0): 345/3526
multivariate_hypergeometric_pdf 5 (12 23 9) (0 2 3): 69/3526

Mean: (15/11 115/44 45/44)

|#
(displayln "Testing multivariate_hypergeometric")
(let ([n 5]
      [balls (list 12 23 9)])
  (displayln (format "multivariate_hypergeometric_dist n:~a balls:~a" n balls))
  (show "Generate" (repeat (lambda () (multivariate_hypergeometric_dist n balls)) 10))
  (newline)
  (displayln "Generate (frequency)")
  (show-freq (for/list ([i 1000])
               (multivariate_hypergeometric_dist 5 (list 12 23 9))
               ))  
  (newline)
  (displayln "PDF")
  (for ([i 5])
    (let* ([r (multivariate_hypergeometric_dist n balls)]
           [t (multivariate_hypergeometric_pdf n balls r)])
      (displayln (format "multivariate_hypergeometric_pdf ~a ~a ~a: ~a (~a)" n balls r t (* 1.0 t)))
      )
    )
  (newline)  
  (show "Mean" (multivariate_hypergeometric_mean n balls))
    
  )

(newline)


#|
  Max stable distribution

max_stable mu_:2 sigma:2 xi:0.9
Samples:
(1.078430071446108 6.789548218228137 9.804184297635071 19.168604495119027 0.43275679302688275 4.6002176806203 8.758387342762962 2.4985685476780635 0.9607711394862266 2.5378471083925027)

PDF
(max_stable_pdf 2 2 0.9 0 : 0)
(max_stable_pdf 2 2 0.9 1 : 0.2530658734606315)
(max_stable_pdf 2 2 0.9 2 : 0.18393972058572117)
(max_stable_pdf 2 2 0.9 3 : 0.1177346713608556)
(max_stable_pdf 2 2 0.9 4 : 0.07900376876249132)
(max_stable_pdf 2 2 0.9 5 : 0.0559159717796068)
(max_stable_pdf 2 2 0.9 6 : 0.04136485792979756)
(max_stable_pdf 2 2 0.9 7 : 0.03170305128024689)
(max_stable_pdf 2 2 0.9 8 : 0.02499982678701601)
(max_stable_pdf 2 2 0.9 9 : 0.020177067396457343)
(max_stable_pdf 2 2 0.9 10 : 0.016600750435110567)

CDF
(max_stable_cdf 2 2 0.9 0 : 2.4596373191009287e-6)
(max_stable_cdf 2 2 0.9 1 : 0.1432650870655339)
(max_stable_cdf 2 2 0.9 2 : 0.36787944117144233)
(max_stable_cdf 2 2 0.9 3 : 0.515941134178969)
(max_stable_cdf 2 2 0.9 4 : 0.6125726873190812)
(max_stable_cdf 2 2 0.9 5 : 0.6790962928916664)
(max_stable_cdf 2 2 0.9 6 : 0.7272133134149918)
(max_stable_cdf 2 2 0.9 7 : 0.7634378887422975)
(max_stable_cdf 2 2 0.9 8 : 0.7915966877555759)
(max_stable_cdf 2 2 0.9 9 : 0.8140596792791923)
(max_stable_cdf 2 2 0.9 10 : 0.8323637129831066)

Quantile
(max_stable_quantile 2 2 0.9 0.0001 : 0.07903632275398942)
(max_stable_quantile 2 2 0.9 0.001 : 0.1680649138925827)
(max_stable_quantile 2 2 0.9 0.01 : 0.33994610066813946)
(max_stable_quantile 2 2 0.9 0.05 : 0.605595629938211)
(max_stable_quantile 2 2 0.9 0.25 : 1.4339960633533253)
(max_stable_quantile 2 2 0.9 0.5 : 2.868390381034809)
(max_stable_quantile 2 2 0.9 0.75 : 6.597488722970117)
(max_stable_quantile 2 2 0.9 0.9 : 16.61914662203211)
(max_stable_quantile 2 2 0.9 0.95 : 31.968665430714875)
(max_stable_quantile 2 2 0.9 0.99 : 139.35834820439396)
(max_stable_quantile 2 2 0.9 0.999 : 1113.0259024955985)
(max_stable_quantile 2 2 0.9 0.9999 : 8846.205675277271)
(max_stable_quantile 2 2 0.9 0.99999 : 70272.29844232416)

Mean
20.91890599704163


|#
(displayln "Testing Max stable distribution")
(let ([mu 2]
      [sigma 2]
      [xi 0.9])
  (displayln (format "max_stable mu_:~a sigma:~a xi:~a" mu sigma xi))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (max_stable_dist mu sigma xi)) 10))
  (newline)
  (displayln "PDF")
  (for ([x (range 0 11)])
    (let ([v (max_stable_pdf mu sigma xi x)])
      (show2 "max_stable_pdf" mu sigma xi x ":" v)
      ))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 11)])
    (let ([v (max_stable_cdf mu sigma xi x)])
      (show2 "max_stable_cdf" mu sigma xi x ":" v)
      ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "max_stable_quantile" mu sigma xi q ":" (max_stable_quantile mu sigma xi q))
    )
  (newline)
  (displayln "Mean")
  (displayln (max_stable_mean mu sigma xi))
  )
(newline)

#|
  Testing min_stable distribution

min_stable mu_:2 sigma:2 xi:0.9
Samples:
(1.0442180698196308 2.8482214631894682 -0.5140370867101987 0.8815463747791032 -7.2172623000476985 2.1239037755083734 -0.043101204149393446 -4.547916120227043 0.029660197758358597 2.659830290207107)

PDF
(min_stable_pdf 2 2 0.9 -5 : 0.020177067396457343)
(min_stable_pdf 2 2 0.9 -4 : 0.02499982678701601)
(min_stable_pdf 2 2 0.9 -3 : 0.03170305128024689)
(min_stable_pdf 2 2 0.9 -2 : 0.04136485792979756)
(min_stable_pdf 2 2 0.9 -1 : 0.0559159717796068)
(min_stable_pdf 2 2 0.9 0 : 0.07900376876249132)
(min_stable_pdf 2 2 0.9 1 : 0.1177346713608556)
(min_stable_pdf 2 2 0.9 2 : 0.18393972058572117)
(min_stable_pdf 2 2 0.9 3 : 0.2530658734606315)
(min_stable_pdf 2 2 0.9 4 : 0)
(min_stable_pdf 2 2 0.9 5 : 0)

CDF
(min_stable_cdf 2 2 0.9 -5 : 0.18594032072080768)
(min_stable_cdf 2 2 0.9 -4 : 0.2084033122444241)
(min_stable_cdf 2 2 0.9 -3 : 0.23656211125770255)
(min_stable_cdf 2 2 0.9 -2 : 0.27278668658500815)
(min_stable_cdf 2 2 0.9 -1 : 0.3209037071083336)
(min_stable_cdf 2 2 0.9 0 : 0.3874273126809188)
(min_stable_cdf 2 2 0.9 1 : 0.48405886582103097)
(min_stable_cdf 2 2 0.9 2 : 0.6321205588285577)
(min_stable_cdf 2 2 0.9 3 : 0.8567349129344661)
(min_stable_cdf 2 2 0.9 4 : 0.9999975403626808)
(min_stable_cdf 2 2 0.9 5 : 1)

Quantile
(min_stable_quantile 2 2 0.9 0.0001 : -8842.205675277271)
(min_stable_quantile 2 2 0.9 0.001 : -1109.0259024955985)
(min_stable_quantile 2 2 0.9 0.01 : -135.35834820439396)
(min_stable_quantile 2 2 0.9 0.05 : -27.968665430714875)
(min_stable_quantile 2 2 0.9 0.25 : -2.597488722970117)
(min_stable_quantile 2 2 0.9 0.5 : 1.131609618965191)
(min_stable_quantile 2 2 0.9 0.75 : 2.5660039366466747)
(min_stable_quantile 2 2 0.9 0.9 : 3.173179038114975)
(min_stable_quantile 2 2 0.9 0.95 : 3.394404370061789)
(min_stable_quantile 2 2 0.9 0.99 : 3.6600538993318605)
(min_stable_quantile 2 2 0.9 0.999 : 3.8319350861074173)
(min_stable_quantile 2 2 0.9 0.9999 : 3.9209636772460135)
(min_stable_quantile 2 2 0.9 0.99999 : 3.975777022988361)

Mean
-16.91890599704163


|#
(displayln "Testing Min stable distribution")
(let ([mu 2]
      [sigma 2]
      [xi 0.9])
  (displayln (format "min_stable mu_:~a sigma:~a xi:~a" mu sigma xi))
  (displayln "Samples:")   
  (displayln (repeat (lambda () (min_stable_dist mu sigma xi)) 10))
  (newline)
  (displayln "PDF")
  (for ([x (range -5 6)])
    (let ([v (min_stable_pdf mu sigma xi x)])
      (show2 "min_stable_pdf" mu sigma xi x ":" v)
      ))
  (newline)
  (displayln "CDF")
  (for ([x (range -5 6)])
    (let ([v (min_stable_cdf mu sigma xi x)])
      (show2 "min_stable_cdf" mu sigma xi x ":" v)
      ))
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "min_stable_quantile" mu sigma xi q ":" (min_stable_quantile mu sigma xi q))
    )
  (newline)
  (displayln "Mean")
  (displayln (min_stable_mean mu sigma xi))
  )
(newline)


#|
  Poisson process

poisson_process mu:4 t:2

PDF
(poisson_process_pdf 4 2 0: 0.00033546262790251185)
(poisson_process_pdf 4 2 1: 0.002683701023220095)
(poisson_process_pdf 4 2 2: 0.01073480409288038)
(poisson_process_pdf 4 2 3: 0.02862614424768101)
(poisson_process_pdf 4 2 4: 0.05725228849536202)
(poisson_process_pdf 4 2 5: 0.09160366159257924)
(poisson_process_pdf 4 2 6: 0.12213821545677231)
(poisson_process_pdf 4 2 7: 0.13958653195059692)
(poisson_process_pdf 4 2 8: 0.13958653195059692)
(poisson_process_pdf 4 2 9: 0.1240769172894195)
(poisson_process_pdf 4 2 10: 0.09926153383153559)
(poisson_process_pdf 4 2 11: 0.07219020642293499)
(poisson_process_pdf 4 2 12: 0.048126804281956655)
(poisson_process_pdf 4 2 13: 0.029616494942742554)
(poisson_process_pdf 4 2 14: 0.01692371139585289)
(poisson_process_pdf 4 2 15: 0.009025979411121541)
(poisson_process_pdf 4 2 16: 0.004512989705560771)
(poisson_process_pdf 4 2 17: 0.0021237598614403624)
(poisson_process_pdf 4 2 18: 0.0009438932717512722)
(poisson_process_pdf 4 2 19: 0.00039742874600053567)
(poisson_process_pdf 4 2 20: 0.00015897149840021427)

CDF
(poisson_process_cdf 4 2 0: 0.0003354626279025119)
(poisson_process_cdf 4 2 1: 0.0030191636511226077)
(poisson_process_cdf 4 2 2: 0.013753967744002981)
(poisson_process_cdf 4 2 3: 0.042380111991684004)
(poisson_process_cdf 4 2 4: 0.09963240048704604)
(poisson_process_cdf 4 2 5: 0.19123606207962526)
(poisson_process_cdf 4 2 6: 0.3133742775363976)
(poisson_process_cdf 4 2 7: 0.45296080948699446)
(poisson_process_cdf 4 2 8: 0.5925473414375915)
(poisson_process_cdf 4 2 9: 0.7166242587270109)
(poisson_process_cdf 4 2 10: 0.8158857925585464)
(poisson_process_cdf 4 2 11: 0.8880759989814814)
(poisson_process_cdf 4 2 12: 0.9362028032634381)
(poisson_process_cdf 4 2 13: 0.9658192982061807)
(poisson_process_cdf 4 2 14: 0.9827430096020335)
(poisson_process_cdf 4 2 15: 0.9917689890131551)
(poisson_process_cdf 4 2 16: 0.9962819787187158)
(poisson_process_cdf 4 2 17: 0.9984057385801562)
(poisson_process_cdf 4 2 18: 0.9993496318519075)
(poisson_process_cdf 4 2 19: 0.9997470605979081)
(poisson_process_cdf 4 2 20: 0.9999060320963082)

Quantile
(poisson_process_quantile 4 2 0.01: 2)
(poisson_process_quantile 4 2 0.025: 3)
(poisson_process_quantile 4 2 0.25: 6)
(poisson_process_quantile 4 2 0.5: 8)
(poisson_process_quantile 4 2 0.75: 10)
(poisson_process_quantile 4 2 0.9: 12)
(poisson_process_quantile 4 2 0.99: 15)
(poisson_process_quantile 4 2 0.999: 18)
(poisson_process_quantile 4 2 0.9999: 20)
(poisson_process_quantile 4 2 0.99999: 23)
(poisson_process_quantile 4 2 0.999999: 25)
(poisson_process_quantile 4 2 0.9999999: 27)
(poisson_process_quantile 4 2 0.99999999: 28)
(poisson_process_quantile 4 2 0.999999999: 30)
(poisson_process_quantile 4 2 0.9999999999: 32)
(poisson_process_quantile 4 2 0.99999999999: 33)

(poisson_process_quantile 4 2 0.0001: 0)
(poisson_process_quantile 4 2 0.001: 1)
(poisson_process_quantile 4 2 0.01: 2)
(poisson_process_quantile 4 2 0.05: 4)
(poisson_process_quantile 4 2 0.25: 6)
(poisson_process_quantile 4 2 0.5: 8)
(poisson_process_quantile 4 2 0.75: 10)
(poisson_process_quantile 4 2 0.9: 12)
(poisson_process_quantile 4 2 0.95: 13)
(poisson_process_quantile 4 2 0.99: 15)
(poisson_process_quantile 4 2 0.999: 18)
(poisson_process_quantile 4 2 0.9999: 20)
(poisson_process_quantile 4 2 0.99999: 23)

(poisson_process_mean 4 2): 8

  
|#
(displayln "Testing poisson_process")

(let ([mu 4]
      [t 2])
  (displayln (format "poisson_process mu:~a t:~a" mu t))
  (for/list ([i 5]) (poisson_process_dist mu t))
  (newline)
  (displayln "PDF")
  (for ([x (range 0 21)])
    (displayln (format "(poisson_process_pdf ~a ~a ~a: ~a)" mu t x (poisson_process_pdf mu t x)))
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 0 21)])
    (displayln (format "(poisson_process_cdf ~a ~a ~a: ~a)" mu t x (poisson_process_cdf mu t x)))    
    )
  (newline)
  (displayln "Quantile")
  (define qs1 '(0.01
               0.025
               0.25
               0.5
               0.75
               0.9
               0.99
               0.999
               0.9999
               0.99999
               0.999999
               0.9999999
               0.99999999
               0.999999999
               0.9999999999
               0.99999999999
               ))
  (for ([q qs1])
    (displayln (format "(poisson_process_quantile ~a ~a ~a: ~a)" mu t q (poisson_process_quantile mu t q)))
    )
  (newline)
  (for ([q ps])
    (displayln (format "(poisson_process_quantile ~a ~a ~a: ~a)" mu t q (poisson_process_quantile mu t q)))
    )
  (newline)
  
  (displayln (format "(poisson_process_mean ~a ~a): ~a" mu t (poisson_process_mean mu t)))

)

(newline)

#| 
  Random walk process

random_walk process p:3/5 t:10 x:4

dist: (2 -2 -2 8 -4 4 2 4 4 2)

PDF x=-t..t
(random_walk process_pdf 3/5 10 -10): 1024/9765625 (0.0001048576)
(random_walk process_pdf 3/5 10 -9): 0 (0)
(random_walk process_pdf 3/5 10 -8): 3072/1953125 (0.001572864)
(random_walk process_pdf 3/5 10 -7): 0 (0)
(random_walk process_pdf 3/5 10 -6): 20736/1953125 (0.010616832)
(random_walk process_pdf 3/5 10 -5): 0 (0)
(random_walk process_pdf 3/5 10 -4): 82944/1953125 (0.042467328)
(random_walk process_pdf 3/5 10 -3): 0 (0)
(random_walk process_pdf 3/5 10 -2): 217728/1953125 (0.111476736)
(random_walk process_pdf 3/5 10 -1): 0 (0)
(random_walk process_pdf 3/5 10 0): 1959552/9765625 (0.2006581248)
(random_walk process_pdf 3/5 10 1): 0 (0)
(random_walk process_pdf 3/5 10 2): 489888/1953125 (0.250822656)
(random_walk process_pdf 3/5 10 3): 0 (0)
(random_walk process_pdf 3/5 10 4): 419904/1953125 (0.214990848)
(random_walk process_pdf 3/5 10 5): 0 (0)
(random_walk process_pdf 3/5 10 6): 236196/1953125 (0.120932352)
(random_walk process_pdf 3/5 10 7): 0 (0)
(random_walk process_pdf 3/5 10 8): 78732/1953125 (0.040310784)
(random_walk process_pdf 3/5 10 9): 0 (0)
(random_walk process_pdf 3/5 10 10): 59049/9765625 (0.0060466176)

PDF2 t=0.., x constant
(random_walk process_pdf 3/5 0 4): 0 (0)
(random_walk process_pdf 3/5 1 4): 0 (0)
(random_walk process_pdf 3/5 2 4): 0 (0)
(random_walk process_pdf 3/5 3 4): 0 (0)
(random_walk process_pdf 3/5 4 4): 81/625 (0.1296)
(random_walk process_pdf 3/5 5 4): 0 (0)
(random_walk process_pdf 3/5 6 4): 2916/15625 (0.186624)
(random_walk process_pdf 3/5 7 4): 0 (0)
(random_walk process_pdf 3/5 8 4): 81648/390625 (0.20901888)
(random_walk process_pdf 3/5 9 4): 0 (0)
(random_walk process_pdf 3/5 10 4): 419904/1953125 (0.214990848)
(random_walk process_pdf 3/5 11 4): 0 (0)
(random_walk process_pdf 3/5 12 4): 10392624/48828125 (0.21284093952)
(random_walk process_pdf 3/5 13 4): 0 (0)
(random_walk process_pdf 3/5 14 4): 1260971712/6103515625 (0.20659760529408)
(random_walk process_pdf 3/5 15 4): 0 (0)
(random_walk process_pdf 3/5 16 4): 30263321088/152587890625 (0.1983337010823168)
(random_walk process_pdf 3/5 17 4): 0 (0)
(random_walk process_pdf 3/5 18 4): 721603344384/3814697265625 (0.1891639871101993)
(random_walk process_pdf 3/5 19 4): 0 (0)
(random_walk process_pdf 3/5 20 4): 3427615885824/19073486328125 (0.17970578775468934)

CDF
(random_walk process_cdf 3/5 10 -10): 0.00010485760000000021
(random_walk process_cdf 3/5 10 -9): 0.00010485760000000021
(random_walk process_cdf 3/5 10 -8): 0.0016777216000000016
(random_walk process_cdf 3/5 10 -7): 0.0016777216000000016
(random_walk process_cdf 3/5 10 -6): 0.012294553600000008
(random_walk process_cdf 3/5 10 -5): 0.012294553600000008
(random_walk process_cdf 3/5 10 -4): 0.054761881600000076
(random_walk process_cdf 3/5 10 -3): 0.054761881600000076
(random_walk process_cdf 3/5 10 -2): 0.16623861760000017
(random_walk process_cdf 3/5 10 -1): 0.16623861760000017
(random_walk process_cdf 3/5 10 0): 0.3668967424
(random_walk process_cdf 3/5 10 1): 0.3668967424
(random_walk process_cdf 3/5 10 2): 0.6177193984000003
(random_walk process_cdf 3/5 10 3): 0.6177193984000003
(random_walk process_cdf 3/5 10 4): 0.8327102463999991
(random_walk process_cdf 3/5 10 5): 0.8327102463999991
(random_walk process_cdf 3/5 10 6): 0.9536425984
(random_walk process_cdf 3/5 10 7): 0.9536425984
(random_walk process_cdf 3/5 10 8): 0.9939533824000005
(random_walk process_cdf 3/5 10 9): 0.9939533824000005
(random_walk process_cdf 3/5 10 10): 1

Quantile
(random_walk_process_quantile 3/5 10 0.0001): -10
(random_walk_process_quantile 3/5 10 0.001): -8
(random_walk_process_quantile 3/5 10 0.01): -6
(random_walk_process_quantile 3/5 10 0.05): -4
(random_walk_process_quantile 3/5 10 0.25): 0
(random_walk_process_quantile 3/5 10 0.5): 2
(random_walk_process_quantile 3/5 10 0.75): 4
(random_walk_process_quantile 3/5 10 0.9): 6
(random_walk_process_quantile 3/5 10 0.95): 6
(random_walk_process_quantile 3/5 10 0.99): 8
(random_walk_process_quantile 3/5 10 0.999): 10
(random_walk_process_quantile 3/5 10 0.9999): 10
(random_walk_process_quantile 3/5 10 0.99999): 10
(random_walk_process_quantile 3/5 10 0.99999999): 10

mean: 2
variance: 48/5

|#
(displayln "Testing random_walk_process")
(let ([p 6/10]
      [t 10]
      [x 4])
  (displayln (format "random_walk process p:~a t:~a x:~a" p t x))
  (newline)
  (show "dist" (for/list ([i 10]) (random_walk_process_dist p t)))
  (newline)
  (displayln "PDF x=-t..t")
  (for ([n (range (- t) (add1 t))])
    (let ([v (random_walk_process_pdf p t n)])
      (displayln (format "(random_walk process_pdf ~a ~a ~a): ~a (~a)" p t n v (* 1.0 v) ))
      )
    )
  (newline)
  (displayln "PDF2 t=0.., x constant")
  (for ([tt (range 0 (add1 (* 2 t)))])
    (let ([v (random_walk_process_pdf p tt x)])
      (displayln (format "(random_walk process_pdf ~a ~a ~a): ~a (~a)" p tt x v (* 1.0 v) ))
      )
    )
  (newline)
  (displayln "CDF")
  (for ([n (range (- t) (add1 t))])
    (displayln (format "(random_walk process_cdf ~a ~a ~a): ~a" p t n (random_walk_process_cdf p t n) ))
    )
  (newline)

  (displayln "Quantile")
  (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.99999999))
  (for ([q ps])
    (displayln (format "(random_walk_process_quantile ~a ~a ~a): ~a" p t q (random_walk_process_quantile p t q) ))
    )
  (newline)
  
  (show "mean" (random_walk_process_mean p t))
  (show "variance" (random_walk_process_variance p t))
  
  )

(newline)

#|
  Binimial process

binomial process p:3/5 t:10 x:4

dist: (6 7 6 6 7 7 6 6 9 6)

PDF x=0..t
(binomial process_pdf 3/5 10 0): 1024/9765625 (0.0001048576)
(binomial process_pdf 3/5 10 1): 3072/1953125 (0.001572864)
(binomial process_pdf 3/5 10 2): 20736/1953125 (0.010616832)
(binomial process_pdf 3/5 10 3): 82944/1953125 (0.042467328)
(binomial process_pdf 3/5 10 4): 217728/1953125 (0.111476736)
(binomial process_pdf 3/5 10 5): 1959552/9765625 (0.2006581248)
(binomial process_pdf 3/5 10 6): 489888/1953125 (0.250822656)
(binomial process_pdf 3/5 10 7): 419904/1953125 (0.214990848)
(binomial process_pdf 3/5 10 8): 236196/1953125 (0.120932352)
(binomial process_pdf 3/5 10 9): 78732/1953125 (0.040310784)
(binomial process_pdf 3/5 10 10): 59049/9765625 (0.0060466176)

PDF2 t=0.., x constant
(binomial process_pdf 3/5 0 4): 0 (0)
(binomial process_pdf 3/5 1 4): 0 (0)
(binomial process_pdf 3/5 2 4): 0 (0)
(binomial process_pdf 3/5 3 4): 0 (0)
(binomial process_pdf 3/5 4 4): 81/625 (0.1296)
(binomial process_pdf 3/5 5 4): 162/625 (0.2592)
(binomial process_pdf 3/5 6 4): 972/3125 (0.31104)
(binomial process_pdf 3/5 7 4): 4536/15625 (0.290304)
(binomial process_pdf 3/5 8 4): 18144/78125 (0.2322432)
(binomial process_pdf 3/5 9 4): 326592/1953125 (0.167215104)
(binomial process_pdf 3/5 10 4): 217728/1953125 (0.111476736)
(binomial process_pdf 3/5 11 4): 684288/9765625 (0.0700710912)
(binomial process_pdf 3/5 12 4): 2052864/48828125 (0.04204265472)
(binomial process_pdf 3/5 13 4): 5930496/244140625 (0.024291311616)
(binomial process_pdf 3/5 14 4): 83026944/6103515625 (0.01360313450496)
(binomial process_pdf 3/5 15 4): 45287424/6103515625 (0.00741989154816)
(binomial process_pdf 3/5 16 4): 120766464/30517578125 (0.003957275492352)
(binomial process_pdf 3/5 17 4): 315850752/152587890625 (0.0020699594883072)
(binomial process_pdf 3/5 18 4): 812187648/762939453125 (0.00106455059398656)
(binomial process_pdf 3/5 19 4): 10287710208/19073486328125 (0.0005393723009531904)
(binomial process_pdf 3/5 20 4): 5143855104/19073486328125 (0.0002696861504765952)

CDF
(binomial process_cdf 3/5 10 0): 0.00010485760000000021
(binomial process_cdf 3/5 10 1): 0.0016777216000000016
(binomial process_cdf 3/5 10 2): 0.012294553600000008
(binomial process_cdf 3/5 10 3): 0.054761881600000076
(binomial process_cdf 3/5 10 4): 0.16623861760000017
(binomial process_cdf 3/5 10 5): 0.3668967424
(binomial process_cdf 3/5 10 6): 0.6177193984000003
(binomial process_cdf 3/5 10 7): 0.8327102463999991
(binomial process_cdf 3/5 10 8): 0.9536425984
(binomial process_cdf 3/5 10 9): 0.9939533824000005
(binomial process_cdf 3/5 10 10): 1

Quantile
(binomial_process_quantile 3/5 10 0.0001): 0
(binomial_process_quantile 3/5 10 0.001): 1
(binomial_process_quantile 3/5 10 0.01): 2
(binomial_process_quantile 3/5 10 0.05): 3
(binomial_process_quantile 3/5 10 0.25): 5
(binomial_process_quantile 3/5 10 0.5): 6
(binomial_process_quantile 3/5 10 0.75): 7
(binomial_process_quantile 3/5 10 0.9): 8
(binomial_process_quantile 3/5 10 0.95): 8
(binomial_process_quantile 3/5 10 0.99): 9
(binomial_process_quantile 3/5 10 0.999): 10
(binomial_process_quantile 3/5 10 0.9999): 10
(binomial_process_quantile 3/5 10 0.99999): 10
(binomial_process_quantile 3/5 10 0.99999999): 10

mean: 6
variance: 12/5

  
|#

(displayln "Testing binomial_process")
(let ([p 6/10]
      [t 10]
      [x 4])
  (displayln (format "binomial process p:~a t:~a x:~a" p t x))
  (newline)
  (show "dist" (for/list ([i 10]) (binomial_process_dist p t)))
  (newline)
  (displayln "PDF x=0..t")
  (for ([n (range 0 (add1 t))])
    (let ([v (binomial_process_pdf p t n)])
      (displayln (format "(binomial process_pdf ~a ~a ~a): ~a (~a)" p t n v (* 1.0 v) ))
      )
    )
  (newline)
  (displayln "PDF2 t=0.., x constant")
  (for ([tt (range 0 (add1 (* 2 t)))])
    (let ([v (binomial_process_pdf p tt x)])
      (displayln (format "(binomial process_pdf ~a ~a ~a): ~a (~a)" p tt x v (* 1.0 v) ))
      )
    )
  (newline)
  (displayln "CDF")
  (for ([n (range 0 (add1 t))])
    (displayln (format "(binomial process_cdf ~a ~a ~a): ~a" p t n (binomial_process_cdf p t n) ))
    )
  (newline)

  (displayln "Quantile")
  (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.99999999))
  (for ([q ps])
    (displayln (format "(binomial_process_quantile ~a ~a ~a): ~a" p t q (binomial_process_quantile p t q) ))
    )
  (newline)
  
  (show "mean" (binomial_process_mean p t))
  (show "variance" (binomial_process_variance p t))
  
  )

(newline)

#|
  probability-of-run-size

|#
(displayln "Testing probability-of-run-size")
(let ([n 10]
      [p 1/2])
  (displayln (format "probability-of-run-size n:~a p:~a" n p))
  (for ([x (range 11)])
    (displayln (format "probability-of-run-size(~a ~a ~a): ~a " n p x (probability-of-run-size n p x)))
    ))

(newline)  
      
#|

  prob-n-heads-after-k-in-max-m-tosses

(p 1/2 m 21 n 3)
Dist: (6 3 3 21 11 6 18 20 3 12)
Dist mean (10000 samples): 11.8266
Theoretical Mean: 14
Histogram:
 3: 1260 ##################################################### (0.126 / 0    )
 4:  614 ########################## (0.061 / 0.126)
 5:  609 ########################## (0.060 / 0.187)
 6:  662 ############################ (0.066 / 0.248)
 7:  558 ######################## (0.055 / 0.314)
 8:  506 ###################### (0.050 / 0.370)
 9:  474 #################### (0.047 / 0.420)
10:  421 ################## (0.042 / 0.468)
11:  397 ################# (0.039 / 0.510)
12:  366 ################ (0.036 / 0.550)
13:  309 ############# (0.030 / 0.586)
14:  308 ############# (0.030 / 0.617)
15:  277 ############ (0.027 / 0.648)
16:  255 ########### (0.025 / 0.676)
17:  256 ########### (0.025 / 0.701)
18:  240 ########## (0.024 / 0.727)
19:  214 ######### (0.021 / 0.751)
20:  169 ######## (0.016 / 0.772)
21:  178 ######## (0.017 / 0.789)
22: 1927 ################################################################################ (0.192 / 0.807)

PDF
(0 0 0)
(1 0 0)
(2 0 0)
(3 1/8 0.125)
(4 1/16 0.0625)
(5 1/16 0.0625)
(6 1/16 0.0625)
(7 7/128 0.0546875)
(8 13/256 0.05078125)
(9 3/64 0.046875)
(10 11/256 0.04296875)
(11 81/2048 0.03955078125)
(12 149/4096 0.036376953125)
(13 137/4096 0.033447265625)
(14 63/2048 0.03076171875)
(15 927/32768 0.028289794921875)
(16 1705/65536 0.0260162353515625)
(17 49/2048 0.02392578125)
(18 721/32768 0.022003173828125)
(19 10609/524288 0.020235061645507813)
(20 19513/1048576 0.018609046936035156)
(21 17945/1048576 0.017113685607910156)
(22 51343/262144 0.19585800170898438)
sum: 1

CDF
(0 0 0)
(1 0 0)
(2 0 0)
(3 1/8 0.125)
(4 3/16 0.1875)
(5 1/4 0.25)
(6 5/16 0.3125)
(7 47/128 0.3671875)
(8 107/256 0.41796875)
(9 119/256 0.46484375)
(10 65/128 0.5078125)
(11 1121/2048 0.54736328125)
(12 2391/4096 0.583740234375)
(13 79/128 0.6171875)
(14 1327/2048 0.64794921875)
(15 22159/32768 0.676239013671875)
(16 46023/65536 0.7022552490234375)
(17 47591/65536 0.7261810302734375)
(18 49033/65536 0.7481842041015625)
(19 402873/524288 0.7684192657470703)
(20 825259/1048576 0.7870283126831055)
(21 210801/262144 0.8041419982910156)
(22 1 1.0)

Quantile
0.0001: 3
0.001: 3
0.01: 3
0.05: 3
0.25: 5
0.3: 6
0.4: 8
0.5: 10
0.75: 19
0.8: 21
0.84: 22
0.9: 22
0.95: 22
0.99: 22
0.999: 22
0.9999: 22
0.99999: 22
0.999999: 22

|#
(displayln "Testing prob-n-heads-after-k-in-max-m-tosses")
(let ([p 1/2]
      [m 21]
      ; [m 280]      
      [n 3])
  (show2 "p" p "m" m "n" n)
  (show "Dist" (for/list ([i 10]) (prob-n-heads-after-k-in-max-m-tosses-dist p m n)))
  (show "Dist mean (10000 samples)" (* 1.0 (avg (for/list ([i 10000]) (prob-n-heads-after-k-in-max-m-tosses-dist p m n)))))
  (show "Theoretical Mean" (expected-tosses-needed-for-n-heads n))  
  (displayln "Histogram:")
  (show-histogram (for/list ([i 10000]) (prob-n-heads-after-k-in-max-m-tosses-dist p m n)))
  (newline)
  (displayln "PDF")
  (show "sum" (sum
               (for/list ([k (range 0 (+ 2 m))])
                 (let ([v (prob-n-heads-after-k-in-max-m-tosses-pdf p m n k)])
                   (if (> m 30) 
                       (show2 k (* 1.0 v) )
                       (show2 k v (* 1.0 v) ))
                   v
                   ))))
  (newline)
  (flush-output)
  (displayln "CDF")
  (for ([k (range 0 (+ 2 m))])
    (let ([v (prob-n-heads-after-k-in-max-m-tosses-cdf p m n k)])
      (if (> m 30) 
          (show2 k (* 1.0 v) )
          (show2 k v (* 1.0 v) ))
      
      ))
  (newline)
  (flush-output)
  (displayln "Quantile")
  (for ([q '(0.0001 0.001 0.01 0.05 0.25 0.3 0.4 0.5 0.75 0.80 0.84 0.9 0.95 0.99 0.999 0.9999 0.99999 0.999999)])
    (show q (prob-n-heads-after-k-in-max-m-tosses-quantile p m n q))
    (flush-output)
    )

  (flush-output)
  )

(newline)


#|
  Expected number of "tosses" to get n successes

  (expected-tosses-needed-for-n-successes 10 1/2): 2046
  (expected-tosses-needed-for-n-successes 10 1/6): 72559410
  (expected-tosses-needed-for-n-successes 100 1/2): 2535301200456458802993406410750
  (expected-tosses-needed-for-n-successes 100 1/6): 783982348200085087316028320589669384644572452567545845851686359643396569772850
  (expected-tosses-needed-for-n-successes 1000 1/2): 21430172143725346418968500981200036211228096234110672148875007767407021022498722449863967576313917162551893458351062936503742905713846280871969155149397149607869135549648461970842149210124742283755908364306092949967163882534797535118331087892154125829142392955373084335320859663305248773674411336138750

|#
(displayln "Expected number of 'tosses' to get n successes: expected-tosses-needed-for-n-successes")
(displayln (format "(expected-tosses-needed-for-n-successes 10 1/2): ~a" (expected-tosses-needed-for-n-successes 10 1/2)))
(displayln (format "(expected-tosses-needed-for-n-successes 10 1/6): ~a" (expected-tosses-needed-for-n-successes 10 1/6)))
(displayln (format "(expected-tosses-needed-for-n-successes 100 1/2): ~a" (expected-tosses-needed-for-n-successes 100 1/2)))
(displayln (format "compare: (expected-tosses-needed-for-n-heads 100): ~a" (expected-tosses-needed-for-n-heads 100)))
(displayln (format "(expected-tosses-needed-for-n-successes 100 1/6): ~a" (expected-tosses-needed-for-n-successes 100 1/6)))
(displayln (format "(expected-tosses-needed-for-n-successes 1000 1/2): ~a" (expected-tosses-needed-for-n-successes 1000 1/2)))
(newline)


#|
  Wiener process

wiener process mu:1 sigma:2 t:3

dist: (1.537098688003291 1.0652119287563047 0.37344338121089127 5.7459530403224 0.403046874581539)

PDF
(wiener process_pdf 1 2 3 -10): 0.00010073084243312548
(wiener process_pdf 1 2 3 -8): 0.0007443058456309081
(wiener process_pdf 1 2 3 -6): 0.0039407198941995274
(wiener process_pdf 1 2 3 -4): 0.01494978254456404
(wiener process_pdf 1 2 3 -2): 0.04063772223030267
(wiener process_pdf 1 2 3 0): 0.07915147493888483
(wiener process_pdf 1 2 3 2): 0.11046478188859792
(wiener process_pdf 1 2 3 4): 0.11046478188859792
(wiener process_pdf 1 2 3 6): 0.07915147493888483
(wiener process_pdf 1 2 3 8): 0.04063772223030267
(wiener process_pdf 1 2 3 10): 0.01494978254456404

CDF
(wiener process_cdf 1 2 3 -10): 8.744329627104659e-5
(wiener process_cdf 1 2 3 -8): 0.0007480821448727764
(wiener process_cdf 1 2 3 -6): 0.004687384229717447
(wiener process_cdf 1 2 3 -4): 0.021654071405395988
(wiener process_cdf 1 2 3 -2): 0.07445733658938289
(wiener process_cdf 1 2 3 0): 0.19323811538561636
(wiener process_cdf 1 2 3 2): 0.3864149963422237
(wiener process_cdf 1 2 3 4): 0.6135850036577764
(wiener process_cdf 1 2 3 6): 0.8067618846143836
(wiener process_cdf 1 2 3 8): 0.9255426634106171
(wiener process_cdf 1 2 3 10): 0.978345928594604

Quantile
(wiener_process_quantile 1 2 3 1e-7): -15.011033716120565
(wiener_process_quantile 1 2 3 1e-5): -11.774015087614199
(wiener_process_quantile 1 2 3 0.0001): -9.883051013990956
(wiener_process_quantile 1 2 3 0.001): -7.704878722946789
(wiener_process_quantile 1 2 3 0.01): -5.0587054278371575
(wiener_process_quantile 1 2 3 0.05): -2.69794010578779
(wiener_process_quantile 1 2 3 0.25): 0.6634989669518929
(wiener_process_quantile 1 2 3 0.5): 3.0
(wiener_process_quantile 1 2 3 0.75): 5.336501033048107
(wiener_process_quantile 1 2 3 0.9): 7.439424848085369
(wiener_process_quantile 1 2 3 0.95): 8.697940105787787
(wiener_process_quantile 1 2 3 0.99): 11.058705427837157
(wiener_process_quantile 1 2 3 0.999): 13.70487872294679
(wiener_process_quantile 1 2 3 0.9999): 15.883051013991054
(wiener_process_quantile 1 2 3 0.99999): 17.774015087617713
(wiener_process_quantile 1 2 3 0.99999999): 22.44054257108969

mean: 3
variance: 12

|#
(displayln "Testing wiener_process")
(let ([mu 1]
      [sigma 2]
      [t 3])
  (displayln (format "wiener process mu:~a sigma:~a t:~a" mu sigma t))
  (newline)
  (show "dist" (for/list ([i 5]) (wiener_process_dist mu sigma t)))
  (newline)
  (displayln "PDF")
  (for ([x (range -10 11 2)])
    (let ([v (wiener_process_pdf mu sigma t x)])
      (displayln (format "(wiener process_pdf ~a ~a ~a ~a): ~a" mu sigma t x v))
      )
    )
  (newline)
  (displayln "CDF")
  (for ([x (range -10 11 2)])
    (displayln (format "(wiener process_cdf ~a ~a ~a ~a): ~a" mu sigma t x (wiener_process_cdf mu sigma t x) ))
    )
  (newline)

  (displayln "Quantile")
  (define ps '(0.0000001 0.00001 0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.99999999))
  (for ([q ps])
    (displayln (format "(wiener_process_quantile ~a ~a ~a ~a): ~a" mu sigma t q (wiener_process_quantile mu sigma t q) ))
    )
  (newline)
  
  (show "mean" (wiener_process_mean mu sigma t))
  (show "variance" (wiener_process_variance mu sigma t))
  
  )

(newline)


#|
  Discrete Markov Process

Transition-matrix
(0.1 0.6 0.3)
(0.4 0.4 0.2)
(0.5 0.2 0.3)
initial-state-probabilities: (1 0 0)

discrete_markov_process_stationary
(0.3275862068794929 0.4137931034613079 0.2586206896591996)
(0.3275862068794929 0.4137931034613079 0.2586206896591996)

Random variates:
Random: (0 1 0 1 1 0 0 2 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 2 1 1 1)
(0 : 0.4186046511627907)
(1 : 0.32558139534883723)
(2 : 0.2558139534883721)
(mean: 0.8604651162790697)

Probability of being in state 2 after t=0..10 steps
(discrete_markov_process_pdf time: t 0 state 0:  0 0)
(discrete_markov_process_pdf time: t 1 state 0:  0.6 0.6)
(discrete_markov_process_pdf time: t 2 state 0:  0.36 0.36)
(discrete_markov_process_pdf time: t 3 state 0:  0.432 0.432)
(discrete_markov_process_pdf time: t 4 state 0:  0.408 0.408)
(discrete_markov_process_pdf time: t 5 state 0:  0.41568000000000005 0.41568000000000005)
(discrete_markov_process_pdf time: t 6 state 0:  0.413184 0.413184)
(discrete_markov_process_pdf time: t 7 state 0:  0.41399040000000004 0.41399040000000004)
(discrete_markov_process_pdf time: t 8 state 0:  0.41372928000000003 0.41372928000000003)
(discrete_markov_process_pdf time: t 9 state 0:  0.41381376000000003 0.41381376000000003)

Fix t and check PDF/CDF/Quantile on the states, with t: 30

PDF
(state 0 0.3275862068965534)
(state 1 0.4137931034482752)
(state 2 0.2586206896551723)

CDF
(state 0 0.3275862068965534)
(state 1 0.7413793103448285)
(state 2 1.0000000000000009)

Quantiles
(q 0.0001 0)
(q 0.001 0)
(q 0.01 0)
(q 0.05 0)
(q 0.25 0)
(q 0.5 1)
(q 0.75 2)
(q 0.9 2)
(q 0.95 2)
(q 0.99 2)
(q 0.999 2)
(q 0.9999 2)
(q 0.99999 2)


|#
(displayln "Testing discrete_markov_process")

(let ([tmp 2])
  
  (define transition-matrix
    '((0.1 0.6 0.3)
      (0.4 0.4 0.2)
      (0.5 0.2 0.3)))    
    
  (when (<= (length transition-matrix) 10)
    (display-matrix transition-matrix "Transition-matrix" )
    )
  
  (markov-check-matrix transition-matrix)
  
  ;; Initial state probabilities (assuming starting from state 0)
  (define initial-state-probabilities '(1 0 0))
  
  (show "initial-state-probabilities" initial-state-probabilities)
  (newline)
  
  (displayln "discrete_markov_process_stationary")
  (displayln (discrete_markov_process_stationary transition-matrix #:tolerance 1e-10))
  (displayln (discrete_markov_process_stationary2 transition-matrix #:tolerance 1e-10))
  (newline)
  (displayln "Random variates:")
  (displayln (format "Random: ~a" (for/list ([i 30]) (discrete_markov_process_dist transition-matrix initial-state-probabilities 4))))
  (show-freq (for/list ([i 40]) (discrete_markov_process_dist transition-matrix initial-state-probabilities 4)))
  (newline)  
  (let ([t 30])
    (newline)
    (displayln "Probability of being in state 2 after t=0..10 steps")
    (for/list ([t (range 10)])
      (let ([v (discrete_markov_process_pdf transition-matrix initial-state-probabilities t 1)])
        (displayln (list "discrete_markov_process_pdf" "time: t" t "state 0: " v (* 1.0 v) ))
        )
      )
    (newline)
    (show "Fix t and check PDF/CDF/Quantile on the states, with t" t)
    (newline)
    (displayln "PDF")
    (for ([state (length transition-matrix)])
      (show2 "state" state (discrete_markov_process_pdf transition-matrix initial-state-probabilities t state)))   
    (newline)
    (displayln "CDF")
    (for ([state (length transition-matrix)])
      (show2 "state" state (discrete_markov_process_cdf transition-matrix initial-state-probabilities t state)))
    (newline)
    (displayln "Quantiles")
    (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999))  
    (for ([q ps])  
      (displayln (list "q" q (discrete_markov_process_quantile transition-matrix initial-state-probabilities t q)))
      )   
    (newline)
    )
  )
(newline)

#|
  Sum probability

sum_prob a:1 b:6 n:4 (s:12)
Dist: (13 14 11 12 16)
count-partitions-sum: 125

PDF
(4 1/1296)
(5 1/324)
(6 5/648)
(7 5/324)
(8 35/1296)
(9 7/162)
(10 5/81)
(11 13/162)
(12 125/1296)
(13 35/324)
(14 73/648)
(15 35/324)
(16 125/1296)
(17 13/162)
(18 5/81)
(19 7/162)
(20 35/1296)
(21 5/324)
(22 5/648)
(23 1/324)
(24 1/1296)

CDF
(4 1/1296)
(5 5/1296)
(6 5/432)
(7 35/1296)
(8 35/648)
(9 7/72)
(10 103/648)
(11 155/648)
(12 145/432)
(13 575/1296)
(14 721/1296)
(15 287/432)
(16 493/648)
(17 545/648)
(18 65/72)
(19 613/648)
(20 1261/1296)
(21 427/432)
(22 1291/1296)
(23 1295/1296)
(24 1)
Quantiles
(0.0001 4)
(0.001 5)
(0.01 6)
(0.05 8)
(0.25 12)
(0.5 14)
(0.75 16)
(0.9 18)
(0.95 20)
(0.99 22)
(0.999 23)
(0.9999 24)
(0.99999 24)

mean: 14


|#
(displayln "Testing sum_prob")
; Probability (etc) of throwing n dice and getting the sum s
(let ([a 1]
      [b 6]
      [n 4]
      [s 12])
  (displayln (format "sum_prob a:~a b:~a n:~a (s:~a)" a b n s))
  (show "Dist" (for/list ([i 5]) (sum_prob_dist a b n)))
  (show "count-partitions-sum" (count-partitions-sum a b n s))
  (newline)
  (displayln "PDF")
  (for ([ss (range (* a n) (add1 (* b n)))])
    (show2 ss (sum_prob_pdf a b n ss)))
  (newline)
  (displayln "CDF")
  (for ([ss (range (* a n) (add1 (* b n)))])
    (show2 ss (sum_prob_cdf a b n ss))
    )
  (displayln "Quantiles")
  (for ([q '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999)])
    (show2 q (sum_prob_quantile a b n q))
    )
  (newline)
  (show "mean" (sum_prob_mean a b n))
  (newline)
)

