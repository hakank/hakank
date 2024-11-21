#| 

  Poisson process in Racket/Gamble 

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes",
  Chapter "14: The Poisson process" 
  and 
  Mathematica PoissonProcess


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")



#|
  From Mathematica PoissonProcess
  """
  Customers arrive at a store according to a Poisson rate of four per hour. 
  Given that the store opens at 9am, find the probability that exactly one 
  customer has arrived by 9:30am:
    arrivalProcess = PoissonProcess[4];

  The probability of exactly one arrival by 9:30am:
    Probability[x[1/2] == 1, x \[Distributed] arrivalProcess]
  -> 
   2/exp(2)

    N[%] 
  -> 
   0.270671
  """

  (poisson_process_pdf 4 1/2 1): 0.2706705664732254

  * mu = 1/2 (as in the problem statement)

  variable : x
  1: 0.27077000000000007
  2: 0.26970000000000005
  3: 0.18060000000000004
  0: 0.13507000000000002
  4: 0.09166000000000002
  5: 0.03590000000000001
  6: 0.011330000000000002
  7: 0.003620000000000001
  8: 0.0010600000000000002
  9: 0.00023000000000000006
  10: 5.0000000000000016e-5
  11: 1.0000000000000003e-5
  mean: 2.00259
  HPD interval (0.5): 1..2
  HPD interval (0.84): 0..3
  HPD interval (0.9): 0..4
  HPD interval (0.99): 0..6
  HPD interval (0.99999): 0..10

  variable : p
  #f: 0.72923
  #t: 0.27077000000000007
  mean: 0.27077000000000007

|#
(define (model)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define x (poisson_process_dist 4 1/2))
   ; (define x (poisson_process_dist 4 2))   
   (define p (= x 1))
   
   (list x
         p
         )

   )
)

(displayln "\nModel 1")
(show "(poisson_process_pdf 4 1/2 1)" (poisson_process_pdf 4 1/2 1))
(newline)
      
(show-marginals (model)
                (list  "x"
                       "p"
                     )
                    #:num-samples 100000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.5 0.84 0.9 0.99 0.99999)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


(displayln "\nModel2")

#|

  Simulating the process.

  Solving the same problem as in model1, but since this approach
  uses indices in a list we have to scale it (since there's no 1/2 index).

  Thus we scale
    (poisson_process_pdf 4 1/2 1) -> 0.2706705664732254
  to
    (poisson_process_pdf 2 1 1) -> 0.2706705664732254

  The model uses enumerate with #:limit 1e-10 so it's not identical to the theoretical results.
  but quite close: 0.2706705664931979 (difference: -1.9972468123796716e-11).
 
  variable : T 1
  1: 0.2706705664931979
  2: 0.2706705664931979
  3: 0.18044704432814884
  0: 0.13533528324532298
  4: 0.0902235221609401
  5: 0.036089408861327194
  6: 0.012029802950494146
  7: 0.0034370865525941417
  8: 0.0008592716352097556
  9: 0.00019094924789085077
  10: 3.818984610577463e-5
  11: 6.943604226899458e-6
  12: 1.1572633767353907e-6
  13: 1.780363397164777e-7
  14: 2.5429718968454405e-8
  15: 3.3868724650030556e-9
  16: 4.1872303031424993e-10
  17: 4.630067167121692e-11
  mean: 1.9999999993445796
  HPD interval (0.5): 1..2
  HPD interval (0.84): 0..3
  HPD interval (0.9): 0..4
  HPD interval (0.99): 0..6
  HPD interval (0.99999): 0..10

  variable : T 2
  5: 0.12698842964021265
  6: 0.12373231605985054
  4: 0.1221042592696695
  7: 0.1079169072400208
  3: 0.09768340741573557
  8: 0.08884538483941432
  2: 0.0732625555618017
  9: 0.06770648873735015
  10: 0.049079451682921056
  1: 0.03663127778090085
  11: 0.03354407802781625
  12: 0.02195049689813674
  0: 0.01831563889045042
  13: 0.01369825429939186
  14: 0.00822846401224004
  15: 0.0047499963466809195
  16: 0.0026508655440550034
  17: 0.0014295127318680045
  18: 0.0007479159784833257
  19: 0.0003796780453784123
  20: 0.0001875509971588597
  21: 9.018162578657483e-5
  22  : 4.229850758134967e-5
  23: 1.936189048569316e-5
  24: 8.663239548290756e-6
  25: 3.7909589604919936e-6
  26: 1.624416473298591e-6
  27: 6.819483902376058e-7
  28: 2.8076769074426157e-7
  29: 1.1342256654617375e-7
  30: 4.499398199818285e-8
  31: 1.7535592617314017e-8
  32: 6.717555915081762e-9
  33: 2.530685506934913e-9
  34: 9.363113134395298e-10
  35: 3.3932538631098157e-10
  36: 1.1943384284936534e-10
  37: 3.8775632478228926e-11
  38: 1.0330923755703762e-11
  39: 9.743236476187663e-13
  mean: 5.999999998033737
  HPD interval (0.5): 2..6
  HPD interval (0.84): 1..9
  HPD interval (0.9): 0..10
  HPD interval (0.99): 0..15
  HPD interval (0.99999): 0..23

  variable : T 3
  11: 0.07617956974106926
  10: 0.07551214674029609
  12: 0.0740667100532018
  9: 0.07239879903251645
  13: 0.06968650611634286
  8: 0.06622596292351729
  14: 0.06376551934942204
  7: 0.057570002940643114
  15: 0.05671182156556175
  16: 0.049162797326679296
  6: 0.04814287561384016
  17: 0.041633293941890456
  5: 0.0370160325085819
  18: 0.03445492814344677
  19: 0.02791683242181742
  4: 0.02644002322041565
  20: 0.022172657053255544
  3: 0.01817751596403576
  21: 0.017273438697657507
  22: 0.013215386369799456
  23: 0.009937411732280643
  2: 0.009915008707655867
  24: 0.007349041713483945
  25: 0.005349601942703192
  1: 0.004957504353827934
  26: 0.003835378504788489
  27: 0.002709756112867012
  0: 0.002478752176913967
  28: 0.0018878084962071322
  29: 0.0012974797401353628
  30: 0.0008801576710585917
  31: 0.0005895833955429047
  32: 0.00039014612183615376
  33: 0.00025513854047885126
  34: 0.00016495182167707752
  35: 0.00010546670057615724
  36: 6.671033026733593e-5
  37: 4.175671193217232e-5
  38: 2.587249720081504e-5
  39: 1.5872659656375012e-5
  40: 9.644387935337241e-6
  41: 5.80522799619399e-6
  42: 3.4624734651651567e-6
  43: 2.04679332241745e-6
  44: 1.1994262579773479e-6
  45: 6.969070880513759e-7
  46: 4.0156947411938327e-7
  47: 2.2951660766013328e-7
  48: 1.3013833217520216e-7
  49: 7.321707104155972e-8
  50: 4.0878946279096987e-8
  51: 2.2655321144020046e-8
  52: 1.2461484887611103e-8
  53: 6.805800654473725e-9
  54: 3.689166942133886e-9
  55: 1.986612536586412e-9
  56: 1.0605933680559492e-9
  57: 5.62592406512081e-10
  58: 2.95231750238722e-10
  59: 1.5183903387062527e-10
  60: 7.796707479597188e-11
  61: 3.77073640039969e-11
  62: 1.6998440585709525e-11
  63: 6.5692096516433484e-12
  64: 5.258254606196528e-13
  mean: 11.999999996067478
  HPD interval (0.5): 5..12
  HPD interval (0.84): 4..18
  HPD interval (0.9): 2..19
  HPD interval (0.99): 0..26
  HPD interval (0.99999): 0..41


|#
(define (model2)
  (enumerate #:limit 1e-10
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define mu 2) ; (scaled version of mu=4, t=1/2 -> mu=2 t=1
   (define n 3) ; we only need 1 element for T=1, but let's see T=1..3

   (define x (for/list ([i n]) (poisson mu)))
   (define xs (cons 0 (accum x)))
   
   (define (T xs t) (sum (take xs (add1 t))))
   (define (X xs n) (if (= n 0) 0 (- (T xs n) (T xs (sub1 n)))))
   
   
   (list (T xs 1)
         (T xs 2)
         (T xs 3)
         )

   )
)

(show "(poisson_process_pdf 2 1 1)" (poisson_process_pdf 2 1 1))

(show-marginals (model2)
                (list "T 1"
                      "T 2"
                      "T 3"
                     )
                    #:num-samples 100000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.5 0.84 0.9 0.99 0.99999)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


