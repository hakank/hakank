#| 

  Wiener process in Racket/Gamble 

  From Mathematica WienerProcess
  """
  The state at time t follows NormalDistribution[Mu t,Sigma Sqrt[t]]
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#|
  Generate some random weiner_process_dist

  mean : 3

  variable : v
  7.115056425881436: 0.00010000000000000938
  4.305085817182464: 0.00010000000000000938
  4.804593021300257: 0.00010000000000000938
  8.206460830886027: 0.00010000000000000938
  -0.22687341318227183: 0.00010000000000000938
  ...
  0.8762829158928827: 0.00010000000000000938
  -0.7417292776223952: 0.00010000000000000938
  7.634766548931189: 0.00010000000000000938
  6.268358357374897: 0.00010000000000000938
  3.187792428868953: 0.00010000000000000938
  mean: 2.9940350259641515
  HPD interval (0.84): -1.97665233591617..7.550150362637374
  HPD interval (0.95): -3.7316499600822617..9.665088602624525
  HPD interval (0.99): -5.97048667954355..11.789312876166889
  HPD interval (0.99999): -10.583580076471074..16.9519670046092

  variable : p
  #f: 0.9901999999999473
  #t: 0.009800000000000943
  mean: 0.009800000000000943


|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 10)
   (define mu 1)
   (define sigma 2)
   (define t-val 3)

   ; Generate n samples
   (define x (for/list ([i n]) (wiener_process_dist mu sigma t-val)))

   (define v (first x))
   (define p (>= v (wiener_process_quantile mu sigma t-val 0.99)))

   (list v
         p
         )

   )
)

(show "mean" (wiener_process_mean 1 2 3))
(newline)

(show-marginals (model)
                (list  "v"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.95 0.99 0.99999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  Simulate the process by accumulated random normal variates

  variable : v1
  1.7532098993233416: 0.00010000000000000938
  1.8500401337810697: 0.00010000000000000938
  2.6533619135368216: 0.00010000000000000938
  2.194825346155194: 0.00010000000000000938
  -1.0879185773753886: 0.00010000000000000938
  ...
  0.2942217448959098: 0.00010000000000000938
  1.0360647727500083: 0.00010000000000000938
  1.9268139509334814: 0.00010000000000000938
  -2.485967446066419: 0.00010000000000000938
  -2.134160407603288: 0.00010000000000000938
  mean: 1.0193148542571389
  HPD interval (0.5): -0.31287079867337786..2.3647842010798508
  HPD interval (0.84): -1.856547246863586..3.7648557087763304
  HPD interval (0.9): -2.2759645614530406..4.296684328499958
  HPD interval (0.95): -2.8868722068304775..4.927185075632765
  HPD interval (0.99): -4.23313455816277..6.018656790454938
  HPD interval (0.99999): -6.593117386804874..8.506721059644
  HPD interval (0.9999999): -6.593117386804874..8.506721059644

  variable : mean-v1
  1: 0.9999999999999463
  mean: 0.9999999999999463

  variable : var-1
  4: 0.9999999999999463
  mean: 3.999999999999785

  variable : t-val
  8: 0.9999999999999463
  mean: 7.99999999999957

  variable : v-tval
  11.771124115567114: 0.00010000000000000938
  18.261793140998066: 0.00010000000000000938
  8.288231997745141: 0.00010000000000000938
  15.046799908449906: 0.00010000000000000938
  14.960925986973436: 0.00010000000000000938
  ...
  11.59505343883774: 0.00010000000000000938
  10.71061144624588: 0.00010000000000000938
  6.421517370976475: 0.00010000000000000938
  3.9798130767989743: 0.00010000000000000938
  9.009016446824406: 0.00010000000000000938
  mean: 7.938835722509462
  HPD interval (0.5): 4.216406527025036..11.936813305023934
  HPD interval (0.84): -0.5588053641980353..15.27197387319416
  HPD interval (0.9): -1.536941205992417..17.078598347593587
  HPD interval (0.95): -3.179940478449978..18.89966617576603
  HPD interval (0.99): -7.131939697135797..21.99841912583219
  HPD interval (0.99999): -12.126459836159125..28.6405319610472
  HPD interval (0.9999999): -12.126459836159125..28.6405319610472

  variable : mean-tval
  8: 0.9999999999999463
  mean: 7.99999999999957

  variable : var-tval
  32: 0.9999999999999463
  mean: 31.99999999999828


|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 10)
   (define mu 1)
   (define sigma 2)
   (define t-val 8)
  
   ; Generate n time steps of normal distribution
   (define x (for/list ([t (range 1 n)]) (normal mu sigma)))
   ; accumulate to create a process
   (define xa (cons 0 (accum x)))
   
   (define v1 (list-ref xa 1))
   (define mean-1 (wiener_process_mean mu sigma 1))  
   (define var-1 (wiener_process_variance mu sigma 1))

   (define v-tval (list-ref xa t-val))
   (define mean-t-val (wiener_process_mean mu sigma t-val))
   (define var-t-val (wiener_process_variance mu sigma t-val))
   
   
   (list v1
         mean-1         
         var-1
         
         t-val
         v-tval
         mean-t-val
         var-t-val
         )

   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "v1"
                       "mean-v1"
                       "var-1"                       
                       "t-val"
                       "v-tval"
                       "mean-tval"
                       "var-tval"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.5 0.84 0.9 0.95 0.99 0.99999 0.9999999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )



