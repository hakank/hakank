#| 

  Max stable distribution in Racket/Gamble 

  From Mathematica MaxStableDistribution

  This is also called Generalized Extreme Value distribution 
  (for maximum values).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#|
  variable : d
  0.9237077434242553: 0.00010000000000000938
  1.638726338381754: 0.00010000000000000938
  7.132672518930475: 0.00010000000000000938
  22.891432148933667: 0.00010000000000000938
  37.01520894496251: 0.00010000000000000938
  ...
  1.592250429110916: 0.00010000000000000938
  1.7117222892451136: 0.00010000000000000938
  0.7540112030398294: 0.00010000000000000938
  2.999913259374588: 0.00010000000000000938
  0.5917017861337783: 0.00010000000000000938
  mean: 13.770952107201273

  variable : p
  #t: 0.6344999999999865
  #f: 0.3655000000000161
  mean: 0.6344999999999865


|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define mu 2)
   (define sigma 2)
   (define xi 0.9)

   (define d (max_stable_dist mu sigma xi))

   (define p (>= d mu))
              
   (list d
         p
         )

   )
)

(show-marginals (model)
                (list  "d"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  Recovering parameters

  For 100 values of max (normal 100 15)

  data: (54.65876955517643 64.7233996743916 52.413461985536266 45.1783660647792 50.06894268084677 49.138887972964845 58.53799586228594 58.180653778207656 47.32811541205034 50.782615041693525 52.71567432655761 53.39451101531212 48.392359036894256 49.6611858754421 39.61714480420473 57.50060135688129 55.30675794881516 56.30500732433086 46.770558144311174 50.602596650023486 52.69965941689426 46.440789714892624 43.017989804745504 54.824226422392755 58.530578932157646 42.01482069218778 48.67677016563298 52.158959822096485 56.30600172556527 57.64753871911637 38.76593846822976 54.995791418899344 52.73792785551227 55.247583975150604 56.53145032709746 53.783066990236186 56.46517633466781 50.87047220193979 51.16358362932241 44.82640757865977 53.19028862840537 53.420940725849064 56.4310958358144 50.88371063212399 58.25356043433736 47.01080258269051 40.527431786749034 48.209427891275396 52.19519679590802 51.70503267271497 45.42553401293141 50.60990492800276 56.51993871375903 53.66760461882363 50.613550305965646 44.486913954990996 56.03398426619635 58.786018654506364 55.630395114092344 54.768722186703016 47.82956887765237 54.16253298352625 41.92322170242115 52.897062270482074 48.350010194996145 51.671745882639236 55.21405019078433 51.56989349763181 57.78967941655537 47.561669418326 52.36123288506865 52.423514148442166 57.200531893406996 57.565799527593754 53.98766139836539 45.79245003357636 45.80867289584791 50.117154560989555 54.1974512031816 47.98220592863635 57.63313583628234 56.077302482018354 55.12295138155966 53.27447775327597 55.650343547893186 45.71382280968694 55.173398207051726 52.3925716640726 52.52638200420179 56.39975782562205 48.874391900051016 52.1177871178991 45.719432719834394 58.31832475042182 56.145499461902304 48.04361288738497 52.23881547695864 58.198393374398975 58.3778526294091 59.712606996485874)
(min 38.76593846822976 mean 52.11469363184476 max 64.7233996743916 stddev 4.92204620630309 variance 24.22653885698264)

  38.766:  1 #### (0.01  / 0    )
  41.362:  2 ######## (0.02  / 0.01 )
  43.957:  3 ########### (0.03  / 0.03 )
  46.553:  9 ################################# (0.09  / 0.06 )
  49.149: 13 ################################################ (0.13  / 0.15 )
  51.745: 13 ################################################ (0.13  / 0.28 )
  54.34 : 22 ################################################################################ (0.22  / 0.41 )
  56.936: 21 ############################################################################# (0.21  / 0.63 )
  59.532: 14 ################################################### (0.14  / 0.84 )
  62.128:  1 #### (0.01  / 0.98 )

  variable : mu
  mean: 52.15154357316529
  HPD interval (0.84): 50.83547432415591..53.71409910122524
  HPD interval (0.999): 49.38966170041677..55.34140235208489

  variable : sigma
  mean: 0.7682398415481579
  HPD interval (0.84): 0.01328235556435072..1.3276704624657185
  HPD interval (0.999): 0.0009710994088064594..3.769343710509942

  variable : xi
  mean: -1.068397721497805
  HPD interval (0.84): -2.034731290588181..0.327017352920866
  HPD interval (0.999): -2.9917940409605306..1.143356998175908

  variable : post
  mean: 52.055045718624484
  HPD interval (0.84): 50.25104869754913..53.831881035843274
  HPD interval (0.999): 36.40337668508917..62.28758350256348

  variable : p
  mean: 1.960681785383672e-5


  Mathematica gives:
  {mu -> 146.993, sigma -> 4.60607, xi -> -0.100792}

|#

(define data (for/list ([i 100])
               (max-list (for/list ([i 1000])
                           (normal 100 15)
                           ; (binomial 100 0.5)
                           ; (exponential 10)
                           ; (gamma 10 10)
                           ; (poisson 10)
                           ; (pareto 5 5)
                           ; (sample (t-dist 2 100 15)) ; The parameters are in different order from Mathematica's StudentTDistribution. This is slow!
                           ))))

; (define data (range 1 11))

(show "data" data)
(show2 "min" (min-list data) "mean" (avg data) "max" (max-list data) "stddev" (stddev data) "variance" (variance data))
(newline)
(show-histogram data)
(flush-output)
(newline)
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define mu (normal (avg data) 10))
   (define mu (normal (avg data) (stddev data)))
   ; (define mu (uniform (min-list data) (max-list data)))
   (define sigma (uniform 0 20))
   (define xi (uniform -3 3))

   (for ([i (length data)])
     (observe-sample (normal-dist (max_stable_dist mu sigma xi) 10) (list-ref data i)))
   
   (define post (max_stable_dist mu sigma xi))

   (define p (>= post (max-list data)))
   
   (list mu
         sigma
         xi
         post
         p
         )

   )
)

(show-marginals (model2)
                (list  "mu"
                       "sigma"
                       "xi"
                       "post"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.001 0.84 0.999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

