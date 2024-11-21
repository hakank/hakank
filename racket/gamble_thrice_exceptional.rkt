#| 

  Thrice exceptional in Racket/Gamble 

  From @cremieuxrecueil 
  https://x.com/cremieuxrecueil/status/1858655075192713685
  """
  I simulated 100,000 people to show how often people are "thrice-exceptional": 
  Smart, stable, and exceptionally hard-working.

  ...
  In short, there are not many people who are thrice-exceptional, in the sense 
  of being at least +2 standard deviations in conscientiousness, emotional 
  stability (i.e., inverse neuroticism), and intelligence.
  """

  Also see 
  Gilles E. Gignac
  "The number of exceptional people: Fewer than 85 per 1 million across key traits"
  https://www.sciencedirect.com/science/article/pii/S019188692400415X

  The criteria for exceptional is 2SD for each dimension.

  Assuming that all traits are normal distributed (with mean 0 and stddev 1), 
  i.e. a 2SD of 2:
  > (- 1 (dist-cdf (normal-dist 0 1) 2))
  0.02275013194817921

  About one in 44:
  > (/ $1)
  43.95578901598566

  For three exceptional (2sd) traits:
  > (expt (- 1 (dist-cdf (normal-dist 0 1) 2)) 3)
  1.1774751750476762e-5

  This is about one in 850 000 people
  > (/ $1)
  84927.4805270956

  It's very rate with three 3sd traits:
  > (expt (- 1 (dist-cdf (normal-dist 0 1) 3)) 3)
  2.4598175300466015e-9
  > (/ $1)
  406534219.626061

  Being thrice 1SD exceptional is quite rare (about 1 in 250)
  > (expt (- 1 (dist-cdf (normal-dist 0 1) 1)) 3)
  0.003993589074329777
  > (/ $1)
  250.40132607229367


  The simulation below assumes that the traits are are all 
  normally distributed (here with mean 0 and standard deviation 1),
  and does not correlate the traits (contrary to the research cited above,
  though the correlation seems to be small, see the discussion in the thread),
  It shows a little less probability of exceptional (2sd) people:  
  about 10 in 1_000_000.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|
  85 exceptional people of 1_000_000 is
  > (/ 85 1000000.0)
  8.5e-5

  For a run of 1 000_000 samples:12 exceptional people

  x: (2.446800796261254 2.8284966270116807 2.5306827826989946)
  x: (2.8007713300968735 2.0540415630933797 2.212631381705356)
  x: (2.263085944875865 2.198794420254132 2.0868481288323553)
  x: (2.1142727253087377 2.23628879333429 2.071827912404287)
  x: (2.131927507277522 2.1559516079018683 2.2905533957142263)
  x: (2.153554649439712 2.1511442517776542 2.1818022998177895)
  x: (2.561768810091873 2.6763556945120195 2.189715255800239)
  x: (2.1501330517303603 2.104277855415556 2.2035027600089148)
  x: (2.3593026664281975 2.005700653130788 2.126421271911848)
  x: (2.5573741604594353 2.34581462389682 2.2287239557505596)
  x: (2.3520225430938404 2.2636217646023082 2.3762748704741403)
  x: (2.070374938491301 2.0585511071679807 2.044978396125109)
  variable : p-2sd
  #f: 0.999988
  #t: 1.2e-5
  mean: 1.2e-5

  variable : p-2sd-theoretical
  1.1774751750476762e-5: 1.0
  mean: 1.1774751750476762e-5

  variable : p-3sd
  #f: 1.0
  mean: 0 (0.0)

  variable : p-3sd-theoretical
  2.4598175300466015e-9: 1.0
  mean: 2.4598175300466015e-9

  variable : limit-1sd
  1: 1.0
  mean: 1.0

  variable : p-1sd
  #f: 0.996028
  #t: 0.003972
  mean: 0.003972

  variable : p-1sd-theoretical
  0.003993589074329777: 1.0
  mean: 0.003993589074329777

  num-exceptional: 12

  > (/ 1.2e-5)
  83333.33333333333

  Another run: 10 of 1000000:

  x: (2.3827632376653676 2.6549558619482427 2.168392939678968)
  x: (2.5584493222455613 2.0111111991793047 2.124475910263515)
  x: (2.0867048468188245 2.7232013865385305 2.0829754525011346)
  x: (2.1389640106285532 3.0528287715717872 2.9760963881429188)
  x: (2.0901440610988145 2.7758957580921844 2.1824546771793867)
  x: (2.1310124010364637 3.3084771299690674 2.1938233424671987)
  x: (2.0978309797048524 3.1055990276609164 2.6375782196120285)
  x: (2.526461830167845 2.2650679113824728 2.556986077032489)
  x: (2.2940495623038095 2.3251955956160395 2.369194265908606)
  x: (2.361137558747621 3.5862134041755263 2.6431478954960297)
  variable : p-2sd
  #f: 0.99999
  #t: 1e-5
  mean: 1e-5

  variable : p-2sd-theoretical
  1.1774751750476762e-5: 1.0
  mean: 1.1774751750476762e-5

  variable : p-3sd
  #f: 1.0
  mean: 0 (0.0)

  variable : p-3sd-theoretical
  2.4598175300466015e-9: 1.0
  mean: 2.4598175300466015e-9

  variable : limit-1sd
  1: 1.0
  mean: 1.0

  variable : p-1sd
  #f: 0.996008
  #t: 0.003992000000000001
  mean: 0.003992000000000001

  variable : p-1sd-theoretical
  0.003993589074329777: 1.0
  mean: 0.003993589074329777

  num-exceptional: 10


  Running 20 simulations of 1_000_000:

  (total-exceptional (13 12 11 10 7 13 7 10 11 9 8 13 13 7 9 11 9 5 14 8) sum: 200)
  (13 : 0.1724137931034483)
  (7 : 0.13793103448275862)
  (9 : 0.13793103448275862)
  (11 : 0.13793103448275862)
  (8 : 0.10344827586206896)
  (10 : 0.10344827586206896)
  (5 : 0.06896551724137931)
  (12 : 0.06896551724137931)
  (14 : 0.06896551724137931)
  (mean: 10.0)


|#
(define num-exceptional 0)
(define num-exceptional-1sd 0)
(define num-exceptional-2sd 0)
(define num-exceptional-3sd 0)
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num-traits 3)
   (define mu 0)
   (define std 1)

   (define limit-1sd (+ mu (* std 1)))   
   (define limit-2sd (+ mu (* std 2)))
   (define limit-3sd (+ mu (* std 3)))
   
   (define x (for/list ([i num-traits]) (normal mu std)))
   
   ; Probability of n-exceptional
   (define p-1sd (andmap (lambda (trait) (>= trait limit-1sd)) x)) ; 2SD     
   (define p-2sd (andmap (lambda (trait) (>= trait limit-2sd)) x)) ; 2SD  
   (define p-3sd (andmap (lambda (trait) (>= trait limit-3sd)) x)) ; 3SD
   
   (define p-1sd-theoretical (expt (- 1 (dist-cdf (normal-dist mu std) limit-1sd)) num-traits))   
   (define p-2sd-theoretical (expt (- 1 (dist-cdf (normal-dist mu std) limit-2sd)) num-traits))
   (define p-3sd-theoretical (expt (- 1 (dist-cdf (normal-dist mu std) limit-3sd)) num-traits))  
   
   (when p-1sd
     (set! num-exceptional-1sd (add1 num-exceptional-1sd)))
  
   (when p-2sd
     (show2 "x" x "p-2sd" p-2sd "p-3sd" p-3sd)
     (set! num-exceptional (add1 num-exceptional))
     (when p-2sd
       (set! num-exceptional-2sd (add1 num-exceptional-2sd)))
     (when p-3sd
       (set! num-exceptional-3sd (add1 num-exceptional-3sd)))
     )
   
   (list limit-2sd
         p-2sd
         p-2sd-theoretical
         limit-3sd
         p-3sd
         p-3sd-theoretical
         limit-1sd
         p-1sd
         p-1sd-theoretical
         
         )
   
   )
)

(set! num-exceptional 0)
(set! num-exceptional-1sd 0)
(set! num-exceptional-2sd 0)
(set! num-exceptional-3sd 0)
(define total-exceptional '())
(for ([i 1])
  (set! num-exceptional 0)
  (show-marginals (model)
                  (list  "limit-2sd"
                         "p-2sd"
                         "p-2sd-theoretical"
                         "limit-3sd"
                         "p-3sd"
                         "p-3sd-theoretical"
                         "limit-1sd"
                         "p-1sd"
                         "p-1sd-theoretical"
                                                  
                         )
                  #:num-samples 1000000
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
  (show "num-exceptional" num-exceptional)
  (set! total-exceptional (append total-exceptional (list num-exceptional)))
  (newline)
  )

(show2 "total-exceptional" total-exceptional "sum:" (sum total-exceptional))
(show-freq total-exceptional)
(newline)
(show2 "num-exceptional-2sd" num-exceptional-2sd "num-exceptional-3sd" num-exceptional-3sd)
(show2 "num-exceptional-1sd" num-exceptional-1sd)
(newline)
