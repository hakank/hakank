#| 

  Simple mixture model in Racket.Gamble 

  From SPPL model simple-mixture-model.pynb
  """
  X ~=  .525 * norm(loc=-8, scale=2) \
      | .100 * norm(loc=0, scale=2) \
      | .375 * norm(loc=7, scale=3)
  modelc = n.model.(observe/fail ((0 < n.X) < 10) | ((-6 < n.X) < -3))
  """  

var : c
3: 0.3372000000000192
2: 0.3365000000000193
1: 0.3263000000000204
mean: 2.0109000000001163
Credible interval (0.94): 1..3

var : x
-0.06787128597250834: 0.00010000000000000938
1.1765592612374196: 0.00010000000000000938
-0.22122254393227947: 0.00010000000000000938
-4.96897501835264: 0.00010000000000000938
0.293338507545082: 0.00010000000000000938
...
-0.19458548789914773: 0.00010000000000000938
0.24620529238728311: 0.00010000000000000938
2.2151343563550903: 0.00010000000000000938
2.776356415120491: 0.00010000000000000938
-3.0622689815252944: 0.00010000000000000938
mean: -0.4603927505225239
Credible interval (0.94): -5.552174455593028..3.917062256523029


Clusters:

Cluster 1
Stats:
Min: -5.99805058887682 Mean: -4.10440267048029 Max: -0.6227098493493473 Variance: 0.9218640087138883 Stddev: 0.9601374946922385
Percentiles:
(0.01 -5.898687071899308)
(0.1 -5.343355859722192)
(0.025 -5.76883904369848)
(0.05 -5.623110285028178)
(0.25 -4.824423669276613)
(0.5 -4.152972101748606)
(0.75 -3.4492896547478176)
(0.84 -3.1158200779742486)
(0.9 -2.8213930352489336)
(0.95 -2.411687839372125)
(0.975 -2.1461397421475343)
(0.99 -1.8275864963626587)
(0.999 -0.990106224836477)

Cluster 2
Stats:
Min: -0.6466135969897366 Mean: -0.0037442083854677525 Max: 0.6717401033183856 Variance: 0.04047758241631951 Stddev: 0.20119041333105192
Percentiles:
(0.01 -0.4648953400137413)
(0.1 -0.25961840713551204)
(0.025 -0.3962421826890901)
(0.05 -0.3322789424229038)
(0.25 -0.13894103473093683)
(0.5 -0.005956999683738542)
(0.75 0.12998979834018592)
(0.84 0.19720402339662574)
(0.9 0.2550358644647637)
(0.95 0.33757219785822623)
(0.975 0.3919996515826718)
(0.99 0.47317033550069926)
(0.999 0.5974299274365754)

Cluster 3
Stats:
Min: -1.3152416328632006 Mean: 2.621919821941438 Max: 7.136419302213792 Variance: 1.2947567302392191 Stddev: 1.1378737760574409
Percentiles:
(0.01 -0.06545947283892439)
(0.1 1.150904604096044)
(0.025 0.42883359946657307)
(0.05 0.7689541075269848)
(0.25 1.8395643843110139)
(0.5 2.626044334073103)
(0.75 3.3891051568523003)
(0.84 3.7195578969690404)
(0.9 4.046996905829818)
(0.95 4.453271140496883)
(0.975 4.858671099936659)
(0.99 5.247253937986097)
(0.999 6.371523581482332)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;;  The clusters (prior: uniform distribution of the clusters)
   (define c (categorical-vw2 (vector 1/3 1/3 1/3) (vector 1 2 3)))
   
   (define x
     (cond [(= c 1) (* 0.525 (normal -8  2))]
           [(= c 2) (* 0.100 (normal 0 2))]
           [else (* 0.375 (normal 7 3))]))

   ;; Condition
   (observe/fail (or
                  (< 0 x 10)
                  (< -6 x 3)))

   (list c
         x
         )
    
   )
)

(show-marginals (model)
                (list  "c"
                       "x"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.94
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

;
; Show some stats about each cluster
;
(newline)
(displayln "Clusters:")
(for ([cluster '(1 2 3)])
  (let ([vs (map second (filter (lambda (v) (= (first v) cluster)) (make-samples (model) 10000)))])
    (newline)
    (show "cluster" cluster)
    (displayln "Stats:")
    (show-stats vs)
    ; (displayln "Histogram:")    
    ; (show-histogram vs)
    (displayln "Percentiles:")        
    (show-percentiles vs)    
    )
  )


      
