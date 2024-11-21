#| 

  Records in Racket/Gamble 

  This is from Robert Matthews (http://www.numberwatch.co.uk/record.htm  not available anymore)
  """
  Let us invent a stationary process and create a little allegory. In the 
  eighteenth century an eccentric amateur astronomer looks through his low-power
  telescope each night, pointing roughly in the same direction, and counts the 
  number of stars in its field of vision. He is keen to establish a record so 
  that he can apply for entry in the contemporary equivalent of the Guinness 
  Book of Records. The conditions are such that the average number of stars he 
  sees is 100 and the average scatter (standard deviation) is plus or minus ten 
  stars.

  We can model this process by generating numbers from a normal distribution 
  (for the cognoscenti the function rnorm(N,100,10) in Mathcad).

  Our astronomer takes one reading each night and only writes down the current 
  record, the highest value so far, so after a hundred nights he gets something 
  like....
  ....
  By now we have reached the tenth generation. It is the late twentieth century 
  and the age of rationality has come to an end. The new scion of the family, 
  realising that it has obtained no benefit from all these years of effort, 
  publishes a paper warning of the increase in star density that is occurring 
  and calculating all sorts of disasters that arise from the increased 
  gravitation. He is appointed to a professorial chair in a new university and 
  founds a new Department of Celestial Change. The cause is taken up by the 
  Deputy Prime Minister and large sums of money are diverted from other research 
  areas to a new programme for the study of celestial change. Disaster stories 
  are regularly fed to the media, who sell more papers and advertising time.

  A few old-fashioned scientists point out that the results are completely 
  explained by the well-known statistics of extremes, which predict that the 
  largest value will increase as the logarithm of the number of observations. 
  There is no money in that, so they are ignored and the bandwagon rolls on.  
  """

  For n = 20


  for n = 1..20 (just the mean)

  See my Swedish text http://www.hakank.org/sims/simulering.html for an R simulation

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|

  Max/min values of n number of (normal 100 15), n=1..10
  Here we can see that the mean values are increasing for max,
  and decreasing for min.

  
  n: 1
  var : max-val
  mean: 100.03399516817505

  var : min-val
  mean: 100.03399516817505

  n: 2
  var : max-val
  mean: 108.52196654814867

  var : min-val
  mean: 91.61952684669407

  n: 3
  var : max-val
  mean: 112.67037837051963

  var : min-val
  mean: 87.24344368170419

  n: 4
  var : max-val
  mean: 115.37648410669154

  var : min-val
  mean: 84.51151265498628

  n: 5
  var : max-val
  mean: 117.46319401633512

  var : min-val
  mean: 82.54768767516488

  n: 6
  var : max-val
  mean: 118.90215595692472

  var : min-val
  mean: 81.00861272269067

  n: 7
  var : max-val
  mean: 120.25041533487284

  var : min-val
  mean: 79.71260125048892

  n: 8
  var : max-val
  mean: 121.38505010886941

  var : min-val
  mean: 78.7032774905444

  n: 9
  var : max-val
  mean: 122.2063990133632

  var : min-val
  mean: 77.62720205768645

  n: 10
  var : max-val
  mean: 123.10533939687332

  var : min-val
  mean: 76.90529013066107

  ... 

  n: 100
  var : max-val
  mean: 137.61780039041997

  var : min-val
  mean: 62.28097243016903

  * For (poisson 15) we see the same behaviour:
  n: 1
  var : max-val
  mean: 14.988399999999995

  var : min-val
  mean: 14.988399999999995

  n: 2
  var : max-val
  mean: 17.169050000000016

  var : min-val
  mean: 12.82345000000001

  n: 3
  var : max-val
  mean: 18.305400000000027

  var : min-val
  mean: 11.750200000000017

  n: 4
  var : max-val
  mean: 19.087900000000023

  var : min-val
  mean: 11.10365000000001

  n: 5
  var : max-val
  mean: 19.60220000000002

  var : min-val
  mean: 10.62360000000001

  n: 6
  var : max-val
  mean: 20.03565000000001

  var : min-val
  mean: 10.28780000000001

  n: 7
  var : max-val
  mean: 20.436850000000028

  var : min-val
  mean: 9.986250000000013

  n: 8
  var : max-val
  mean: 20.72760000000002

  var : min-val
  mean: 9.763100000000009

  n: 9
  var : max-val
  mean: 20.98930000000001

  var : min-val
  mean: 9.538350000000007

  n: 10
  var : max-val
  mean: 21.190350000000024

  var : min-val
  mean: 9.377600000000008

  ...

  n: 100
  var : max-val
  mean: 25.54960000000002

  var : min-val
  mean: 6.296750000000003



|#
(define (model1 n)
  (show "n" n)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; We could use this by simply take the max of x,
   ; but I like scan...

   (define mu 100)
   (define sigma 15)
   
   (define x (for/list ([i n]) (normal mu sigma)))
   ; (define x (for/list ([i n]) (poisson sigma)))
   ; (define x (for/list ([i n]) (extreme_value_dist2 mu sigma)))
   (define max-val (apply max x))   
   (define min-val (apply min x))
   (list max-val
         min-val         
         )
   )
)

(displayln "\nModel 1 (relation with extreme value distributions")
(for ([n (range 1 11)])
  (show-marginals (model1 n)
                  (list "max-val"
                        "min-val"
                        )
                  #:num-samples 20000
                  #:truncate-output 5
                  #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:hpd-interval (list 0.5 0.84)
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 0
                  )
)




#|
  Some more explorations.

var : last-record
135.45725336055995: 0.0009999999999999994
133.3273023598387: 0.0009999999999999994
120.30035295653732: 0.0009999999999999994
116.65959598281981: 0.0009999999999999994
122.27096864033516: 0.0009999999999999994
...
130.9261522035431: 0.0009999999999999994
138.62020229522338: 0.0009999999999999994
138.45905789108738: 0.0009999999999999994
119.14917910040347: 0.0009999999999999994
119.19235044230263: 0.0009999999999999994
mean: 128.01997282692398

var : num-unique
4: 0.2690000000000002
3: 0.2490000000000002
2: 0.18600000000000014
5: 0.1580000000000001
6: 0.06300000000000003
1: 0.05400000000000002
...
7: 0.01699999999999999
8: 0.0029999999999999983
10: 0.0009999999999999994
mean: 3.5700000000000016

var : ix
8: 0.061000000000000026
18: 0.060000000000000026
1: 0.058000000000000024
4: 0.05700000000000002
11: 0.05700000000000002
...
7: 0.04600000000000001
9: 0.04000000000000001
5: 0.038000000000000006
6: 0.038000000000000006
10: 0.038000000000000006
mean: 9.550000000000002

var : p
#f: 0.6400000000000005
#t: 0.36000000000000026
mean: 0.36000000000000026


|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 20)
   
   (define mu 100)
   (define sigma 15)
   
   (define x (for/list ([i n]) (normal mu sigma)))
   (define records (scan max 0 x))
   (define last-record (last records)) ; This is the max value
   (define num-unique (length (remove-duplicates records)))
   (define p (> last-record 130))
   (define ix (index-of x last-record))
   (list records
         last-record
         num-unique
         ix
         p
         )
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                  (list  "records"
                         "last-record"
                         "num-unique"
                         "ix"
                         "p"
                         )
                  #:num-samples 1000
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

