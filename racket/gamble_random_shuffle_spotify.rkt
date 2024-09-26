#| 

  Random Shuffle (in Spotify) in Racket.Gamble 

  From a (Swedish) forum discussion regarding Spotify's non random shuffling
  of playlists (https://swedroid.se/folk-har-problem-med-spotifys-slumpmassiga-uppspelning/).

  Some questions:

  * What is the mean value until any of the previous tunes is played again?
    Let's assume 100 tunes in a playlist.
    
    See model1.    


  * What is the probability that the same tune is played immediately after
    it has played.

    See model2

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


#|
  Model 1 

  - What is the mean value until any of the previous played tunes is played again?
    Let's assume 100 tunes in a playlist.
    
   * For n = 100:

   Model1
   var : len
   9: 0.06310000000000002
   11: 0.06180000000000001
   8: 0.061000000000000006
   12: 0.06090000000000001
   10: 0.059500000000000004
   ...
   35: 0.0006000000000000001
   37: 0.00030000000000000003
   38: 0.00030000000000000003
   34: 0.00020000000000000004
   36: 0.00020000000000000004
   mean: 12.187600000000002
   Min: 1 Mean: 12.2274 Max: 45 Variance: 38.91368924 Stddev: 6.23808377949511
   Credible interval (0.84): 2..19
   Percentiles:
   (0.01 1)
   (0.1 5)
   (0.025 2)
   (0.05 3)
   (0.25 7)
   (0.5 12)
   (0.75 16)
   (0.84 19)
   (0.9 21)
   (0.95 24)
   (0.975 26)
   (0.99 29)
   (0.999 34)

   Thus, if we have 100 tunes and they are shuffled (i.e. drawn randomly), we can expect
   to listen to an already played tune after about 12 tunes. 
   However, we should not be very surprised (at surprise level 0.05) if it's just after 3 tunes, 
   or as long as after 24 tunes.


   * For n=10
   var : len
   3: 0.232
   4: 0.186
   2: 0.183
   5: 0.147
   1: 0.099
   6: 0.092
   ...
   7: 0.044
   8: 0.014
   9: 0.003
   mean: 3.6390000000000002
   Min: 1 Mean: 3.659 Max: 9 Variance: 2.934719 Stddev: 1.7131021569071705

   enumerate gives the following exact calculations (8 minutes):
   var : len
   3: 27/125 (0.216)
   4: 126/625 (0.2016)
   2: 9/50 (0.18)
   5: 189/1250 (0.1512)
   1: 1/10 (0.1)
   6: 567/6250 (0.09072)
   7: 1323/31250 (0.042336)
   8: 1134/78125 (0.0145152)
   9: 5103/1562500 (0.00326592)
   10: 567/1562500 (0.00036288)
   mean: 5719087/1562500 (3.66021568)


   * For n = 1000

   var : len
   35: 0.02899999999999998
   36: 0.02599999999999998
   17: 0.024999999999999984
   27: 0.023999999999999983
   31: 0.023999999999999983
   ...
   97: 0.0009999999999999994
   98: 0.0009999999999999994
   104: 0.0009999999999999994
   107: 0.0009999999999999994
   111: 0.0009999999999999994
   mean: 38.55899999999997
   Min: 2 Mean: 38.93 Max: 108 Variance: 460.7551 Stddev: 21.465206730893602



|#
(define (model1)
  (; enumerate ; #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 100)

   (define (f a)
     (let ([len (length a)])
       (if (= n len)
           n ; all tunes have been played and no duplicates
           (let ([i (random-integer n)])
             (if (member i a)
                 len
                 (f (append a (list i)))
                 )))))
   
   (define len (f '()))

   
   ;; (define x (for/list ([i n]) (random-integer n)))
   ;; (define len (let ([t (for/first ([i (range 1 n)]
   ;;               #:when (member (list-ref x i) (list-slice x 0 (sub1 i))))
   ;;                        i)])
   ;;               (if t t n))) ; Fix for the case when there's no match
                 

   
   (list len
         )

   )
)


#|
(displayln "Model1")
(show-marginals (model1)
                (list  "len"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.90
                    ; #:credible-interval2 0.90                    
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
|#

#|
  - What is the probability that the same tune is played immediately after
    it has played.
 
  Let's first assume 100 tunes in the playlist.
  * n = 100
  var : found-a-duplicate
  #t: 0.6342
  #f: 0.3658
  mean: 0.6342


  * n = 10
  var : found-a-duplicate
  #t: 0.61277
  #f: 0.38723
  mean: 0.61277


  * n = 1000
  var : found-a-duplicate
  #t: 0.65
  #f: 0.35
  mean: 0.65
  
|#

(define (model2)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 100)

   (define (f a)
     (let ([len (length a)])
       (if (= n len)
           #f ; all tunes have been played, so no duplicate failed
           (let ([i (random-integer n)])
             (if (and (> len 0) (= i (last a)))
                 #t ; Same tune as last is played again
                 (f (append a (list i)))
                 )))))

   (define found-a-duplicate (f '()))

   (list found-a-duplicate )

   )
)

(displayln "Model2")
(show-marginals (model2)
              (list  "found-a-duplicate"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )

