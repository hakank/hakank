#| 

  Aircraft static in  Racket Gamble.

  From BLOG example/aircraft-static.blog

var : numAircraft
3: 0.3134700000004523
4: 0.2743000000004131
2: 0.16909000000030786
5: 0.147240000000286
...
8: 0.0039000000000074604
9: 0.0007900000000015155
10: 0.00015000000000028746
11: 2.0000000000038328e-5
mean: 3.6176300000060007

var : sumBlip
4: 1.0000000000039144
mean: 4.000000000015658

var : localtion 0
128.33277459564087: 2.0000000000038328e-5
140.57968616033315: 2.0000000000038328e-5
193.01118837816807: 2.0000000000038328e-5
170.98492634595948: 1.0000000000019164e-5
...
162.94371383085206: 1.0000000000019164e-5
131.378239492577: 1.0000000000019164e-5
196.87060998964284: 1.0000000000019164e-5
169.82090657640913: 1.0000000000019164e-5
mean: 149.90335217098087


  This is a port of my WebPPL model aircraft_static.wppl 
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   
   (define numAircraft (poisson 3))
   (define numBlip (poisson 2))
   
   (define (blipPerAircraft a)  (poisson 1.0))
   
   (define sumBlip (for/sum ([a numAircraft])
                     (blipPerAircraft a)))
    
   (define (location a) (uniform 100.0 200.0))

   (defmem (blipLocation b)
        (if (> (blipPerAircraft b) 0)
            (normal-dist (blipPerAircraft b) 1.0)
            (uniform-dist 90.0 210.0)))

   (observe/fail (= sumBlip 4))

    ;; observe(blipLocation(0),156);
    ;; observe(blipLocation(1),133);
    ;; observe(blipLocation(2),158);
    ;; observe(blipLocation(3),180);

   (list 
        numAircraft
        sumBlip
        (location 0)
        ;; locationB1:location(1),
        ;; locationB2:location(2),
        ;; locationB3:location(3),
    )

   )
  )

(show-marginals (model)
                  (list "numAircraft"
                        "sumBlip"
                        "localtion 0"
                        )
                  #:num-samples 100000
                  #:truncate-output 4
                  ; #:skip-marginals? #t
                  ; #:credible-interval 0.94
                  ; #:show-stats? #t
                  ; #:show-histogram? #t
                  )

