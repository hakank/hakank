#| 

  My neighbour in Racket/Gamble 

  This is a "real world" problem.

  My beautiful neighbour and I look out of our windows M and N times a day, respectively. 
  How many times do we look out at the same time?

  Say that there are 24*60 (=1440) timeslots a day, and we each look out 40 times a day
  (independently of each other). Then, on average, we would look out at the same time
  about once a day.

  var : s_me
  1: 0.3716
  0: 0.3171
  2: 0.2041
  3: 0.0803
  4: 0.0221
  5: 0.0043
  6: 0.0002
  7: 0.0002
  8: 0.0001
  mean: 1.1340000000000003


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

   (define mins (* 24 60)) ; Number of minutes per day

   (define n 40) ; Number of times I look out
   (define m 40) ; Number of times my neighbour look out

   ; The time slots that we look out
   (define me (for/list ([i mins]) (bernoulli (/ n mins))))
   (define neighbour (for/list ([i mins]) (bernoulli (/ n mins))))   

   ; Total number of looking out
   (define s_me (sum me))
   (define s_neighbour (sum neighbour))

    ; Do we look out at the same time?
   (define same_time (for/sum ([i mins])
                       (boolean->integer (and (= (list-ref me i) 1)
                                              (= (list-ref neighbour i) 1)))))

   (list same_time
         ;; s_me
         ;; s_neighbour
         )

   )
)

(show-marginals (model)
                (list  "same_time"
                       ;; "s_me"
                       ;; "s_neighbour"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


