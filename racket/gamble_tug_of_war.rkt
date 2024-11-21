#| 

  Tug of war in Racket/Gamble 

  From Hakaru example documentation/tugofwar_rejection.hk

  $ hakaru tugofwar_rejection.hk | head -100000 | collect
  1.0	true: 69223
  1.0	false: 30777

  In this model we observe that Alice wins of Bob.

  variable : match1
  alice: 0.9999999999999463

  variable : match2
  carol: 0.6710999999999825
  bob: 0.3289000000000201

  variable : match3
  alice: 0.6526999999999845
  carol: 0.3473000000000181

  variable : (strength alice)
  mean: 0.9466452533086277

  variable : (strength bob)
  mean: 0.6669176708970358

  variable : (strength carol)
  mean: 0.8000462930774284

  variable : (pulls alice)
  mean: 1.57755644283945

  variable : (pulls bob)
  mean: 0.6625856400969847

  variable : (pulls carol)
  mean: 1.1482706395796345


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

   (defmem (strength p) (abs (normal 0 1)))
   (defmem (pulls p) (abs (normal (strength p) 1)))

   ; The Hakary/BLOG model had "pulls(a) < pulls(b) "
   ; which is a little strange so I change to return the winner.
   (define (winner a b) (if (> (pulls a) (pulls b)) a b))
    
   (define match1 (winner "alice" "bob"))
   (define match2 (winner "bob" "carol"))
   (define match3 (winner "alice" "carol"))
    
   (observe/fail (eq? match1 "alice"))
   ; (observe/fail (eq? match2 "bob"))

   (list match1
         match2
         match3
         (strength "alice")
         (strength "bob")
         (strength "carol")
         (pulls "alice")
         (pulls "bob")
         (pulls "carol")
         )

   )
)

(show-marginals (model)
              (list  "match1"
                     "match2"
                     "match3"
                     "(strength alice)"
                     "(strength bob)"
                     "(strength carol)"
                     "(pulls alice)"
                     "(pulls bob)"
                     "(pulls carol)"
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


