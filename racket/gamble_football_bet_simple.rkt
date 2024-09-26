#| 

  Football bet simple in Racket Gamble.

  Netica model of a football bet.
  From Neapolitan?
  The Netica model use an utility node (u) and a decision node (accept_bet)
  which I try to model here...

  var : weather
  wet: 1 (1.0)

  var : result
  melbwins: 3/5 (0.6)
  melbloses: 2/5 (0.4)

  var : u
  40: 3/10 (0.3)
  20: 3/10 (0.3)
  -20: 1/5 (0.2)
  -5: 1/5 (0.2)
  mean: 13 (13.0)

  var : accept_bet
  yes: 1/2 (0.5)
  no: 1/2 (0.5)

  var : (accept_bet u)
  (no 20): 3/10 (0.3)
  (yes 40): 3/10 (0.3)
  (yes -20): 1/5 (0.2)
  (no -5): 1/5 (0.2)

  var : u_accept_bet_yes
  0: 1/2 (0.5)
  40: 3/10 (0.3)
  -20: 1/5 (0.2)
  mean: 8 (8.0)

  var : u_accept_bet_no
  0: 1/2 (0.5)
  20: 3/10 (0.3)
  -5: 1/5 (0.2)
  mean: 5 (5.0)

  This is a port of my WebPPL model football_bet_simple.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define weather (categorical-vw2 (vector 30 70) (vector "wet" "dry")))
   (define accept_bet (categorical-vw2 (vector 50 50) (vector "yes" "no")))
    
   (define result (if (eq? weather"wet")
                      (categorical-vw2 (vector 60 40) (vector "melbwins" "melbloses"))
                      (categorical-vw2 (vector 25 75) (vector "melbwins" "melbloses"))))
    
   (define u
     (cond
       [(and (eq? result "melbwins") (eq? accept_bet "yes"))   40]
       [(and (eq? result "melbwins") (eq? accept_bet "no"))    20]
       [(and (eq? result "melbloses") (eq? accept_bet "yes")) -20]
       [(and (eq? result "melbloses") (eq? accept_bet "no"))   -5]
       [else 0]))
    
   (define u_accept_bet_yes (if (eq? accept_bet "yes") u 0))
   (define u_accept_bet_no  (if (eq? accept_bet "no")  u 0))
    
   (observe/fail (eq? weather "wet"))
    ;; (observe/fail (eq? result "melbwins"))

   (list weather
         result
         u
         accept_bet
         (list accept_bet u)
         u_accept_bet_yes
         u_accept_bet_no
    )

   )
  )

(show-marginals (model)
                (list "weather"
                      "result"
                      "u"
                      "accept_bet"
                      "(accept_bet u)"
                      "u_accept_bet_yes"
                      "u_accept_bet_no"
                      )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )




