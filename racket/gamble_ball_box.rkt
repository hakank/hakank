#| 

  Ball box in Racket.Gamble 

  From https://bitbucket.org/pedrozudo/hal_problog/src/master/examples/ball_box.pl
  """
  beta(2,2)~b.

  P::p:- b~=P.

  box(1):-p.
  box(2):- \+p.

  1/4::ball(X, red);3/4::ball(X, white):- box(1).
  3/4::ball(X, red);1/4::ball(X, white):- box(2).


  evidence(ball(1,red)).
  evidence(ball(2,red)).

  :-free(b).
  query(density(b)).
  """

  What I understand it's a common pick a ball from a random box-problem:
  - There are two boxes (box 1 and box 2)
  - box 1 contains 1 red ball and 3 white balls
  - box 2 contains 3 red balls and 1 white ball
  - one random box is selected 
  - one ball is picked from that box (and returned I suppose): it's a red ball
  - again one ball is picked from the same box: it's also red
  - Question: which box was selected?
  - Answer: It's box 2 with a probability of about 90%


  var : b
  mean: 0.4178775106218638

  var : p
  #f: 0.9063999999999566
  #t: 0.093600000000007
  mean: 0.093600000000007

  var : box
  2: 0.9063999999999566
  1: 0.093600000000007
  mean: 1.9063999999999202

  * If we pick a 3rd ball which we observe is also red:
  2: 0.9650999999999501
  1: 0.0349000000000034
  mean: 1.9650999999999037

  * And a 4th ball which is also observed to be red:
  var : box
  2: 0.9866999999999477
  1: 0.013300000000001286
  mean: 1.9866999999998967



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

   (define b (beta 2 2))
   (define p (flip b))
    
   (define box (if p 1 2))

   (define red 1)
   (define white 2)
      
   (defmem (ball the_box) 
     (if (= box 1)
         (categorical-vw2 (vector 1/4 3/4) (vector "red" "white"))
         (categorical-vw2 (vector 3/4 1/4) (vector "red" "white"))))

   (observe/fail (eq? (ball 1) "red"))
   (observe/fail (eq? (ball 2) "red"))
   ; Let's continue to pick balls
   ; (observe/fail (eq? (ball 3) "red"))
   ; (observe/fail (eq? (ball 4) "red"))
    
   (list b
         p
         box
         ; (ball 1)
         ;; (ball 2)
         ;; (ball 3)
         ;; (ball 4)
    )

   )
)

(show-marginals (model)
                (list  "b"
                       "p"
                       "box"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


