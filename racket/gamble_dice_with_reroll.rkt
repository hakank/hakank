#| 

  Dice with re-roll in Racket.Gamble 

  From https://www.reddit.com/r/Probability/comments/1duf4na/dice_probability_involving_rerolls/
  """
  Dice probability involving re-rolls

  Hi, In a scenario where you need to roll a 6 on a d6 dice for a game, 
  the chance is X/6 of success, where X is the number of d6 dice rolled.

  However you are allowed to re-roll results of 1 or 2. There can only be one 
  re-roll, further results of 1 or 2 do not generate re-rolls.

  What is the probability of success?

  My guess is (X/6) + ((1/3)(X/6)) but I'm uncertain if I've missed something. 
  """

  For 1..5 dice enumerate is used, after that importance-sampler.

  * For 1 die
  var : dice
  3: 2/9 (0.2222222222222222)
  4: 2/9 (0.2222222222222222)
  5: 2/9 (0.2222222222222222)
  6: 2/9 (0.2222222222222222)
  1: 1/18 (0.05555555555555555)
  2: 1/18 (0.05555555555555555)
  mean: 25/6 (4.166666666666667)

  var : p
  #f: 7/9 (0.7777777777777778)
  #t: 2/9 (0.2222222222222222)
  mean: 2/9 (0.2222222222222222)


  * For 2 dice
  var : num 6s
  0: 49/81 (0.6049382716049383)
  1: 28/81 (0.345679012345679)
  2: 4/81 (0.04938271604938271)
  mean: 4/9 (0.4444444444444444)

  var : p
  #f: 49/81 (0.6049382716049383)
  #t: 32/81 (0.3950617283950617)
  mean: 32/81 (0.3950617283950617)

  * For 3 dice
  var : num 6s
  0: 343/729 (0.47050754458161864)
  1: 98/243 (0.40329218106995884)
  2: 28/243 (0.11522633744855967)
  3: 8/729 (0.010973936899862825)
  mean: 2/3 (0.6666666666666666)

  var : p
  #t: 386/729 (0.5294924554183813)
  #f: 343/729 (0.47050754458161864)
  mean: 386/729 (0.5294924554183813)


  * For 4 dice
  var : num 6s
  1: 2744/6561 (0.41822892851699434)
  0: 2401/6561 (0.3659503124523701)
  2: 392/2187 (0.17924096936442616)
  3: 224/6561 (0.034141137021795456)
  4: 16/6561 (0.0024386526444139613)
  mean: 8/9 (0.8888888888888888)

  var : p
  #t: 4160/6561 (0.6340496875476299)
  #f: 2401/6561 (0.3659503124523701)
  mean: 4160/6561 (0.6340496875476299)

  * For 5 dice
  var : num 6s
  1: 24010/59049 (0.4066114582804112)
  0: 16807/59049 (0.28462802079628785)
  2: 13720/59049 (0.23234940473166354)
  3: 3920/59049 (0.06638554420904673)
  4: 560/59049 (0.009483649172720961)
  5: 32/59049 (0.0005419228098697691)
  mean: 10/9 (1.1111111111111112)

  var : p
  #t: 42242/59049 (0.7153719792037122)
  #f: 16807/59049 (0.28462802079628785)
  mean: 42242/59049 (0.7153719792037122)

  * For 6 dice 
    This is too slow for enumerate -> importance-sampler

  var : num 6s
  1: 0.38023
  2: 0.27082
  0: 0.22162
  3: 0.10234
  4: 0.02211
  5: 0.00276
  6: 0.00012
  mean: 1.3318500000000002

  var : p
  #t: 0.77838
  #f: 0.22162
  mean: 0.77838


  * For 10 dice

  var : num 6s
  2: 0.29560000000000003
  1: 0.23312000000000002
  3: 0.22733000000000003
  4: 0.11227000000000001
  0: 0.08192000000000002
  5: 0.03833000000000001
  6: 0.009780000000000002
  7: 0.0015000000000000002
  8: 0.00014000000000000001
  9: 1.0000000000000003e-5
  mean: 2.21743

  var : p
  #t: 0.91808
  #f: 0.08192000000000002
  mean: 0.91808

 * For 20 dice

  var : num 6s
  4: 0.21222000000000002
  5: 0.19323000000000004
  3: 0.17459000000000002
  6: 0.13845000000000002
  2: 0.10261000000000002
  7: 0.07881000000000002
  1: 0.037430000000000005
  8: 0.03626000000000001
  9: 0.014110000000000001
  0: 0.006390000000000001
  10: 0.004380000000000001
  11: 0.0012700000000000003
  12: 0.00017000000000000004
  13: 6.000000000000001e-5
  14: 2.0000000000000005e-5
  mean: 4.44176

  var : p
  #t: 0.99361
  #f: 0.006390000000000001
  mean: 0.99361



  Note: the answer includes a formula:
    (5/6)^X * (1-(3/5)^X) * 1/6. 
  where X is the number of dice thrown.
  But this makes no sense since the probability actually _decreases_ 
  when the number of dice increases:
( theoretical n 1 1/18 0.05555555555555555)
( theoretical n 2 2/27 0.07407407407407407)
( theoretical n 3 49/648 0.07561728395061729)
( theoretical n 4 17/243 0.06995884773662552)
( theoretical n 5 1441/23328 0.061771262002743486)
( theoretical n 6 931/17496 0.053212162780064014)
( theoretical n 7 37969/839808 0.045211524538942234)
( theoretical n 8 6001/157464 0.03811029822689631)
( theoretical n 9 966721/30233088 0.031975595744635814)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;
; From the answer, the formula for the probability of a 6 with X dice:
;   (5/6)^X * (1-(3/5)^X) * 1/6.
; But it seems to be wrong. See comment above.
;
(define (theoretical n) (* (expt 5/6 n) (- 1 (expt 3/5 n)) 1/6))

;
; From n >= 6 switch to importance-sampler since enumerate is too slow
;
(define (model)
  (enumerate ; for n=1..5
   ; rejection-sampler 
   ; importance-sampler ; for n > 5
   ; mh-sampler

   (define n 5)
   
   (defmem (dice i)
     (let ([d1 (add1 (random-integer 6))])
       (if (<= d1 2) (add1 (random-integer 6)) d1)))
   
   (define all-dice (for/list ([i n]) (dice i)))

   (define num-6s (for/sum ([i n]) (if (= (dice i) 6) 1 0 )))
   ; (define p1 (ormap (lambda (d) (= d 6)) all-dice))
   (define p (> num-6s 0))

   (list num-6s
         p
         )

   )
)

(show-marginals (model)
                (list "num 6s"
                      "p"
                      )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

;; (for ([n (range 10)])
;;   (show2 " theoretical n" n (theoretical n) (exact->inexact (theoretical n))))
