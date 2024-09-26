#| 

  Coin: HH vs HT in Racket.Gamble 

  From Daniel Litt ( Mar 16, 2024)
  https://x.com/littmath/status/1769044719034647001
  """
  Flip a fair coin 100 times - it gives a sequence of heads (H) and tails (T). 
  For each HH in the sequence of flips, Alice gets a point; for each HT, 
  Bob does, so e.g. for the sequence THHHT Alice gets 2 points and Bob gets 1 point. 
  Who is most likely to win?
    Alice 26.3%
    Bob   10.2% 
    Equally likely 42.8%
    See results 20.7%
  """

  The problem is also discussed in the Quanta Magazine article on Daniel Litt:
  "Perplexing the Web, One Probability Puzzle at a Time" by Erica Klarreich:
  https://www.quantamagazine.org/perplexing-the-web-one-probability-puzzle-at-a-time-20240829/?mc_cid=94caee8978

  Unfortunately, enumerate (even with #:limit 1e-05) is too slow.

  With importance-sampler (100000 samples):

  var : alice
  mean: 24.77257000000001

  var : bob
  mean: 24.728940000000026

  var : alice_point
  mean: 0.9999999999999998

  var : bob_point
  mean: 0.9999999999999998

  var : alice_better_than_bob
  mean: 0.46143000000000056

  var : bob_better_than_alice
  mean: 0.48289000000000076

  var : same
  mean: 0.05568000000000004

  Note: The values of alice and bob should be exactly the same.
  So Bob has a slight advantage (bob_better_than_alice > alice_better_than_bob) 
  even though the average points are the same, i.e. values alice and bob.

  However, we can see the exact probabilities for n=20 (instead of 100) using 
  enumerate:

  var : alice
  mean: 19/4 (4.75)

  var : bob
  mean: 19/4 (4.75)

  var : alice_point
  mean: 1030865/1048576 (0.9831094741821289)

  var : bob_point
  mean: 1048555/1048576 (0.9999799728393555)

  var : alice_better_than_bob
  mean: 106101/262144 (0.4047431945800781)

  var : bob_better_than_alice
  mean: 490435/1048576 (0.4677152633666992)

  var : same
  mean: 133737/1048576 (0.12754154205322266)

  This clearly shows that bob is better off 
    bob_better_than_alice (0.4677152633666992)
    > 
    alice_better_than_bob (0.4047431945800781)
  even though he and alice has the same average points (4.75).

  It's also interesting that Alice can get more points than Bob.
  Here for n=20, where Alice can get a maximum of 19 points, whereas
  Bob can get a maximum of 10 points.

  var : alice
  4: 10803/65536 (0.1648406982421875)
  5: 162843/1048576 (0.15529918670654297)
  3: 155141/1048576 (0.14795398712158203)
  6: 16695/131072 (0.12737274169921875)
  2: 28017/262144 (0.10687637329101563)
  7: 97153/1048576 (0.0926523208618164)
  8: 31713/524288 (0.06048774719238281)
  1: 59155/1048576 (0.05641460418701172)
  9: 18749/524288 (0.03576087951660156)
  10: 10059/524288 (0.019186019897460938)
  0: 17711/1048576 (0.016890525817871094)
  11: 2475/262144 (0.009441375732421875)
  12: 547/131072 (0.00417327880859375)
  13: 1827/1048576 (0.0017423629760742188)
  14: 159/262144 (0.000606536865234375)
  15: 237/1048576 (0.00022602081298828125)
  16: 55/1048576 (5.245208740234375e-5)
  17: 21/1048576 (2.002716064453125e-5)
  18: 1/524288 (1.9073486328125e-6)
  19: 1/1048576 (9.5367431640625e-7)
  mean: 19/4 (4.75)

  var : bob
  5: 88179/262144 (0.3363761901855469)
  4: 146965/524288 (0.28031349182128906)
  6: 101745/524288 (0.1940631866455078)
  3: 14535/131072 (0.11089324951171875)
  7: 6783/131072 (0.05175018310546875)
  2: 20349/1048576 (0.01940631866455078)
  8: 5985/1048576 (0.005707740783691406)
  1: 665/524288 (0.0012683868408203125)
  9: 105/524288 (0.0002002716064453125)
  0: 21/1048576 (2.002716064453125e-5)
  10: 1/1048576 (9.5367431640625e-7)
  mean: 19/4 (4.75)


  Also see: 
  *  the paper by Shalosh B. EKHAD and Doron ZEILBERGER: 
    "How to Answer Questions of the Type: If you toss a coin n times, how likely is HH to show up more than HT?"
    https://arxiv.org/pdf/2405.13561
    (The paper refers to and discusses a Maple package that calculates the 
     exact probabilities which calculates the exact probabilities for
     much larger sequences than this Gamble/Racket model.)

  * Geoffrey R. Grimmett:
    "Alice and Bob on X: reversal, coupling, renewal"
    https://arxiv.org/pdf/2409.00732

  * Simon Segert: 
    "A proof that HT is more likely to outnumber HH than vice versa in a sequence of n coin flips"
    https://arxiv.org/pdf/2405.16660
    
  * Emin's Page: The Coin Paradox 
    https://web.archive.org/web/20031208074118/www.csua.berkeley.edu/~emin/writings/coinGame.html

  Also see my WebPPL model coin_hh_vs_ht.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate ; #:limit 0.1 ; way too slow for n=100
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ;; #:transition (enumerative-gibbs)

   (define n 11)
   (define H 1)
   (define T 0)

   ; (defmem (a i) (if (flip 1/2 ) H T ))
   ; An explicit list seems to be a little faster
   (define as (for/list ([i n]) (if (flip 1/2 ) H T )))
   (define (a i) (list-ref as i))

   (define alice (for/sum ([i (sub1 n)]) (if (and (= (a i) H) (= (a (add1 i)) H)) 1 0)))
   (define bob   (for/sum ([i (sub1 n)]) (if (and (= (a i) H) (= (a (add1 i)) T)) 1 0)))
    
   (define alice_point (> alice 0))
   (define bob_point (> bob 0))

   (define alice_better_than_bob (> alice bob))
   (define diff1 (- alice bob))

   (define bob_better_than_alice (> bob alice))
   (define diff2 (- bob alice))

   (define same (= alice bob))

   (list alice
         bob
         alice_point
         bob_point
         alice_better_than_bob
         ; diff1
         bob_better_than_alice
         ; diff2
         same
    )

   )
)

(show-marginals (model)
                (list  "alice"
                       "bob"
                       "alice_point"
                       "bob_point"
                       "alice_better_than_bob"
                       ; "diff1"
                       "bob_better_than_alice"
                       ; "diff2"
                       "same"
                     )
                #:num-samples 100000
                ; #:truncate-output 1
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


#|
  Some experiments to show that the result of n=10 and n=20 are not flukes 
  (though it might not generalize for larger and larger n's).

  Here we show the means of x_better_than_y for some n and the difference
  (negative value: bob is better off than alice).
  
  The means are:   
  (
   probability of alice_is_better_than_bob 
   probability of bob_is_better_than_alice
   )

  It seems that the difference between the two are shrinking but not monotonic.

  Starting from n=3, bob_better_than_alice is larger than alice_better_than bob.

  enumerate for n=1..25 (after that: out of memory error)

  n: 1: means (0 0) diff: 0
  n: 2: means (0.25 0.25) diff: 0.0
  n: 3: means (0.25 0.375) diff: -0.125
  n: 4: means (0.25 0.375) diff: -0.125
  n: 5: means (0.3125 0.40625) diff: -0.09375
  n: 6: means (0.328125 0.4375) diff: -0.109375
  n: 7: means (0.328125 0.4375) diff: -0.109375
  n: 8: means (0.34765625 0.44140625) diff: -0.09375
  n: 9: means (0.359375 0.451171875) diff: -0.091796875
  n: 10: means (0.3623046875 0.453125) diff: -0.0908203125
  n: 11: means (0.3701171875 0.4541015625) diff: -0.083984375
  n: 12: means (0.37744140625 0.457763671875) diff: -0.080322265625
  n: 13: means (0.381103515625 0.459716796875) diff: -0.07861328125
  n: 14: means (0.38543701171875 0.46063232421875) diff: -0.0751953125
  n: 15: means (0.39007568359375 0.462371826171875) diff: -0.072296142578125
  n: 16: means (0.39337158203125 0.463836669921875) diff: -0.070465087890625
  n: 17: means (0.3964385986328125 0.46475982666015625) diff: -0.06832122802734375
  n: 18: means (0.3996238708496094 0.4658355712890625) diff: -0.06621170043945313
  n: 19: means (0.4023246765136719 0.4668922424316406) diff: -0.06456756591796875
  n: 20: means (0.4047431945800781 0.4677152633666992) diff: -0.0629720687866211
  n: 21: means (0.40712642669677734 0.46851348876953125) diff: -0.061387062072753906
  n: 22: means (0.40930724143981934 0.46930623054504395) diff: -0.05999898910522461
  n: 23: means (0.41129088401794434 0.47000038623809814) diff: -0.05870950222015381
  n: 24: means (0.41318947076797485 0.4706451892852783) diff: -0.05745571851730347
  n: 25: means (0.41497528553009033 0.4712721109390259) diff: -0.05629682540893555

  Using importance-sampler (10000 samples) for n=90..110

  n: 90: means (0.4571 0.4791) diff: -0.02200000000000002
  n: 91: means (0.4523 0.483) diff: -0.030700000000000005
  n: 92: means (0.4594 0.4788) diff: -0.01940000000000003
  n: 93: means (0.4582 0.4807) diff: -0.02250000000000002
  n: 94: means (0.4585 0.4848) diff: -0.02629999999999999
  n: 95: means (0.4493 0.4896) diff: -0.0403
  n: 96: means (0.457 0.4844) diff: -0.02739999999999998
  n: 97: means (0.4482 0.4971) diff: -0.0489
  n: 98: means (0.4536 0.4904) diff: -0.0368
  n: 99: means (0.4619 0.4795) diff: -0.017600000000000005
  n: 100: means (0.4533 0.4922) diff: -0.038900000000000046
  n: 101: means (0.4573 0.4916) diff: -0.0343
  n: 102: means (0.4561 0.4883) diff: -0.032200000000000006
  n: 103: means (0.4583 0.4874) diff: -0.029100000000000015
  n: 104: means (0.4662 0.4762) diff: -0.010000000000000009
  n: 105: means (0.4629 0.4804) diff: -0.017500000000000016
  n: 106: means (0.4561 0.4869) diff: -0.030799999999999994
  n: 107: means (0.449 0.4973) diff: -0.04830000000000001
  n: 108: means (0.4542 0.4894) diff: -0.03520000000000001
  n: 109: means (0.461 0.4865) diff: -0.025499999999999967
  n: 110: means (0.457 0.4926) diff: -0.035599999999999965

  Testing some larger values does not support the claim since 
  we now observe some instances where Alice is better off than 
  Bob, but since it's just from random samples it's not conclusive...

  n: 197: means (0.4706 0.4865) diff: -0.01589999999999997
  n: 198: means (0.4777 0.4844) diff: -0.006699999999999984
  n: 199: means (0.4711 0.4885) diff: -0.01739999999999997
  n: 200: means (0.4716 0.4869) diff: -0.01529999999999998
  n: 201: means (0.4822 0.4765) diff: 0.005700000000000038 <---
  n: 202: means (0.4784 0.4863) diff: -0.007900000000000018
  n: 203: means (0.4702 0.4891) diff: -0.018899999999999972

  testing n=201 some more times:
  n: 201: means (0.4678 0.4901) diff: -0.022299999999999986
  n: 201: means (0.4744 0.4886) diff: -0.01419999999999999

  and with 100000 samples:
  n: 201: means (0.47078 0.4886) diff: -0.017820000000000003


|#
; 
; Just returning x_is_better_than_y
;
(define (model2 n)
  (enumerate  ; #:limit 0.1 ; way too slow for n=100, even with limit 0.1
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ;; #:transition (enumerative-gibbs)

   ; (define n 11)
   (define H 1)
   (define T 0)
   
   (define as (for/list ([i n]) (if (flip 1/2 ) H T )))
   (define (a i) (list-ref as i))
  
   (define alice (for/sum ([i (sub1 n)]) (if (and (= (a i) H) (= (a (add1 i)) H)) 1 0)))
   (define bob   (for/sum ([i (sub1 n)]) (if (and (= (a i) H) (= (a (add1 i)) T)) 1 0)))
    
   (define alice_point (> alice 0))
   (define bob_point (> bob 0))

   (define alice_better_than_bob (> alice bob))
   (define diff1 (- alice bob))

   (define bob_better_than_alice (> bob alice))
   (define diff2 (- bob alice))

   (define same (= alice bob))

   (list ; alice
         ; bob
         ; alice_point
         ; bob_point
         alice_better_than_bob
         ; diff1
         bob_better_than_alice
         ; diff2
         ; same
    )

   )
)

#|
(define ps '("alice" "bob  "))
(for ([n (range 1 21)])
  (display (format "n: ~a: " n ))
  (define means (map (lambda (probs)
                       (probs-mean probs)) (get-probs (model2 n) #:num-samples 10000 #:float? #t)))
  (define diff (apply - means))
  (displayln (format "means ~a diff: ~a" means diff))
  (flush-output)
  )
|#
