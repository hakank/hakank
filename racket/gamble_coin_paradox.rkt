#| 

  in Racket.Gamble 

  From https://web.archive.org/web/20031208074118/www.csua.berkeley.edu/~emin/writings/coinGame.html
  """
  ... 

  Now consider a simple variation on this game. Instead of betting on the outcome of a single coin, 
  imagine what would happen if both you and your friend each had a coin. The game would now be 
  played as follows: Each turn you flip your coin and your friend flips his coin. If the sequence 
  (Heads, Tails) occurs on your coin before the sequence (Heads, Heads) occurs on your friends coin, 
  then you win. If the sequence (Heads, Heads) occurs on your friends coin before (Heads, Tails) 
  occurs on your coin, then your friend wins. If the sequence (Heads, Tails) occurs on your coin 
  in the same turn that (Heads, Heads) occurs on your friends coin, then both of you start over 
  (e.g. there are no ties). 

   
  At first glance it seems like the second game also results in each player having a 
  probability of winning of 1/2. However this is not true! The player who bets on 
  (Heads, Tails) will win much more often. The simplest way to see this is by calcuting 
  the average number of flips it takes each player to win.

  Let X_HH be the number of flips it takes to get the sequence (Heads, Heads) on a given 
  fair coin, and let F_k be the result of the kth coin flip (k starts at 1).


  E[X_HH|F_1 = T] = 1 + E[X_HH]

  E[X_HH|F_1 = H] = (1/2) * ( 2 ) + (1/2) * (E[X_HH] + 2)
  E[X_HH|F_1 = H] = 2 + (1/2) * E[X_HH]

  E[X_HH] = (1/2) * ( E[X_HH|F_1 = H] + E[X_HH|F_1 = T] )
  E[X_HH] = (1/2) * ( 1 + E[X_HH] + 2 + (1/2)*E[X_HH])
  E[X_HH] = (1/2) * ( 3 + (3/2)*E[X_HH]) = 3/2 + (3/4) E[X_HH]
  E[X_HH] = 6

  E[X_HT|F_1 = T] = 1 + E[X_HT]

  E[X_HT|F_1 = H] = (1/2) * ( 2 ) + (1/2) * (E[X_HT|F_1 = H] + 1)
  E[X_HT|F_1 = H] = 1 + (1/2) * (E[X_HT|F_1 = H] + 1)
  E[X_HT|F_1 = H] = 3

  E[X_HT] = (1/2) * ( E[X_HT|F_1 = H] + E[X_HT|F_1 = T] )
  E[X_HT] = (1/2) * ( 1 + E[X_HT] + 3 )
  E[X_HT] = 2 + (1/2)*E[X_HT]
  E[X_HT] = 4

  As we showed above the average time until the sequence (Heads, Heads) occurs is six 
  flips, but the average time until the sequence (Heads, Tails) occurs is four flips. 
  Therefore "on average" it will take longer to get (Heads, Heads) on a coin than it 
  will to get (Heads, Tails). Technically this does not show that the person betting 
  on (Heads, Tails) in the second game has a higher probability of winning. To do that 
  we would have to do more detailed calculations similiar to what we did above. However 
  the fact that the average time to get (Heads, Tails) is shorter than the average time 
  to get heads tails is still quite a fascinating result! 
  """
  
  Below are some experiments of this. 

  Cf gamble_coin_hh_vs_ht.rkt

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


#|

  Simpler variant: Throw a coin - say - 20 times and get the length of the sequence to get to 
  the first HT and HH, respectively.

  For 20 throws, enumerate. Note that the probability of winners is identical.

var : winner
a: 524287/1048576 (0.4999990463256836)
b: 524287/1048576 (0.4999990463256836)
tie: 1/524288 (1.9073486328125e-6)

var : ht pos
2: 1/4 (0.25)
3: 1/4 (0.25)
4: 3/16 (0.1875)
5: 1/8 (0.125)
6: 5/64 (0.078125)
7: 3/64 (0.046875)
8: 7/256 (0.02734375)
9: 1/64 (0.015625)
10: 9/1024 (0.0087890625)
11: 5/1024 (0.0048828125)
12: 11/4096 (0.002685546875)
13: 3/2048 (0.00146484375)
14: 13/16384 (0.00079345703125)
15: 7/16384 (0.00042724609375)
16: 15/65536 (0.0002288818359375)
17: 1/8192 (0.0001220703125)
18: 17/262144 (6.4849853515625e-5)
19: 9/262144 (3.4332275390625e-5)
22: 21/1048576 (2.002716064453125e-5)
20: 19/1048576 (1.811981201171875e-5)
mean: 2097151/524288 (3.999998092651367)

var : hh pos
2: 1/4 (0.25)
3: 1/8 (0.125)
4: 1/8 (0.125)
5: 3/32 (0.09375)
6: 5/64 (0.078125)
7: 1/16 (0.0625)
8: 13/256 (0.05078125)
9: 21/512 (0.041015625)
10: 17/512 (0.033203125)
11: 55/2048 (0.02685546875)
12: 89/4096 (0.021728515625)
13: 9/512 (0.017578125)
22: 17711/1048576 (0.016890525817871094)
14: 233/16384 (0.01422119140625)
15: 377/32768 (0.011505126953125)
16: 305/32768 (0.009307861328125)
17: 987/131072 (0.00753021240234375)
18: 1597/262144 (0.006092071533203125)
19: 323/65536 (0.0049285888671875)
20: 4181/1048576 (0.003987312316894531)
mean: 3117071/524288 (5.945341110229492)



|#
(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define coins (list "h" "t"))
   
  
   (define lst (for/list ([i 20]) (uniform-draw coins)))
   (define ht-pos (+ 2 (index-of-sub lst '("h" "t")))) ; add pattern length
   (define hh-pos (+ 2 (index-of-sub lst '("h" "h"))))
   
   (define winner
     (cond [(= ht-pos hh-pos) "tie"]
           [(< ht-pos hh-pos) "a"]
           [(< hh-pos ht-pos) "b"]
           [else "else"]))
   ; (observe/fail (not (= ht-pos hh-pos))) ; No ties
   
   (list winner
         ht-pos
         hh-pos
         )   

   )
)

#|
(show-marginals (model1)
                (list  "winner"
                       "ht pos"
                       "hh pos"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )

|#


#|
  
  Generate a list to get either HH or HT and compare the lengths.

  var : winner
a: 0.5389900000000001
b: 0.3197000000000007
tie: 0.14131000000000027

var : ht pos
2: 0.2536300000000005
3: 0.2505700000000005
4: 0.18602000000000032
5: 0.12404000000000025
6: 0.07853000000000016
...
17: 0.00013000000000000026
18: 8.000000000000016e-5
20: 3.000000000000006e-5
21: 2.000000000000004e-5
19: 1.000000000000002e-5
mean: 3.9827300000000068

var : hh pos
2: 0.24996000000000046
3: 0.12510000000000027
4: 0.12397000000000025
5: 0.09396000000000017
6: 0.07949000000000017
...
49: 1.000000000000002e-5
51: 1.000000000000002e-5
52: 1.000000000000002e-5
53: 1.000000000000002e-5
56: 1.000000000000002e-5
mean: 6.001680000000013

  * Weeding out ties
   Note that the average length increases a little (as expected)

var : winner
a: 0.6242499999999996
b: 0.37575000000000114

var : ht pos
3: 0.25563000000000075
2: 0.2171400000000007
4: 0.19248000000000048
5: 0.13097000000000036
6: 0.08320000000000023
...
17: 0.0001400000000000004
18: 9.000000000000025e-5
19: 4.000000000000011e-5
20: 4.000000000000011e-5
25: 1.0000000000000028e-5
mean: 4.126640000000012

var : hh pos
2: 0.21804000000000062
4: 0.11815000000000032
3: 0.1102700000000003
5: 0.09504000000000029
6: 0.08207000000000021
...
47: 1.0000000000000028e-5
52: 1.0000000000000028e-5
53: 1.0000000000000028e-5
58: 1.0000000000000028e-5
61: 1.0000000000000028e-5
mean: 6.45987000000002


|#

(define (model2)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ;;; (enumerative-gibbs)
   
   (define coins (list "h" "t"))
   
   (define draw (lambda () (uniform-draw coins)))
   (define ht (draw-until-pattern '("h" "t") draw))
   (define hh (draw-until-pattern '("h" "h") draw))
   ; Some other tests
   ; (define ht (draw-until-pattern '("h" "t" "h") draw))
   ; (define hh (draw-until-pattern '("h" "h" "t") draw))
   
   (define winner
     (cond [(= ht hh) "tie"]
           [(< ht hh) "ht"]
           [(< hh ht) "hh"]))

   ; (show2 "hh" hh "ht" ht "winner" winner)
   
   ; (observe/fail (not (= hh ht))) ; no ties

   ;; Experimental (it does not behave as I want)
   ;; (define res (draw-until-patterns '(("h" "t")
   ;;                                    ("h" "h")
   ;;                                    )
   ;;                                  (lambda () (uniform-draw coins))))
   ;; (define winner (list-ref res 0))
   ;; (define len (list-ref res 1))

   (list winner
         ht
         hh
         ; len
         )

   )
)

(displayln "Model 2")
(show-marginals (model2)
                (list  "winner"
                       "ht pos"
                       "hh pos"
                       ; "len"
                     )
                    #:num-samples 100000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )



#|
  Here's a more complex model. It shows the winner but the drawback is that the individual length
  of the two sequences are not available.

  * Using importance-sampler

var : winner
a: 0.6242800000000011
b: 0.3757200000000016

var : winner-list
(h h): 0.2190100000000005
(h t): 0.2170400000000005
(h h t): 0.09067000000000021
(t h t): 0.0901100000000002
(t h h): 0.07262000000000017
...
(t h t h t t t t h h): 1.0000000000000025e-5
(t h t t t h t t h h): 1.0000000000000025e-5
(t t t t h t h t t h h): 1.0000000000000025e-5
(t t h t t h t t h t t t t h h): 1.0000000000000025e-5
(h t h t t t t t t h h): 1.0000000000000025e-5

var : winner-len
2: 0.43605000000000105
3: 0.25340000000000057
4: 0.15399000000000035
5: 0.07919000000000019
6: 0.0409900000000001
...
13: 0.0002400000000000006
14: 7.000000000000017e-5
15: 4.00000000000001e-5
16: 2.000000000000005e-5
18: 2.000000000000005e-5
mean: 3.175900000000008

  * Using enumerate #:limit 1e-01 (5s)
var : winner
a: 36865/59579 (0.6187582873160006)
b: 22714/59579 (0.3812417126839994)

var : winner-list
(h h): 13646/59579 (0.2290404337098642)
(h t): 13636/59579 (0.22887258933516844)
(h h t): 5520/59579 (0.0926500948320717)
(t h t): 5520/59579 (0.0926500948320717)
(t h h): 4416/59579 (0.07412007586565736)
...
(t t h t t h h): 28/59579 (0.0004699642491481898)
(t t t t t h h): 28/59579 (0.0004699642491481898)
(h t t h t h h): 24/59579 (0.00040282649926987697)
(h t h t t h h): 24/59579 (0.00040282649926987697)
(h t t t t h h): 24/59579 (0.00040282649926987697)

var : winner-len
2: 27282/59579 (0.4579130230450326)
3: 15456/59579 (0.25942026552980074)
4: 9248/59579 (0.15522247771865927)
5: 4480/59579 (0.07519427986371037)
6: 2240/59579 (0.037597139931855185)
7: 873/59579 (0.014652813910941774)
...
mean: 179875/59579 (3.01910068984038)


  * Using enumerate #:limit 1e-02 (50s)

var : winner
a: 7902243/12656080 (0.6243831423315909)
b: 4753837/12656080 (0.3756168576684092)

var : winner-list
(h h): 58278337/265777680 (0.21927476001747023)
(h t): 1214132/5537035 (0.21927475625492704)
(h h t): 24225311/265777680 (0.09114877893433339)
(t h t): 865189/9492060 (0.09114870744601278)
(t h h): 4845061/66444420 (0.0729190050872594)
...
(t h t t h t h t t t t h h): 1/66444420 (1.505017276093312e-8)
(h t h t t t t t t t t h h): 1/66444420 (1.505017276093312e-8)
(h t h t t t h t t h t h h): 1/66444420 (1.505017276093312e-8)
(h t h t h t t t t h t h h): 1/66444420 (1.505017276093312e-8)
(h t h t h t t t h t t h h): 1/66444420 (1.505017276093312e-8)

var : winner-len
2: 116556673/265777680 (0.4385495162723973)
3: 9690121/37968240 (0.25521649146760556)
4: 10258891/66444420 (0.15439808188558196)
5: 1750891/22148140 (0.07905363610668886)
6: 6204/158201 (0.03921593415970822)
...
9: 21206/5537035 (0.0038298475628201737)
10: 18253/11074070 (0.0016482648204318737)
11: 2392/3322221 (0.0007200002648830406)
12: 1768/5537035 (0.0003193044652959571)
13: 69/5537035 (1.2461543046052626e-5)
mean: 279204561/88592560 (3.1515576590178678)


|#
(define (model3)
  (enumerate #:limit 1e-02
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define coins (list "h" "t"))
   
   ; """
   ; If the sequence (Heads, Tails) occurs on your coin before the sequence (Heads, Heads) occurs
   ; on your friends coin, then you win. If the sequence (Heads, Heads) occurs on your friends coin
   ; before (Heads, Tails) occurs on your coin, then your friend wins.
   ; If the sequence (Heads, Tails) occurs on your coin in the same turn that (Heads, Heads) occurs
   ; on your friends coin, then both of you start over (e.g. there are no ties).
   ; """
   (define (f a b)
     (define len (length a))
     (define a-toss (uniform-draw coins))
     (define b-toss (uniform-draw coins))
     ; we need at least two coins
     (if (< len 2) 
         (f (append a (list a-toss)) (append b (list b-toss))) 
         (let ([a-patt (list-slice a (- len 2))]
               [b-patt (list-slice b (- len 2))]
               [pos-a (index-of-sub a '("h" "t"))]
               [pos-b (index-of-sub b '("h" "h"))])
           (cond
             [(and (equal? a-patt '("h" "t")) (equal? b-patt '("h" "h"))) (f '() '())] ; tie -> restart
             [(< pos-a pos-b) (list "a" a)] ; a (you) win             
             [(< pos-b pos-a) (list "b" b)] ; b (your friend) win
             [else (f (append a (list a-toss)) (append b (list b-toss)))]))))

  
   (define res (f '() '()))
   (define winner (list-ref res 0))
   (define winner-list (list-ref res 1))
   (define winner-len (length winner-list))
   
   ; (define the-last (take-last winner-list 2))   
   (list winner
         winner-list
         winner-len
         )
   

   )
)

; #|
(displayln "Model 3 (this takes a while...)")
(show-marginals (model3)
                (list  "winner"
                       "winner-list"
                       "winner-len"
                     )
                    #:num-samples 100000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )

; |#
