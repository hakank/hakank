#| 

  n heads in a row after k tosses in Racket/Gamble 

  Inpired by (and extended)
  https://www.reddit.com/r/learnmath/comments/1gi6kuf/whats_the_probability_to_get_10_heads_in_a_row_if/
  """
  What’s the probability to get 10 heads in a row if you flip a coin 500 times? How would you 
  generalize the method? 

  Hi!

  I came across a short where a person tried flipping a coin until they got 10 heads in 
  a row. It took them 486 attempts. I do not care about how real or fake the video is, but 
  it did made me ask a question. How would you calculate the chances of getting 10 heads in 
  a row in 486 flips, and more generally, how to calculate the probability of x events(with 
  a probability of say p, to happen independently) happening y times in a row with n cases?
  """

  The probability of getting a run of 10 heads in 486 flips is 
  > (* 1.0 (probability-of-run-size 486 1/2 10))
  0.20910597145616058

  The mean value of tosses needed to get 10 heads in a row is
  > (expected-tosses-needed-for-n-heads 10))
  2046


  Here I explore a related question:
  * What is the probability of getting n heads in a row after exact k tosses with probability 
    p of getting heads?

  For practical/computational purposes we have to give a some maximum limit, e.g. 
  m = 20*expected value of n tosses in a row.

  Thus, the PDF etc can be used in two cases:
  - The probability of n heads after exactly k tosses in max number of tosses
  - The probability of n heads after exactly k tosses (ignoring - kind of - max number of tosses)

  The PDF is
     (prob-n-heads-after-k-in-max-m-tosses-pdf p m n k)
  p: probability of tossing heads
  m: maximum number of tosses
  n: the number of heads in a row to study
  k: the number of tosses when the n number of heads occurred.


  Let's use a simpler example to explore this: 
  What is the probability of getting 3 heads in a rows?

  First, the (total) probability of getting 3 heads in a row in max 21 tosses is
  about 0.804:
  > (probability-of-run-size 21 1/2 3))
  210801/262144 (0.8041419982910156)

  We can ask questions such as:
  What is the probability of getting a row of 3 heads come _after exactly_ 7 tosses (with 
  max 21 tosses)?

  p: probability of tossing heads: 1/2
  m: maximum number of tosses: 21
  n: the number of heads in a row to study: 3
  k: the number of tosses when the n number of heads occurred: 7

  > (prob-n-heads-after-k-in-max-m-tosses-pdf 1/2 21 3 7)
  7/128 0.0546875

  Here is the PDF for 3 heads in max 21 tosses (and p=1/2)
  (0 0 0)
  (1 0 0)
  (2 0 0)
  (3 1/8 0.125)
  (4 1/16 0.0625)
  (5 1/16 0.0625)
  (6 1/16 0.0625)
  (7 7/128 0.0546875)
  (8 13/256 0.05078125)
  (9 3/64 0.046875)
  (10 11/256 0.04296875)
  (11 81/2048 0.03955078125)
  (12 149/4096 0.036376953125)
  (13 137/4096 0.033447265625)
  (14 63/2048 0.03076171875)
  (15 927/32768 0.028289794921875)
  (16 1705/65536 0.0260162353515625)
  (17 49/2048 0.02392578125)
  (18 721/32768 0.022003173828125)
  (19 10609/524288 0.020235061645507813)
  (20 19513/1048576 0.018609046936035156)
  (21 17945/1048576 0.017113685607910156)
  (22 51343/262144 0.19585800170898438)
  sum: 1

  Note: The last entry (k=22) is the probability of not getting 3 heads in a rows using
        max 21 tosses.

  Here is the CDF:
  (0 0 0)
  (1 0 0)
  (2 0 0)
  (3 1/8 0.125)
  (4 3/16 0.1875)
  (5 1/4 0.25)
  (6 5/16 0.3125)
  (7 47/128 0.3671875)
  (8 107/256 0.41796875)
  (9 119/256 0.46484375)
  (10 65/128 0.5078125)
  (11 1121/2048 0.54736328125)
  (12 2391/4096 0.583740234375)
  (13 79/128 0.6171875)
  (14 1327/2048 0.64794921875)
  (15 22159/32768 0.676239013671875)
  (16 46023/65536 0.7022552490234375)
  (17 47591/65536 0.7261810302734375)
  (18 49033/65536 0.7481842041015625)
  (19 402873/524288 0.7684192657470703)
  (20 825259/1048576 0.7870283126831055)
  (21 210801/262144 0.8041419982910156)
  (22 1 1.0)

  Some quantiles:
  0.0001: 3
  0.001: 3
  0.01: 3
  0.05: 3
  0.25: 5
  0.3: 6
  0.4: 8
  0.5: 10
  0.75: 19
  0.8: 21
  0.84: 22
  0.9: 22
  0.95: 22
  0.99: 22
  0.999: 22
  0.9999: 22
  0.99999: 22

  Note that everything larger than 21 (max number of tosses), the number 21+1=22 is
  used. 

  For the more general question of "all" probabilities of a specific n, then just 
  make m (much) larger, for example m=20*expectation (n=3) = 20*14 = 280:

  PDF
  (0 0)
  (1 0)
  (2 0)
  (3 0.125)
  (4 0.0625)
  (5 0.0625)
  (6 0.0625)
  (7 0.0546875)
  (8 0.05078125)
  (9 0.046875)
  (10 0.04296875)
  ...
  (78 0.00014442822418039307)
  (79 0.00013282245990704932)
  (80 0.00012214929565099986)
  (81 0.0001123337908248114)
  (82 0.00010330702681353683)
  (83 9.500562306934624e-5)
  (84 8.737129209115876e-5)
  (85 8.035043016460804e-5)
  ...
  276 9.042796130921462e-12)
  (277 8.316147576852854e-12)
  (278 7.647890046255897e-12)
  (279 7.033331433706345e-12)
  (280 6.4681566755237534e-12)
  (281 7.402498107604121e-11)
  sum: 1

  CDF
  (0 0)
  (1 0)
  (2 0)
  (3 0.125)
  (4 0.1875)
  (5 0.25)
  (6 0.3125)
  (7 0.3671875)
  (8 0.41796875)
  (9 0.46484375)
  (10 0.5078125)
  ...
  (78 0.9983470875709834)
  (79 0.9984799100308904)
  (80 0.9986020593265414)
  (81 0.9987143931173663)
  (82 0.9988177001441798)
  (83 0.9989127057672491)
  (84 0.9990000770593404)
  (85 0.9990804274895049)
  ...
  (275 0.9999999998874667)
  (276 0.9999999998965095)
  (277 0.9999999999048257)
  (278 0.9999999999124736)
  (279 0.9999999999195068)
  (280 0.999999999925975)
  (281 1.0)

  Quantile
  0.0001: 3
  0.001: 3
  0.01: 3
  0.05: 3
  0.25: 5
  0.3: 6
  0.4: 8
  0.5: 10
  0.75: 19
  0.8: 21
  0.84: 24
  0.9: 30
  0.95: 38
  0.99: 57
  0.999: 84
  0.9999: 112
  0.99999: 139
  0.999999: 167


  See distributions_test.rkt for some more tests.  

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; 0.20910597145616058
(* 1.0 (probability-of-run-size 486 1/2 10))

#|
  Probability of getting n heads in a row in 21 tosses
(0 1 1.0)
(1 2097151/2097152 0.9999995231628418)
(2 2068495/2097152 0.986335277557373)
(3 210801/262144 0.8041419982910156)
(4 521063/1048576 0.49692440032958984)
(5 34425/131072 0.26264190673828125)
(6 271623/2097152 0.12951993942260742)
(7 4061/65536 0.0619659423828125)
(8 1915/65536 0.0292205810546875)
(9 14327/1048576 0.013663291931152344)
(10 13311/2097152 0.006347179412841797)
(11 3/1024 0.0029296875)
(12 11/8192 0.0013427734375)
(13 5/8192 0.0006103515625)
(14 9/32768 0.000274658203125)
(15 1/8192 0.0001220703125)
(16 7/131072 5.340576171875e-5)
(17 3/131072 2.288818359375e-5)
(18 5/524288 9.5367431640625e-6)
(19 1/262144 3.814697265625e-6)
(20 3/2097152 1.430511474609375e-6)
(21 1/2097152 4.76837158203125e-7)

|#
(for ([n (range 0 22)])
  (let ([v (probability-of-run-size 21 1/2 n)])
    (show2 n v (* 1.0 v))
    ))


(newline)

#|
  Random values, PDF, CDF, and quantiles

Dist: (9 5 7 3 5 5 4 5 9 3)
Dist mean (10000 samples): 5.9651
Theoretical Mean: 6
Histogram:
 2: 2556 ################################################################################ (0.255 / 0    )
 3: 1164 ##################################### (0.116 / 0.255)
 4: 1245 ####################################### (0.124 / 0.372)
 5:  946 ############################## (0.094 / 0.496)
 6:  745 ######################## (0.074 / 0.591)
 7:  629 #################### (0.062 / 0.665)
 8:  538 ################# (0.053 / 0.728)
 9:  434 ############## (0.043 / 0.782)
10:  353 ############ (0.035 / 0.825)
11:  268 ######### (0.026 / 0.861)
12:  218 ####### (0.021 / 0.887)
13:  167 ###### (0.016 / 0.909)
14:  130 ##### (0.013 / 0.926)
15:  120 #### (0.012 / 0.939)
16:   94 ### (0.009 / 0.951)
17:   64 ### (0.006 / 0.960)
18:   56 ## (0.005 / 0.967)
19:   59 ## (0.005 / 0.972)
20:   45 ## (0.004 / 0.978)
21:   22 # (0.002 / 0.983)
22:  147 ##### (0.014 / 0.985)

PDF
(0 0 0)
(1 0 0)
(2 1/4 0.25)
(3 1/8 0.125)
(4 1/8 0.125)
(5 3/32 0.09375)
(6 5/64 0.078125)
(7 1/16 0.0625)
(8 13/256 0.05078125)
(9 21/512 0.041015625)
(10 17/512 0.033203125)
(11 55/2048 0.02685546875)
(12 89/4096 0.021728515625)
(13 9/512 0.017578125)
(14 233/16384 0.01422119140625)
(15 377/32768 0.011505126953125)
(16 305/32768 0.009307861328125)
(17 987/131072 0.00753021240234375)
(18 1597/262144 0.006092071533203125)
(19 323/65536 0.0049285888671875)
(20 4181/1048576 0.003987312316894531)
(21 6765/2097152 0.0032258033752441406)
(22 28657/2097152 0.013664722442626953)
sum: 1

CDF
(0 0 0)
(1 0 0)
(2 1/4 0.25)
(3 3/8 0.375)
(4 1/2 0.5)
(5 19/32 0.59375)
(6 43/64 0.671875)
(7 47/64 0.734375)
(8 201/256 0.78515625)
(9 423/512 0.826171875)
(10 55/64 0.859375)
(11 1815/2048 0.88623046875)
(12 3719/4096 0.907958984375)
(13 3791/4096 0.925537109375)
(14 15397/16384 0.93975830078125)
(15 31171/32768 0.951263427734375)
(16 7869/8192 0.9605712890625)
(17 126891/131072 0.9681015014648438)
(18 255379/262144 0.9741935729980469)
(19 256671/262144 0.9791221618652344)
(20 1030865/1048576 0.9831094741821289)
(21 2068495/2097152 0.986335277557373)
(22 1 1.0)

Quantile
0.0001: 2
0.001: 2
0.01: 2
0.05: 2
0.25: 2
0.3: 3
0.4: 4
0.5: 4
0.75: 8
0.8: 9
0.84: 10
0.9: 12
0.95: 15
0.99: 22
0.999: 22
0.9999: 22
0.99999: 22
0.999999: 22

|#
(let ([p 1/2]
      [m 21]
      ; [m 280]      
      [n 3])
  (show2 "p" p "m" m "n" n)
  (show "Dist" (for/list ([i 10]) (prob-n-heads-after-k-in-max-m-tosses-dist p m n)))
  (show "Dist mean (10000 samples)" (* 1.0 (avg (for/list ([i 10000]) (prob-n-heads-after-k-in-max-m-tosses-dist p m n)))))
  (show "Theoretical Mean" (expected-tosses-needed-for-n-heads n))  
  (displayln "Histogram:")
  (show-histogram (for/list ([i 10000]) (prob-n-heads-after-k-in-max-m-tosses-dist p m n)))
  (newline)
  (displayln "PDF")
  (show "sum" (sum
               (for/list ([k (range 0 (+ 2 m))])
                 (let ([v (prob-n-heads-after-k-in-max-m-tosses-pdf p m n k)])
                   (if (> m 30) 
                       (show2 k (* 1.0 v) )
                       (show2 k v (* 1.0 v) ))
                   v
                   ))))
  (newline)
  (flush-output)
  (displayln "CDF")
  (for ([k (range 0 (+ 2 m))])
    (let ([v (prob-n-heads-after-k-in-max-m-tosses-cdf p m n k)])
      (if (> m 30) 
          (show2 k (* 1.0 v) )
          (show2 k v (* 1.0 v) ))
      
      ))
  (newline)
  (flush-output)
  (displayln "Quantile")
  (for ([q '(0.0001 0.001 0.01 0.05 0.25 0.3 0.4 0.5 0.75 0.80 0.84 0.9 0.95 0.99 0.999 0.9999 0.99999 0.999999)])
    (show q (prob-n-heads-after-k-in-max-m-tosses-quantile p m n q))
    (flush-output)
    )

  (flush-output)
  )

(newline)

; 0.20910597145616058
(show "(prob-n-heads-after-k-in-max-m-tosses-cdf 1/2 486 10 486))" (* 1.0 (prob-n-heads-after-k-in-max-m-tosses-cdf 1/2 486 10 486)))
(newline)

; 2046
(show "mean value of getting 10 heads in a row" (expected-tosses-needed-for-n-heads 10)) 
(newline)

; 167
(show "0.999999 quantile for 3 heads in a row"
      (prob-n-heads-after-k-in-max-m-tosses-quantile 1/2 (* 20 (expected-tosses-needed-for-n-heads 3)) 3 0.999999))

#|
  m for 20*E(n)
  (1 2 est: 40)
  (2 6 est: 120)
  (3 14 est: 280)
  (4 30 est: 600)
  (5 62 est: 1240)
  (6 126 est: 2520)
  (7 254 est: 5080)
  (8 510 est: 10200)
  (9 1022 est: 20440)
  (10 2046 est: 40920)
 
|#
(displayln "m for 20*mean(n)")
(for ([n (range 1 11)])
  (show2 n (expected-tosses-needed-for-n-heads n) "est:" (* 20 (expected-tosses-needed-for-n-heads n)))
  )

(newline)

#|
; This is too slow. See a faster model below.
(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 10)
   (define limit 21)
   
   (define (f a)
     (let ([len (length a)])
       (if (or
            (>= len limit)
            (and (>= (length a) n) (equal? (take a n) (ones-list n 1))))
           (reverse a)
           (f (cons (bernoulli 1/2) a))))
     )

   (define res (f '()))  
   (define len (length res))
   (define p (< len limit))
   (list len
         p)
   )
)

(show-marginals (model)
                (list  "len"
                       "p"

                     )
                    #:num-samples 100000
                    #:truncate-output 10
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )

|#

#|
  Faster version, using a counter

  * n=10, limit=486, use-limit?=#t (importance-sampler)

  variable : n
  10: 0.9999999999999999

  variable : limit
  486: 0.9999999999999999

  variable : len
  487: 0.7914599999999998
  134: 0.0007099999999999999
  10: 0.0006999999999999999
  126: 0.0006699999999999999
  152: 0.0006599999999999999
  21: 0.0006299999999999999
  42: 0.0006299999999999999
  318: 0.0006299999999999999
  79: 0.0006199999999999999
  ...
  415: 0.0002999999999999999
  449: 0.0002999999999999999
  464: 0.0002999999999999999
  38: 0.00028999999999999995
  445: 0.00028999999999999995
  191: 0.0002799999999999999
  341: 0.0002799999999999999
  420: 0.0002799999999999999
  107: 0.0002599999999999999
  429: 0.00022999999999999995
  mean: 435.5692800000006
  HPD interval (0.84): 362..487
  HPD interval (0.99): 30..487
  HPD interval (0.999999): 10..487

  variable : theoretical-mean
  2046: 0.9999999999999999

  variable : theoretical-p
  mean: 0.20910597145616056


  * enumerate for n=4 limit=21

  variable : len
  22: 527513/1048576 (0.5030755996704102)
  4: 1/16 (0.0625)
  5: 1/32 (0.03125)
  6: 1/32 (0.03125)
  7: 1/32 (0.03125)
  8: 1/32 (0.03125)
  9: 15/512 (0.029296875)
  10: 29/1024 (0.0283203125)
  11: 7/256 (0.02734375)
  12: 27/1024 (0.0263671875)
  13: 13/512 (0.025390625)
  14: 401/16384 (0.02447509765625)
  15: 773/32768 (0.023590087890625)
  16: 745/32768 (0.022735595703125)
  17: 359/16384 (0.02191162109375)
  18: 173/8192 (0.0211181640625)
  19: 10671/524288 (0.020353317260742188)
  20: 20569/1048576 (0.019616127014160156)
  21: 1239/65536 (0.0189056396484375)
  mean: 4355065/262144 (16.61325454711914)
  HPD interval (0.84): 8..22
  HPD interval (0.99): 4..22
  HPD interval (0.999999): 4..22

  variable : p
  #f: 527513/1048576 (0.5030755996704102)
  #t: 521063/1048576 (0.49692440032958984)
  mean: 521063/1048576 (0.49692440032958984)

  * enumerate for n=3 limit=21

  variable : len
  22: 51343/262144 (0.19585800170898438)
  3: 1/8 (0.125)
  4: 1/16 (0.0625)
  5: 1/16 (0.0625)
  6: 1/16 (0.0625)
  7: 7/128 (0.0546875)
  8: 13/256 (0.05078125)
  9: 3/64 (0.046875)
  10: 11/256 (0.04296875)
  11: 81/2048 (0.03955078125)
  12: 149/4096 (0.036376953125)
  13: 137/4096 (0.033447265625)
  14: 63/2048 (0.03076171875)
  15: 927/32768 (0.028289794921875)
  16: 1705/65536 (0.0260162353515625)
  17: 49/2048 (0.02392578125)
  18: 721/32768 (0.022003173828125)
  19: 10609/524288 (0.020235061645507813)
  20: 19513/1048576 (0.018609046936035156)
  21: 17945/1048576 (0.017113685607910156)
  mean: 12329679/1048576 (11.758498191833496)
  HPD interval (0.84): 4..22
  HPD interval (0.99): 3..22
  HPD interval (0.999999): 3..22

  variable : p
  #t: 210801/262144 (0.8041419982910156)
  #f: 51343/262144 (0.19585800170898438)
  mean: 210801/262144 (0.8041419982910156)


  * Skipping the limit (use-limit? #f) for n=10 gives a wide range of
    extremes. Using 10000 samples gives that it might take as long as 20579 tosses 
    (in theory in can take "forever").
    Note that the PDF, CDF, quantiles above handles this as well, just pick a large
    enough m, e.g. 20*expected value

  variable : n
  10: 0.9999999999999912

  variable : limit
  2046: 0.9999999999999912

  variable : len
  10: 0.0012000000000000643
  86: 0.0012000000000000643
  304: 0.0012000000000000643
  351: 0.0011000000000000593
  613: 0.0011000000000000593
  ...
  8111: 0.00010000000000000537
  8132: 0.00010000000000000537
  8190: 0.00010000000000000537
  8179: 0.00010000000000000537
  8161: 0.00010000000000000537
  mean: 2076.12030000011
  HPD interval (0.84): 10..3764
  HPD interval (0.99): 10..9264
  HPD interval (0.999999): 10..20579

  variable : theoretical-mean
  2046: 0.9999999999999912

  * For n=3 (use-limit #f)

  variable : n
  3: 1.0

  variable : limit
  21: 1.0

  variable : len
  3: 0.12444000000000004
  4: 0.06337000000000002
  6: 0.06299000000000003
  5: 0.06272000000000003
  7: 0.05363000000000002
  8: 0.050580000000000014
  9: 0.046010000000000016
  10: 0.04362000000000001
  11: 0.04010000000000001
  12: 0.035700000000000016
  13: 0.03382000000000002
  14: 0.03027000000000001
  ...
  96: 1.0000000000000004e-5
  101: 1.0000000000000004e-5
  102: 1.0000000000000004e-5
  105: 1.0000000000000004e-5
  106: 1.0000000000000004e-5
  109: 1.0000000000000004e-5
  120: 1.0000000000000004e-5
  121: 1.0000000000000004e-5
  mean: 13.980020000000001
  HPD interval (0.84): 3..24
  HPD interval (0.99): 3..57
  HPD interval (0.999999): 3..150

  variable : theoretical-mean
  14: 1.0

|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler ; use this if use-limit #f
   ; mh-sampler

   (define prob 1/2)
   
   ; (define use-limit? #f)
   (define use-limit? #t)
   
   (define n 10)

   (define theoretical-mean (expected-tosses-needed-for-n-heads n))

   (define limit 486)
   ;; (define limit theoretical-mean)

   ; The theoretical probability of getting n heads in atmost limit tosses
   (define theoretical-p (probability-of-run-size limit prob n))
   
   ; Simulate the number of tosses required to get n heads
   (define (f c h )
     (if (or (and use-limit? (> c limit))
             (= h n)
             )
          c
          (f (add1 c) (if (flip prob) (add1 h) 0))))
  
   (define len (f 0 0))
  
   (define p (<= len limit))

   (list n
         limit
         len
         theoretical-mean         
         p
         theoretical-p
         )

   )
)

(show-marginals (model2)
                (list  "n"
                       "limit"
                       "len"
                       "theoretical-mean"                       
                       "p"
                       "theoretical-p"
                     )
                    #:num-samples 100000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84 0.99 0.999999)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


