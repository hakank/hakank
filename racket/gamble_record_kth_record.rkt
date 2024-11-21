#| 

  Kth record for permutations in Racket/Gamble 

  What is the k'th record for a permutation of m values?

  The theoretical values for k=2..4

  k: 2
  k_record_pdf n k: (0 1/2 1/6 1/12 1/20 1/30 1/42 1/56 1/72 1/90 1/110 1/132 1/156 1/182 1/210 1/240 1/272 1/306 1/342 1/380)
  k_record_pdf n k: (0 0.5 0.16666666666666666 0.08333333333333333 0.05 0.03333333333333333 0.023809523809523808 0.017857142857142856 0.013888888888888888 0.011111111111111112 0.00909090909090909 0.007575757575757576 0.00641025641025641 0.005494505494505495 0.004761904761904762 0.004166666666666667 0.003676470588235294 0.0032679738562091504 0.0029239766081871343 0.002631578947368421)

  k: 3
  k_record_pdf n k: (0 0 1/6 1/8 11/120 5/72 137/2520 7/160 121/3360 761/25200 7129/277200 671/30240 83711/4324320 6617/388080 1145993/75675600 1171733/86486400 1195757/98017920 143327/12972960 42142223/4190266080 751279/81681600)
  k_record_pdf n k: (0 0 0.16666666666666666 0.125 0.09166666666666666 0.06944444444444445 0.054365079365079366 0.04375 0.03601190476190476 0.0301984126984127 0.02571789321789322 0.02218915343915344 0.01935818810818811 0.017050608122036695 0.0151434940720655 0.01354817636067636 0.012199371298636005 0.011048133964800632 0.010057171118832626 0.00919765283735872)

  k: 4
  k_record_pdf n k: (0 0 0 1/24 1/20 7/144 5/112 29/720 469/12960 29531/907200 1303/44352 16103/604800 190553/7862400 128977/5821200 9061/445500 30946717/1651104000 13215487/762361600 58433327/3632428800 344499373/23005382400 784809203/56137536000)
  k_record_pdf n k: (0 0 0 0.041666666666666664 0.05 0.04861111111111111 0.044642857142857144 0.04027777777777778 0.036188271604938274 0.03255180776014109 0.029378607503607504 0.026625330687830687 0.024235983923483924 0.022156428227856798 0.02033894500561167 0.018743045259414306 0.01733493266187594 0.016086571882702835 0.01497472926161836 0.013980114891398155)


  See gamble_record_number_of_records.rkt for the number of records.

  * n=10, k=2, draw-without-replacement, enumerate
  variable : num-records
  3: 1303/4032 (0.32316468253968256)
  2: 7129/25200 (0.2828968253968254)
  4: 4523/22680 (0.1994268077601411)
  1: 1/10 (0.1)
  5: 19/256 (0.07421875)
  6: 3013/172800 (0.017436342592592594)
  7: 1/384 (0.0026041666666666665)
  8: 29/120960 (0.00023974867724867725)
  9: 1/80640 (1.240079365079365e-5)
  10: 1/3628800 (2.755731922398589e-7)
  mean: 7381/2520 (2.9289682539682538)
  HPD interval (0.84): 1..4
  HPD interval (0.99): 1..6
  HPD interval (0.99999): 1..9

  variable : k
  2: 1 (1.0)
  mean: 2 (2.0)
  HPD interval (0.84): 2..2
  HPD interval (0.99): 2..2
  HPD interval (0.99999): 2..2

  variable : kth record-ix (1-based)
  2: 1/2 (0.5)
  3: 1/6 (0.16666666666666666)
  0: 1/10 (0.1)
  4: 1/12 (0.08333333333333333)
  5: 1/20 (0.05)
  6: 1/30 (0.03333333333333333)
  7: 1/42 (0.023809523809523808)
  8: 1/56 (0.017857142857142856)
  9: 1/72 (0.013888888888888888)
  10: 1/90 (0.011111111111111112)
  mean: 7381/2520 (2.9289682539682538)
  HPD interval (0.84): 1..4
  HPD interval (0.99): 1..10
  HPD interval (0.99999): 1..10


  * n=10, k=3, draw-without-replacement, enumerate

  Theoretical for kth-record

  enumerate:
  variable : num-records
  3: 1303/4032 (0.32316468253968256)
  2: 7129/25200 (0.2828968253968254)
  4: 4523/22680 (0.1994268077601411)
  1: 1/10 (0.1)
  5: 19/256 (0.07421875)
  6: 3013/172800 (0.017436342592592594)
  7: 1/384 (0.0026041666666666665)
  8: 29/120960 (0.00023974867724867725)
  9: 1/80640 (1.240079365079365e-5)
  10: 1/3628800 (2.755731922398589e-7)
  mean: 7381/2520 (2.9289682539682538)
  HPD interval (0.84): 1..4
  HPD interval (0.99): 1..6
  HPD interval (0.99999): 1..9

  variable : k
  3: 1 (1.0)
  mean: 3 (3.0)

  variable : kth record-ix (1-based)
  0: 9649/25200 (0.3828968253968254)
  3: 1/6 (0.16666666666666666)
  4: 1/8 (0.125)
  5: 11/120 (0.09166666666666666)
  6: 5/72 (0.06944444444444445)
  7: 137/2520 (0.054365079365079366)
  8: 7/160 (0.04375)
  9: 121/3360 (0.03601190476190476)
  10: 761/25200 (0.0301984126984127)
  mean: 6515/2016 (3.2316468253968256)
  HPD interval (0.84): 0..7
  HPD interval (0.99): 0..10
  HPD interval (0.99999): 0..10

  Again, apart for n=1, the probabilities matches completely.
  Note that there's no values for 1 or 2 (since we are checking k=3 which 
  requires number of records >= 3).


  * For n=100, k=3 draw-without-replacement, (importance-sampler, 100000 samples)

  variable : num-records
  5: 0.21100000000000033
  4: 0.19250000000000042
  6: 0.17290000000000053
  3: 0.12720000000000048
  7: 0.11920000000000038
  8: 0.06550000000000021
  2: 0.05220000000000015
  9: 0.0320000000000001
  10: 0.011600000000000034
  1: 0.010100000000000027
  11: 0.004000000000000011
  12: 0.0013000000000000036
  13: 0.0005000000000000013
  mean: 5.1870000000000145
  HPD interval (0.84): 2..7
  HPD interval (0.99): 1..10
  HPD interval (0.99999): 1..16

  variable : k
  3: 1.0000000000000013
  mean: 3.000000000000004
  HPD interval (0.84): 3..3
  HPD interval (0.99): 3..3
  HPD interval (0.99999): 3..3

  variable : kth record-ix (1-based)
  3: 0.16580000000000042
  4: 0.12020000000000032
  5: 0.08760000000000023
  6: 0.0728000000000002
  0: 0.062300000000000175
  7: 0.053800000000000146
  8: 0.045900000000000135
  9: 0.036600000000000105
  10: 0.030300000000000087
  11: 0.02520000000000007
  12: 0.024100000000000066
  13: 0.01840000000000005
  15: 0.016500000000000046
  ...
  96: 0.0005000000000000014
  98: 0.0005000000000000014
  82: 0.0004000000000000011
  97: 0.0004000000000000011
  99: 0.0004000000000000011
  92: 0.00040000000000000105
  90: 0.00020000000000000055
  100: 0.00010000000000000028
  mean: 12.754900000000033
  HPD interval (0.84): 1..21
  HPD interval (0.99): 1..84
  HPD interval (0.99999): 1..100


  * Note that the theoretical approach is for permutations and continuous   
    distributions. Here is an example using n=10, k=3, (random-integer 10), importance-sampler
    which shows this.

  variable : num-records
  2: 0.3677500000000001
  3: 0.3148500000000001
  1: 0.15506000000000003
  4: 0.13044000000000003
  5: 0.028690000000000004
  6: 0.0030500000000000006
  7: 0.00016
  mean: 2.5197400000000005
  HPD interval (0.84): 1..4
  HPD interval (0.99): 1..5
  HPD interval (0.99999): 1..8

  variable : kth record-ix (1-based)
  1: 0.5228100000000001
  3: 0.12158000000000002
  4: 0.09872000000000003
  5: 0.07344
  6: 0.056040000000000006
  7: 0.043530000000000006
  8: 0.03250000000000001
  9: 0.028870000000000003
  10: 0.022510000000000002
  mean: 3.0355100000000004
  HPD interval (0.84): 1..6
  HPD interval (0.99): 1..10
  HPD interval (0.99999): 1..10


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(require (rename-in math/special-functions
                    (log-gamma lgammaf)
                    ))


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 10)
   (define limit n)
   (define k 2) ; the k'th record
   
   ; (define x (draw-without-replacement n (range n))) ; enumerate
   ; (define x (shuffle (range n))) ; importance-sampler

   (define x (for/list ([i n]) (normal 100 15)))
   
   ; Some other distributions, which give other probabilities
   ; (define x (for/list ([i n]) (random-integer limit))) 

   ; (define x (for/list ([i n]) (poisson 10)))
   
   ; (define records (get-records x))
   (define records-ix (get-records-ix x))  
   (define num-records (length records-ix))

   ; The k'th record index
   (define record-ix-k (if (>= num-records k) (add1 (list-ref records-ix (sub1 k))) 0))

   ; The record value
   ; (define record-1 (first records))
   ; (define record-k (if (>= num-records k) (list-ref records (sub1 k)) 0))   
 
   (list num-records
         k
         record-ix-k
         ; records-ix
         ; records
         )
   

   )
)

(show-marginals (model)
                (list  "num-records"
                       "k"
                       "kth record-ix (1-based)"                       
                       "records-ix"
                       "kth record (value)"
                       "1st record"
                       "2nd record"
                       "3rd record"                       
                       "records"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.99 0.99999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


