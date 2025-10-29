#| 

  Zener test in Racket/Gamble 

  From Matt Parker: "We have statistical evidence that people are mildly psychic"
  https://www.youtube.com/watch?v=VwIKKBL4ldQ

  Zener cards consists of 25 cards among 5 different symbols:
    circle, plus, 3 wayvy lines, square, star

  Collecting data for getting 25 Zener cards correct, they found an anomaly
  that indicated that the number of correct guesses was slightly more than expected.

  What was the cause of it? Are people psychic?

  Nope, it was premature stopping: people stopped before end if the got too few
  correct guesses (or was bored, or they answered a phone, or the computer broke down,
  etc). And the site didn't registered those trials.

  Here is a model including some probability (0.03) that a person ends the session 
  at each guess.


  * importance-sampler 1000000 samples
variable : len
25: 0.46681900000000015
0: 0.02994500000000001
1: 0.02900400000000001
2: 0.028177000000000015
3: 0.02733400000000001
4: 0.026753000000000006
5: 0.02560300000000001
6: 0.024948000000000005
7: 0.024473000000000012
8: 0.023447000000000013
9: 0.02282500000000001
10: 0.02218200000000001
11: 0.02175700000000001
12: 0.020877000000000007
13: 0.020177000000000004
14: 0.019612000000000008
15: 0.01892100000000001
16: 0.018605000000000007
17: 0.017671000000000006
18: 0.01749100000000001
19: 0.01681400000000001
20: 0.016320000000000008
21: 0.015564000000000005
22: 0.015435000000000006
23: 0.014799000000000005
24: 0.014447000000000005
mean: 17.231527000000003
HPD interval (0.84): 5..25
HPD interval (0.9): 3..25
HPD interval (0.95): 1..25
HPD interval (0.99): 0..25
HPD interval (0.999): 0..25
HPD interval (0.999999): 0..25
Histogram:
 0:  30179 ###### (0.030 / 0    )
 1:  28726 ##### (0.028 / 0.030)
 2:  28283 ##### (0.028 / 0.058)
 3:  27300 ##### (0.027 / 0.087)
 4:  26686 ##### (0.026 / 0.114)
 5:  25892 ##### (0.025 / 0.141)
 6:  25198 ##### (0.025 / 0.167)
 7:  24231 ##### (0.024 / 0.192)
 8:  23324 #### (0.023 / 0.216)
 9:  22950 #### (0.022 / 0.239)
10:  22141 #### (0.022 / 0.262)
11:  21522 #### (0.021 / 0.284)
12:  21010 #### (0.021 / 0.306)
13:  20443 #### (0.020 / 0.327)
14:  19574 #### (0.019 / 0.347)
15:  19026 #### (0.019 / 0.367)
16:  18487 #### (0.018 / 0.386)
17:  18033 #### (0.018 / 0.404)
18:  17335 ### (0.017 / 0.423)
19:  16733 ### (0.016 / 0.440)
20:  16005 ### (0.016 / 0.457)
21:  15922 ### (0.015 / 0.473)
22:  15216 ### (0.015 / 0.489)
23:  14892 ### (0.014 / 0.504)
24:  14259 ### (0.014 / 0.519)
25: 466633 ################################################################################ (0.466 / 0.533)

variable : num-correct
4: 0.13899900000000004
3: 0.13719100000000006
0: 0.13521600000000011
2: 0.12774300000000005
1: 0.12533000000000005
5: 0.12448300000000004
6: 0.09411000000000003
7: 0.060379000000000016
8: 0.03276600000000001
9: 0.015183000000000006
10: 0.005935000000000002
11: 0.0019040000000000005
12: 0.0005710000000000002
13: 0.00015000000000000004
14: 3.400000000000001e-5
15: 4.0000000000000015e-6
16: 2.0000000000000008e-6
mean: 3.4465520000000005
HPD interval (0.84): 0..6
HPD interval (0.9): 0..7
HPD interval (0.95): 0..8
HPD interval (0.99): 0..9
HPD interval (0.999): 0..11
HPD interval (0.999999): 0..15
Histogram:
 0: 135689 ############################################################################## (0.135 / 0    )
 1: 124846 ######################################################################## (0.124 / 0.135)
 2: 127910 ########################################################################## (0.127 / 0.260)
 3: 137082 ############################################################################### (0.137 / 0.388)
 4: 139834 ################################################################################ (0.139 / 0.525)
 5: 123567 ####################################################################### (0.123 / 0.665)
 6:  93778 ###################################################### (0.093 / 0.788)
 7:  60752 ################################### (0.060 / 0.882)
 8:  32672 ################### (0.032 / 0.943)
 9:  15289 ######### (0.015 / 0.976)
10:   5852 #### (0.005 / 0.991)
11:   1973 ## (0.001 / 0.997)
12:    579 # (0.000 / 0.999)
13:    138 # (0.000 / 0.999)
14:     30 # (3e-5  / 0.999)
15:      9 # (9e-6  / 0.999)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")



(define (model)
  (; enumerate #:limit 1e-01
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 25)
   (define p-quit 0.03)

   ; 0s 1s 2s 3s 4s
   (define cards (flatten (for/list ([i n]) (rep 5 i))))
   ; (define shuffled (draw-without-replacement cards n))

   (define (f a c)
     (if (or (= c n) (flip p-quit))
         (list a c)
         (f (append a (list (uniform-draw (range 5)))) (add1 c))))

  
   (define res (f '() 0))
   (define a (first res))
   (define len (second res))   
   (define num-correct (for/sum ([i len]) (b2i (= (list-ref a i) (list-ref cards i)))))
  
   (list len
         num-correct
         )

   )
)

(show-marginals (model)
                (list  "len"
                       "num-correct"
                       )
                #:num-samples 1000000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.9 0.95 0.99 0.999 0.999999)
                #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )
  
  
