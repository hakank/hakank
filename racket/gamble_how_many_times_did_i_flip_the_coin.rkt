#| 

  How many times did I flip the coin? in Racket/Gamble 

  From Pascal Bercker "How Many Times Was This Coin Flipped? Beta-Binomial Updating With a Bayesian Network"
  https://medium.com/@pbercker/how-many-times-was-this-coin-flipped-beta-binomial-updating-with-a-bayesian-network-7df8750dd6ae
  """
  Problem 16.5 How many times did I flip the coin?

  Suppose that I have a coin and that theta denotes the probability of its landing
  heads up, In each experiment I flip the coin N times, where N is unknown to the
  observer, and record the number of heads obtained, Y. I repeat the experiment 10
  times, each time flipping the coin the same N times, and record
  Y = {9,7,11,10,10,9,8,11,9,11} heads.

  Problem 16.5.1 Write down an expression for the likelihood, stating any 
  assumptions you make.
  Problem 16.5.2 Suppose that the maximum number of times the coin could be
  flipped is 20, and that all other (allowed) values we regard a priori as
  equally probable, Further suppose that, based on previous coin flipping fun,
  we specify a prior theta ~  beta(7,2). Write down the model as a whole 
  (namely the likelihood and prior).

  ... 

  SOURCES: 

  A Student's Guide to Bayesian Statistics, Ben Lambert

  """

  Here are some different setups.

  * exact ordering of Y, prior p~beta(7,2) importance-sampler/10000 (2.9s)
    variable : num-flips
    11: 0.4091297503229964
    12: 0.2681996247036471
    13: 0.1566140951748741
    14: 0.077501085721684
    15: 0.03888708020931942
    16: 0.022904368788247354
    17: 0.011799366312404606
    18: 0.006169085047350706
    19: 0.005314994174247619
    20: 0.0034805495452288567
    mean: 12.271825929503887

    variable : p
    0.8632035682558499: 0.0016683136219938308
    0.8628297903652105: 0.001667954145126571
    0.864510279737031: 0.0016678621669643453
    0.8627294637304068: 0.0016678209458259265
    0.8626106045339893: 0.0016676430917836173
    ...
    0.9961078662280567: 1.4498192820451759e-176
    0.9963698399726035: 1.9827790044745007e-179
    0.9961470062932158: 2.7176227742945213e-198
    0.9972648545689932: 7.154589758494581e-214
    0.9981823774622574: 1.8091318249701756e-232
    mean: 0.783961486368912

    variable : s
    (9 9 10 9 9 7 10 9 7 8): 0.0016683136219938308
    (8 11 10 9 9 9 11 10 10 10): 0.001667954145126571
    (10 10 8 9 10 10 10 8 9 9): 0.0016678621669643453
    (9 10 11 10 7 9 9 9 8 8): 0.0016678209458259265
    (9 11 9 10 9 8 11 10 11 9): 0.0016676430917836173
    ...
    (19 20 20 20 20 19 20 20 20 20): 1.0670968791162752e-159
    (20 20 20 20 20 20 19 20 20 20): 2.110190545101545e-164
    (20 19 20 20 19 20 20 20 19 20): 4.125667726671193e-169
    (19 19 19 19 19 19 19 19 19 19): 1.4518020610496505e-176
    (20 20 20 20 20 20 20 20 20 20): 2.717622774294522e-198

  * sorted Y, prior p~beta(7,2), importance-sampler/10000 (2.8s)
    variable : num-flips
    11: 0.40871743353088225
    12: 0.2869739871950578
    13: 0.15067778037643065
    14: 0.0669619987585619
    15: 0.039721484132730016
    16: 0.018558600919189396
    17: 0.013171435206515236
    18: 0.007184335019029092
    19: 0.004612880821640095
    20: 0.00342006403996904
    mean: 12.23789706465567
 
    variable : p
    0.8632061624741258: 0.0016393667090038478
    0.8641434157060409: 0.0016393109013584355
    0.8631191052641947: 0.0016393037446153244
    0.864256273009434: 0.0016392131910357912
    0.8629723670774699: 0.0016391715316789372
    ...
    0.9962729697592829: 4.067742727019452e-157
    0.9930812384493073: 9.853066263252303e-172
    0.9947932777043124: 1.2616037475685989e-184
    0.995073785837148: 3.865332252131598e-187
    0.9986078138599496: 1.2816744848596094e-244
    mean: 0.78667467003099

    variable : s
    (8 9 9 10 10 10 10 11 11 11): 0.010135849166359526
    (9 9 9 10 10 10 10 10 11 11): 0.009540197190352236
    (8 9 9 10 10 10 10 10 11 11): 0.009396466661888621
    (8 9 9 9 10 10 10 11 11 11): 0.008853676979995511
    (8 8 9 9 10 10 10 10 10 11): 0.008384037377902808
    ...
    (19 19 19 19 19 19 19 19 19 19): 4.2939838054083775e-112
    (19 19 20 20 20 20 20 20 20 20): 3.8586189147732736e-117
    (18 20 20 20 20 20 20 20 20 20): 3.1504507534560465e-126
    (18 19 20 20 20 20 20 20 20 20): 1.1107838953274271e-126
    (20 20 20 20 20 20 20 20 20 20): 9.640920425376973e-130


  So, it seems that they flipped the coin 12 times, and the 
  probability of head is (about) 0.78.


  Using 100000 samples (19.1s)
   variable : num-flips
   11: 0.4117814255493325
   12: 0.2832618600298358
   13: 0.14273152691817992
   14: 0.07445080808487693
   15: 0.037312543804691525
   16: 0.02230915193358844
   17: 0.012833534073354332
   18: 0.007493005810589466
   19: 0.004848934655776454
   20: 0.0029772091398041272
   mean: 12.247911877626558

   variable : p
   0.8636639480017057: 0.00016481227351722807
   0.8636701784916053: 0.0001648122440668828
   0.863676869692189: 0.00016481220578109038
   0.8635913709807604: 0.00016481217630536245
   0.8637015816736932: 0.00016481200461803072
   ...
   0.9970037440726432: 9.915067588944263e-211
   0.9985396606675491: 6.48426028996553e-218
   0.9988740053193558: 1.2546296107716043e-228
   0.9980645611348582: 1.2919001681642355e-230
   0.9988264510642568: 2.1305674053595247e-253
   mean: 0.7853223910063488


  * with prior p ~ beta(1,1), uniform prior, importance-sampler/10000 samples (3.6s)
    It's a little different, N is a little higher about 13 and p is a little lower 0.74.

    variable : num-flips
    11: 0.33053188967096486
    12: 0.22783718563526206
    13: 0.12513587870337203
    14: 0.10121703006869508
    15: 0.0716256723190199
    16: 0.04534616064888475
    17: 0.03428721298551907
    18: 0.0241830009602645
    19: 0.02017140795417361
    20: 0.019664561053845897
    mean: 13.008350123521582

    variable : p
    0.8638219040993034: 0.0036784559883583667
    0.862729036120614: 0.003677106423932985
    0.8648775820374809: 0.0036758558302346055
    0.8655217248081507: 0.003672365873366605
    0.8612545598626483: 0.0036688763131405805
    ...
    0.0003482115157945071: 7.712837629445098e-273
    0.0003204997784141344: 4.738938996861946e-293
    0.0003242029499807893: 4.751456435361743e-297
    0.00026108628471986097: 1.629964701771471e-297
    0.00042753249614656887: 3.4122292509192826e-304
    mean: 0.7454157244144134


  * upper limit 110
    Let's now imagine that we didn't know that the upper range of the
    flip throws was 20, but assumed that it would be less than about 110.
    Then the num-flips has a mean of 12.48 (slightly higher), and 
    mean of p is 0.77, slightly lower.

    variable : num-flips
    11: 0.3909443867221105
    12: 0.26728370096566195
    13: 0.1267417968987739
    14: 0.10953377005267967
    15: 0.038499789970833025
    ...
    96: 1.2871120100398421e-126
    102: 6.28546748779295e-138
    95: 4.287644579132579e-159
    110: 1.0948152301746303e-162
    104: 6.177625741224103e-171
    mean: 12.480996979756148

    variable : p
    0.8638691323772757: 0.017583960455166726
    0.8650056413501003: 0.01756892788565679
    0.867178715198408: 0.017480133485393134
    0.867236118130906: 0.017476711101634467
    0.8570768581570088: 0.017243604132525705
    ...
    0.7351786173197482: 1.691043e-318
    0.7540940176520954: 8.45524e-319
    0.7586205709412672: 8.45524e-319
    0.8433354704529198: 8.45524e-319
    0.8382157838085437: 8.45524e-319
    mean: 0.7737570989332252



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

   (define y '(9 7 11 10 10 9 8 11 9 11))
   ; Let's simplify it a little by sorting it
   ; (define y (sort '(9 7 11 10 10 9 8 11 9 11) <))
   (define n (length y))

   (define num-flips (+ 11 (random-integer 10))) ; 11..20
   ; (define num-flips (+ 11 (random-integer 100))) ; 11..110


   ; (define p (uniform 0 1))
   (define p (beta 7 2))   

   ; (define s (sort (for/list ([t n])
   ;            (for/sum ([i num-flips]) (bernoulli p))) <))

   (define s (for/list ([t n]) (binomial num-flips p)) )
   ; (define s (sort (for/list ([t n]) (binomial num-flips p)) <))   

   (for ([i n])
     ; (observe/fail (eq? (list-ref s i) (list-ref y i))))
     ; much faster
     (observe-sample (binomial-dist num-flips p) (list-ref y i)))

   ; (show2 "num-flips:" num-flips "p:" p)
   
   (list num-flips p)
   
   )
)

(show-marginals (model)
                (list  "num-flips"
                       "p"
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


