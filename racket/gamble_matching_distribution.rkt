#| 

  Matching distribution in Racket/Gamble 

  https://ora.ox.ac.uk/objects/uuid:478fa6d8-bc7f-458d-92f5-7e5d63d9824a/files/mf8a85f79596ca2150078192a755dc020
  """
  Notebook
  The matching distribution
  Tom Fanshawe introduces the probability distribution that forms the basis of 
  elementary card games and tests of extra-sensory perception.

  
  At its most basic, the matching distribution relates to the following scenario. 
  A set of cards numbered 1, 2,…, N is shuffled and dealt in a random order: 
  how many cards occupy their correct position in the sequence (for example, 
  card number 4 being dealt in fourth position)? It arises in other nominal "real
  life" situations when matching up two sets of items: for example, diners in a 
  restaurant with their dinners, hats with hat-wearers, or letters with their 
  intended recipients. 
  ...
  
  Like the Binomial distribution, the matching distribution can take whole number 
  (integer) values between zero and N. However, the lack of independence in the way 
  it is constructed (each card must be used exactly once) distinguishes it from the 
  Binomial and gives rise to some special properties. In its simplest form, with 
  no cards repeated, the probability of observing r correct matches out of N is:

    p(r) = 1/r! * sum(t=0..N-r, (-1)^t/t!

  ... 
  It is impossible to obtain exactly N−1 correct matches. 

  ...

  As N increases, the matching distribution very quickly converges to a special 
  case of another distribution whose mean equals its variance: 
  the Poisson distribution with mean 1.

  ...

  Thus, in an elementary matching task being performed by a human participant, 
  we might wish to see at least four matches, irrespective of N, in order to 
  be persuaded that the participant might not be matching the items completely at
  random. In our earlier example using Zener cards, provided the subject is 
  aware of the composition of the pack, the expected number of matches is 
  five and we would probably need to see at least 10 correct matches before 
  even considering any claims of ESP.5

  """

  Also, see
  Siegrist "Probability Mathematical Statistics and Stochastic Processes",
  section "12.5: The Matching Problem"
  https://stats.libretexts.org/Bookshelves/Probability_Theory/Probability_Mathematical_Statistics_and_Stochastic_Processes_(Siegrist)/12:_Finite_Sampling_Models/12.05:_The_Matching_Problem
  

  See some different runs/models below. 

  Note that for the ESP deck, the distribution is NOT according to this matching
  distribution: it's 5 sets of 5 different cards so correct guessing is easier. 
  See model4 below for more.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|


  Here are some results for the numbers 1..25. 

  PDF:
  (matching_pdf 25 0): 79253545592131482810517/215433472824041472000000 (0.36787944117144233)
  (matching_pdf 25 1): 9923922230666898717143/26976017466662584320000 (0.36787944117144233)
  (matching_pdf 25 2): 39299278806015611311/213653030899875840000 (0.18393972058572117)
  (matching_pdf 25 3): 6563440628747948887/107047688359772160000 (0.061313240195240384)
  (matching_pdf 25 4): 72289643288657479/4716086969696256000 (0.015328310048810096)
  (matching_pdf 25 5): 4282366656425369/1396881535795200000 (0.003065662009762019)
  (matching_pdf 25 6): 92079694567171/180214963568640000 (0.0005109436682936699)
  (matching_pdf 25 7): 138547156531409/1898115498639360000 (7.299195261338141e-5)
  (matching_pdf 25 8): 8178130767479/896332318801920000 (9.123994076672673e-6)
  (matching_pdf 25 9): 15549624751/15338307059712000 (1.0137771196303048e-6)
  (matching_pdf 25 10): 34361893981/338949196185600000 (1.0137771196301731e-7)
  (matching_pdf 25 11): 2467007773/267682954936320000 (9.21615563302073e-9)
  (matching_pdf 25 12): 63633137/82854247956480000 (7.68012969394447e-10)
  (matching_pdf 25 13): 1456321/24650850631680000 (5.907792074843906e-11)
  (matching_pdf 25 14): 1468457/347987841417216000 (4.219851458084165e-12)
  (matching_pdf 25 15): 16481/58583811686400000 (2.8132344969670177e-13)
  (matching_pdf 25 16): 1517/86277977210880000 (1.7582702435085604e-14)
  (matching_pdf 25 17): 163/157596891217920000 (1.0342843614510691e-15)
  (matching_pdf 25 18): 103/1792664637603840000 (5.745636849158504e-17)
  (matching_pdf 25 19): 53/17516894458871808000 (3.0256504727158993e-18)
  (matching_pdf 25 20): 1/6635187295027200000 (1.507116461881127e-19)
  (matching_pdf 25 21): 1/136242512457891840000 (7.339852898771723e-21)
  (matching_pdf 25 22): 1/3372002183332823040000 (2.9655971308168577e-22)
  (matching_pdf 25 23): 1/51704033477769953280000 (1.934085085315342e-23)
  (matching_pdf 25 24): 0 (0)
  (matching_pdf 25 25): 1/15511210043330985984000000 (6.446950284384474e-26)
  CDF:
  (matching_cdf 25 0): 79253545592131482810517/215433472824041472000000 (0.36787944117144233)
  (matching_cdf 25 1): 671324150898054913218497/912424120195940352000000 (0.7357588823428847)
  (matching_cdf 25 2): 14265638206583666905893049/15511210043330985984000000 (0.9196986029286058)
  (matching_cdf 25 3): 15216680753689244699619349/15511210043330985984000000 (0.9810118431238462)
  (matching_cdf 25 4): 15454441390465639148050349/15511210043330985984000000 (0.9963401531726563)
  (matching_cdf 25 5): 15501993517820918037739079/15511210043330985984000000 (0.9994058151824183)
  (matching_cdf 25 6): 15509918872380131186011679/15511210043330985984000000 (0.999916758850712)
  (matching_cdf 25 7): 5170350355296196735739593/5170403347776995328000000 (0.9999897508033253)
  (matching_cdf 25 8): 5170397530025715861603193/5170403347776995328000000 (0.999998874797402)
  (matching_cdf 25 9): 2585201385831164548925159/2585201673888497664000000 (0.9999998885745217)
  (matching_cdf 25 10): 7755604943738985632110397/7755605021665492992000000 (0.9999999899522336)
  (matching_cdf 25 11): 7755605015215848540016697/7755605021665492992000000 (0.9999999991683892)
  (matching_cdf 25 12): 7755605021172253782156497/7755605021665492992000000 (0.9999999999364022)
  (matching_cdf 25 13): 7755605021630438800982647/7755605021665492992000000 (0.9999999999954802)
  (matching_cdf 25 14): 158277653503329924533503/158277653503377408000000 (0.9999999999997)
  (matching_cdf 25 15): 7755605021665348135700827/7755605021665492992000000 (0.9999999999999813)
  (matching_cdf 25 16): 369314524841213547628387/369314524841213952000000 (0.9999999999999989)
  (matching_cdf 25 17): 738629049682427859209249/738629049682427904000000 (0.9999999999999999)
  (matching_cdf 25 18): 5170403347776995311537343/5170403347776995328000000 (1.0)
  (matching_cdf 25 19): 15511210043330985981543529/15511210043330985984000000 (1.0)
  (matching_cdf 25 20): 15511210043330985983881249/15511210043330985984000000 (1.0)
  (matching_cdf 25 21): 1193170003333152767999623/1193170003333152768000000 (1.0)
  (matching_cdf 25 22): 2215887149047283711999957/2215887149047283712000000 (1.0)
  (matching_cdf 25 23): 15511210043330985983999999/15511210043330985984000000 (1.0)
  (matching_cdf 25 24): 15511210043330985983999999/15511210043330985984000000 (1.0)
  (matching_cdf 25 25): 1 (1.0)
  Quantile:
  (matching_cumulative 25 0.0001): 0 
  (matching_cumulative 25 0.001): 0 
  (matching_cumulative 25 0.01): 0 
  (matching_cumulative 25 0.05): 0 
  (matching_cumulative 25 0.25): 0 
  (matching_cumulative 25 0.5): 1 
  (matching_cumulative 25 0.75): 2 
  (matching_cumulative 25 0.9): 2 
  (matching_cumulative 25 0.95): 3 
  (matching_cumulative 25 0.99): 4 
  (matching_cumulative 25 0.999): 5 
  (matching_cumulative 25 0.9999): 6 
  (matching_cumulative 25 0.99999): 8 
  (matching_cumulative 25 0.999999): 9 
  (matching_cumulative 25 0.9999999): 10 
  (matching_cumulative 25 0.99999999): 11 

|#
(let ([n 25])
  (displayln "PDF:")
  (for ([i (add1 n)])
    (let ([v_pdf (matching_pdf n i)])
      (displayln (format "(matching_pdf ~a ~a): ~a (~a)" n i v_pdf (* 1.0 v_pdf)))
      ))
  (displayln "CDF:") 
  (for ([i (add1 n)])
    (let ([v_cdf (matching_cdf n i)])
      (displayln (format "(matching_cdf ~a ~a): ~a (~a)" n i v_cdf (* 1.0 v_cdf)))
      )
    )
  (define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.999999 0.9999999 0.99999999))
  (displayln "Quantile:") 
  (for ([q ps])
    (displayln (format "(matching_cumulative ~a ~a): ~a " n q (matching_quantile n q)))
    )  
  )


#|
  A simple simulation for n=25 and comparing with (poisson 1)

  variable : d
  0: 0.37019
  1: 0.36524000000000006
  2: 0.18365000000000004
  3: 0.06139000000000001
  4: 0.01572
  5: 0.00321
  6: 0.0005100000000000003
  7: 8.000000000000002e-5
  8: 1.0000000000000003e-5
  mean: 0.9993400000000002

  variable : pois
  1: 0.3689800000000001
  0: 0.36698000000000003
  2: 0.18393000000000004
  3: 0.06144000000000001
  4: 0.01529
  5: 0.00287
  6: 0.0004800000000000001
  7: 3.0000000000000008e-5
  mean: 0.9997600000000002

  variable : p
  #f: 1.0
  mean: 0 (0.0)

|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 25)
   (define d (matching_dist n))
   (define pois (poisson 1))
   (define p (>= d 10))
   (list d
         pois
         p)
   
   )
)

(displayln "\nModel 1")
(show-marginals (model)
                (list  "d"
                       "pois"
                       "p"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  Poisson 1 only, to be used with enumerate (limit 1e-15)

  variable : pois
  0: 0.3678794411714423
  1: 0.3678794411714423
  2: 0.1839397205857211
  3: 0.061313240195240384
  4: 0.0153283100488101
  5: 0.0030656620097620196
  6: 0.0005109436682936697
  7: 7.299195261338137e-5
  8: 9.12399407667267e-6
  9: 1.013777119630296e-6
  10: 1.0137771196302985e-7
  11: 9.216155633002696e-9
  12: 7.680129694168902e-10
  13: 5.907792072437641e-11
  14: 4.219851480312585e-12
  15: 2.8132343202083986e-13
  16: 1.7582714501302485e-14
  17: 1.0342773236060258e-15
  mean: 0.9999999999999987

  variable : p
  #f: 0.9999998885745214
  #t: 1.1142547827807797e-7
  mean: 1.1142547827807797e-7



|#
(define (model2)
  (enumerate #:limit 1e-15
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define pois (poisson 1))
   (define p (>= pois 10))
   (list pois
         p)
   
   )
)

(displayln "\nModel 2 (poisson only")
(show-marginals (model2)
                (list  "pois"
                       "p"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
 Simulation of the matching task: 
 How many guesses are correct?

 Here we see that the convergence to poisson 1 is quite fast.

  * Enumerate 1..6

  n=1
  variable : num-correct
  1: 1 (1.0)
  mean: 1 (1.0)

  n=2
  variable : num-correct
  0: 1/2 (0.5)
  2: 1/2 (0.5)
  mean: 1 (1.0)

  n=3
  variable : num-correct
  1: 1/2 (0.5)
  0: 1/3 (0.3333333333333333)
  3: 1/6 (0.16666666666666666)
  mean: 1 (1.0)

  n=4
  variable : num-correct
  0: 3/8 (0.375)
  1: 1/3 (0.3333333333333333)
  2: 1/4 (0.25)
  4: 1/24 (0.041666666666666664)
  mean: 1 (1.0)

  n=5
  variable : num-correct
  1: 3/8 (0.375)
  0: 11/30 (0.36666666666666664)
  2: 1/6 (0.16666666666666666)
  3: 1/12 (0.08333333333333333)
  5: 1/120 (0.008333333333333333)
  mean: 1 (1.0)

  n=6
  variable : num-correct
  0: 53/144 (0.3680555555555556)
  1: 11/30 (0.36666666666666664)
  2: 3/16 (0.1875)
  3: 1/18 (0.05555555555555555)
  4: 1/48 (0.020833333333333332)
  6: 1/720 (0.001388888888888889)
  mean: 1 (1.0)


 * importance-sampler (100000 samples)

  n=7
  variable : num-correct
  0: 0.367884
  1: 0.367521
  2: 0.183841
  3: 0.062496
  4: 0.013902
  5: 0.004159
  7: 0.000197
  mean: 1.000473

  n=8
  variable : num-correct
  1: 0.368045
  0: 0.366723
  2: 0.184526
  3: 0.061427
  4: 0.015738
  5: 0.002823
  6: 0.000698
  8: 2e-5
  mean: 1.002793

  n=9
  variable : num-correct
  0: 0.3678890000000001
  1: 0.36770900000000006
  2: 0.18410200000000002
  3: 0.06147300000000001
  4: 0.015093000000000002
  5: 0.0031690000000000004
  6: 0.00046600000000000005
  7: 9.600000000000002e-5
  9: 3.0000000000000005e-6
  mean: 1.000044

  n=10
  variable : num-correct
  0: 0.36856000000000005
  1: 0.36782000000000004
  2: 0.18498000000000003
  3: 0.05929000000000001
  4: 0.015790000000000002
  5: 0.0029300000000000003
  6: 0.0005400000000000001
  7: 8.000000000000002e-5
  8: 1.0000000000000003e-5
  mean: 0.9973400000000001

  For n = 25
  variable : num-correct
  0: 0.36898
  1: 0.36465
  2: 0.18565
  3: 0.06249
  4: 0.01457
  5: 0.00304
  6: 0.00054
  7: 7e-5
  8: 1e-5
  mean: 1.0007100000000002


|#

(define (model3)
  (; enumerate ; use draw-without-replacement
   ; rejection-sampler
   importance-sampler ; use shuffle
   ; mh-sampler

   (define n 10)
   ; (define n 25)

   ; shuffle is much faster than draw-without-replacement but it does
   ; not work well with enumerate.
   (define true (shuffle (range n)))
   (define guess (shuffle (range n)))
   ; (define true (draw-without-replacement n (range n)))
   ; (define guess (draw-without-replacement n (range n)))

   (define num-correct (sum (map (lambda (t g) (b2i (= t g))) true guess)))
   (list num-correct
         )
   
   )
)

(displayln "\nModel 3")
(show-marginals (model3)
                (list  "num-correct"
                       )
                #:num-samples 1000000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

#|
  ESP cards
  
  The ESP deck contains of 5 sets of 5 symbols, so it's more likely to get a match
  by pure guessing. 
  This is an example of the matching problem that is NOT supported by the 
  matching distribution.

  As the simulation shows the expected number of correct guesses
  in this variant is 5, not 1 as in the "pure" variant of 1..25 
  different cards.

  (To show that ESP is probable, it would probably requires about 16 or more
   correct guesses. Using the 0.99 quantile, i.e. 10 correct guesses, will 
   not be enough. One should be looking at something like the 0.9999999 quantile
   instead.
   Also, 0 correct guesses is not enough either, since it is a probability 
   of about 0.0046 for this .)

  variable : num-correct
  5: 0.18963999999999995
  4: 0.18193999999999996
  6: 0.16156999999999996
  3: 0.13755999999999996
  7: 0.10979999999999997
  2: 0.07556999999999998
  8: 0.06388999999999999
  9: 0.031069999999999993
  1: 0.024779999999999993
  10: 0.013459999999999996
  11: 0.004479999999999999
  0: 0.004409999999999999
  12: 0.0013299999999999998
  13: 0.0003899999999999999
  14: 0.00010999999999999998
  mean: 4.999779999999998
  HPD interval (0.84): 2..7
  HPD interval (0.95): 1..9
  HPD interval (0.99): 0..10
  HPD interval (0.999999): 0..16
  Percentiles:
  (0.0001 0)
  (0.001 0)
  (0.01 1)
  (0.05 2)
  (0.25 4)
  (0.5 5)
  (0.75 6)
  (0.9 8)
  (0.95 9)
  (0.99 10)
  (0.999 12)
  (0.9999 14)
  (0.99999 15)
  (0.999999 16)
  (0.9999999 16)
  (0.999999999 16)
  Histogram:
 0:   431 ## (0.004 / 0    )
 1:  2597 ########### (0.025 / 0.004)
 2:  7141 ############################## (0.071 / 0.030)
 3: 13513 ######################################################## (0.135 / 0.101)
 4: 18496 ############################################################################# (0.184 / 0.236)
 5: 19368 ################################################################################ (0.193 / 0.421)
 6: 15912 ################################################################## (0.159 / 0.615)
 7: 11140 ############################################### (0.111 / 0.774)
 8:  6284 ########################## (0.062 / 0.885)
 9:  3106 ############# (0.031 / 0.948)
10:  1305 ###### (0.013 / 0.979)
11:   511 ### (0.005 / 0.992)
12:   153 # (0.001 / 0.998)
13:    30 # (0.000 / 0.999)
14:    10 # (0.000 / 0.999)
15:     2 # (2e-5  / 0.999)
16:     1 # (1e-5  / 0.999)

|#

(define esp_cards (flatten (rep 5 (range 5))))
(show "esp_cards" esp_cards)
(define (model4)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n (length esp_cards))

   ; shuffle is much faster than draw-without-replacement but it does
   ; not work well with enumerate.
   ; (define true (shuffle (range n)))
   ; (define guess (shuffle (range n)))
   (define true (draw-without-replacement n esp_cards))
   (define guess (draw-without-replacement n esp_cards))

   (define num-correct (sum (map (lambda (t g) (b2i (= t g))) true guess)))
   (list num-correct
         )
   
   )
)

(displayln "\nModel 4 ESP cards")
(show-marginals (model4)
                (list  "num-correct"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.95 0.99 0.999999)
                #:show-histogram? #t
                #:show-percentiles? '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999 0.999999 0.9999999 0.999999999)
                ; #:burn 0
                ; #:thin 0
                )


