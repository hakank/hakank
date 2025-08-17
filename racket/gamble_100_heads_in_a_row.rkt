#| 

  100 heads in a row in Racket/Gamble 

  From Holger van Jouanne-Diedrich:
  https://www.linkedin.com/feed/?highlightedUpdateType=SHARED_BY_YOUR_NETWORK&highlightedUpdateUrn=urn%3Ali%3Aactivity%3A7360550738478989312
  """
  A little fun experiment, comments are welcome.

  If I flip a coin 100 times and it lands heads every time, what will it most likely
  land on the next flip?

   * heads
   * 50/50
   * tails
  """

  I'd say heads and that it's a biased coin with a probability of about 0.90 of landing heads.
  
  Note: For smaller number of observed heads in a row, then this still assumes that it's a biased coin,
  and that is somewhat dubious. So let's say that this is a model to when when we think that a biased coin
  is involved.


variable : prob
0.9505267410466692: 0.0009999999999999994
0.999567977118029: 0.0009999999999999994
0.9883619928279039: 0.0009999999999999994
0.9994860784568973: 0.0009999999999999994
0.9893161876257731: 0.0009999999999999994
...
0.9874554859754143: 0.0009999999999999994
0.9973990369326374: 0.0009999999999999994
0.9963527461190467: 0.0009999999999999994
0.9870277390248289: 0.0009999999999999994
0.9927812397845973: 0.0009999999999999994
mean: 0.9900780755639575
HPD interval (0.84): 0.9804720778801697..0.9999544878236534
HPD interval (0.9): 0.9747532786233534..0.9999544878236534
HPD interval (0.95): 0.967614141139852..0.9999398199712026
HPD interval (0.99): 0.9539001568537555..0.9999544878236534
HPD interval (0.999): 0.9392132136587972..0.9999544878236534
Histogram:
0.939:   1 # (0.001 / 0    )
0.942:   0  (0     / 0.001)
0.945:   0  (0     / 0.001)
0.948:   0  (0     / 0.001)
0.951:   4 ## (0.004 / 0.001)
0.954:   7 ### (0.007 / 0.005)
0.957:   1 # (0.001 / 0.012)
0.96 :   6 ## (0.006 / 0.013)
0.964:  12 #### (0.012 / 0.019)
0.967:  14 ##### (0.014 / 0.031)
0.97 :  14 ##### (0.014 / 0.045)
0.973:  24 ######## (0.024 / 0.059)
0.976:  20 ####### (0.02  / 0.083)
0.979:  30 ########## (0.03  / 0.103)
0.982:  43 ############## (0.043 / 0.133)
0.985:  63 #################### (0.063 / 0.176)
0.988:  82 ######################### (0.082 / 0.239)
0.991: 109 ################################## (0.109 / 0.321)
0.994: 135 ######################################### (0.135 / 0.43 )
0.997: 171 #################################################### (0.171 / 0.565)

variable : p is fair coin
#f: 1.0000000000000007
mean: 0 (0.0)
Histogram:
#f: 1000 ################################################################################ (1.0   / 0    )

variable : bias
101/102: 1.0000000000000007
mean: 0.9901960784313732
HPD interval (0.84): 101/102..101/102
HPD interval (0.9): 101/102..101/102
HPD interval (0.95): 101/102..101/102
HPD interval (0.99): 101/102..101/102
HPD interval (0.999): 101/102..101/102
Histogram:
101/102: 1000 ################################################################################ (1.0   / 0    )


  A theoretical result:

  (a_prior, b_prior) = (1,1)
  heads = 100
  tails = 0
  a_post = a_prior + heads
  b_post = b_prior + tails

  # Posterior distribution is Beta(a_post, b_post)
  posterior_mean = a_post / (a_post + b_post)

  posterior_mean
  > (* 1.0 (/ (+ 100 1) (+ (+ 100 1) (+ 1 ))))
  0.9901960784313726


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


#| 
  What is the estimated bias (probability) given
  that we saw n heads in a row?
  If a > 1 then a is the number of prior pseudo-tosses.

(n 3 bias: 4/5 0.8)
(n 10 bias: 11/12 0.9166666666666666)
(n 30 bias: 31/32 0.96875)
(n 70 bias: 71/72 0.9861111111111112)
(n 100 bias: 101/102 0.9901960784313726)
(n 1000 bias: 1001/1002 0.999001996007984)

with prior pseudo-tosses
(n 3 bias: 13/23 0.5652173913043478)
(n 10 bias: 2/3 0.6666666666666666)
(n 30 bias: 4/5 0.8)
(n 70 bias: 8/9 0.8888888888888888)
(n 100 bias: 11/12 0.9166666666666666)
(n 1000 bias: 101/102 0.9901960784313726)


|#
(define (biased-coin-prob n #:a [a 1])
  (if (= a 1)
      (let* ([x (+ n 1)]
             [y 1])
        (/ x (+ x y)))
      (let* ([x (+ n a)]
             [y (+ n (* 2 a))])
        (/ x y)
        )
  ))

(for ([n '(3 10 30 70 100 1000)])
  (show2 "n" n "bias:"  (biased-coin-prob n) (* 1.0 (biased-coin-prob n)))
  )
(newline)
(displayln "with prior pseudo-tosses")
(for ([n '(3 10 30 70 100 1000)])
  (let ([a 10])
    (show2 "n" n "bias:"  (biased-coin-prob n #:a a) (* 1.0 (biased-coin-prob n #:a a)))
    
  ))

(newline)

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 100)

   (define prob (beta 1 1))
   ; (define prob (beta 2 2))
   ; (define prob 0.5)

   (define coins (for/list ([i n]) (flip prob)))

   (define p-all-heads (all coins))

   ; We have seen 100 heads in a row.
   (observe/fail (eq? p-all-heads #t))

   (define p-is-fair-coin (<= (abs (- prob 0.5)) 0.1))

   (define bias (biased-coin-prob n))
     
   (list prob
         p-is-fair-coin
         bias)
   
   )
)

(show-marginals (model)
                (list  "prob"
                       "p is fair coin"
                       "bias"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84 0.9 0.95 0.99 0.999)
                    #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


