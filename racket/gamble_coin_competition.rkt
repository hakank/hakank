#| 

  Coin competition in Racket/Gamble 

  From Pascal Bercker: "Coin Competition and the Geometric Distribution"
  https://medium.com/@pbercker/coin-competition-and-the-geometric-distribution-0f68ef440ea8 
  """
  Ty and Guy are both flipping fair coins until they respectively obtain their first heads. 
  Find the probability that it takes Ty at least 4 times as many flips to obtain his first heads as Guy.
  """

  We cannot use a "full" enumerate since it will generate infinite sequences, but with 
  enumerate #:limit 1e-05 we can study what's happening if we allow sequence of 
  length <= 20 (as in the post):

variable : ty
1: 2097150/4194263 (0.5000044107868296)
2: 1048574/4194263 (0.25000196697250504)
3: 524286/4194263 (0.12500074506534284)
4: 262143/4194263 (0.06250037253267142)
5: 131070/4194263 (0.03124982863497115)
6: 65535/4194263 (0.015624914317485574)
7: 762/97541 (0.007812099527378231)
8: 381/97541 (0.0039060497636891154)
9: 8190/4194263 (0.0019526672504800008)
10: 4094/4194263 (0.0009760952043302959)
11: 2046/4194263 (0.00048780918125544345)
12: 1022/4194263 (0.0002436661697180172)
13: 510/4194263 (0.00012159466394930408)
14: 254/4194263 (6.055891106494752e-5)
15: 126/4194263 (3.0041034622769244e-5)
16: 62/4194263 (1.4782096401680104e-5)
17: 30/4194263 (7.1526272911355345e-6)
18: 14/4194263 (3.3378927358632492e-6)
19: 6/4194263 (1.4305254582271068e-6)
20: 2/4194263 (4.768418194090356e-7)
mean: 8388118/4194263 (1.9999027242688405)

variable : guy
1: 2097150/4194263 (0.5000044107868296)
2: 1048574/4194263 (0.25000196697250504)
3: 524286/4194263 (0.12500074506534284)
4: 262142/4194263 (0.06250013411176171)
5: 131070/4194263 (0.03124982863497115)
6: 65534/4194263 (0.01562467589657587)
7: 762/97541 (0.007812099527378231)
8: 16382/4194263 (0.0039058113427794107)
9: 8190/4194263 (0.0019526672504800008)
10: 4094/4194263 (0.0009760952043302959)
11: 2046/4194263 (0.00048780918125544345)
12: 1022/4194263 (0.0002436661697180172)
13: 510/4194263 (0.00012159466394930408)
14: 255/4194263 (6.079733197465204e-5)
15: 126/4194263 (3.0041034622769244e-5)
16: 63/4194263 (1.5020517311384622e-5)
17: 30/4194263 (7.1526272911355345e-6)
18: 15/4194263 (3.5763136455677673e-6)
19: 6/4194263 (1.4305254582271068e-6)
20: 2/4194263 (4.768418194090356e-7)
mean: 8388148/4194263 (1.9999098768961316)

variable : p
#f: 3923671/4194263 (0.9354852092012351)
#t: 270592/4194263 (0.06451479079876489)
mean: 270592/4194263 (0.06451479079876489)

  Thus, the probability that it takes Ty at least 4 times as many flips to obtain his first heads as Guy 
  is about 0.0645.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define target 1)
   
   (define (f n c)
     (if (= c target)
         n
         (let ([t (bernoulli 1/2)])
           (f (add1 n) (+ c t)))))

   (define ty (f 0 0))
   (define guy (f 0 0))

   ; probability that it takes Ty at least 4 times as many flips to obtain his first heads as Guy.
   (define p (>= ty (* 4 guy )))

   
   (list ty guy p)

   )
)

(show-marginals (model)
                (list  "ty"
                       "guy"
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


