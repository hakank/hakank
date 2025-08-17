#| 

  Landing on 25 in Racket/Gamble 

  From BL "You Are Standing On The First Step Of An Infinitely Long Numbered Path -
  How likely will you step on step 25?"
  https://medium.com/puzzle-sphere/you-are-standing-on-the-first-step-of-an-infinitely-long-numbered-path-9e3173df242d
  """
  You have a fair coin which has the number 1 written on one side and the number 2 on the other. You throw 
  the coin, and if it comes up N, you would then take N steps to the right.
  For example, if you throw the coin and it comes up 2, you take 2 steps to the right to land on step number 3.
  You now repeat the exercise, throwing the coin again and walking the number of steps that comes up on the coin. 
  If you throw the coin 24 times, you are certain to have landed on, or past, step number 25.

  What is the probability that at some point you will land on step number 25?

  ...

  So the probability ... would be:
   u24 = 2/3 = 1/3 * (-1/2)^24 ~ 0.6666666865
  """

  And this model gets the same probabiity: 0.6666666865348816

variable : len
16: 19305/65536 (0.2945709228515625)
17: 3861/16384 (0.23565673828125)
15: 7007/32768 (0.213836669921875)
18: 7735/65536 (0.1180267333984375)
14: 1287/16384 (0.07855224609375)
19: 5049/131072 (0.03852081298828125)
13: 45/4096 (0.010986328125)
20: 8721/1048576 (0.008316993713378906)
21: 1235/1048576 (0.0011777877807617188)
12: 1/4096 (0.000244140625)
22: 441/4194304 (0.00010514259338378906)
23: 45/8388608 (5.364418029785156e-6)
24: 1/8388608 (1.1920928955078125e-7)
mean: 136081863/8388608 (16.222222208976746)
Histogram:
13:  16 ##### (0.016 / 0    )
14:  77 ##################### (0.077 / 0.016)
15: 206 ###################################################### (0.206 / 0.093)
16: 306 ################################################################################ (0.306 / 0.299)
17: 240 ############################################################### (0.24  / 0.605)
18: 116 ############################### (0.116 / 0.845)
19:  28 ######## (0.028 / 0.961)
20:  11 ### (0.011 / 0.989)

variable : tot
25: 11184811/16777216 (0.6666666865348816)
26: 5592405/16777216 (0.3333333134651184)
mean: 425022805/16777216 (25.33333331346512)
Histogram:
25: 674 ################################################################################ (0.674 / 0    )
26: 326 ####################################### (0.326 / 0.674)

variable : p
#t: 11184811/16777216 (0.6666666865348816)
#f: 5592405/16777216 (0.3333333134651184)
mean: 11184811/16777216 (0.6666666865348816)
Histogram:
#f: 326 ####################################### (0.326 / 0    )
#t: 674 ################################################################################ (0.674 / 0.326)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define target 25)
   
   (define (f a tot)
     (if (>= tot target)
         (list a tot)
         (let ([c (uniform-draw '(1 2))])
           (f (append a (list c)) (+ tot c)))))
   
   (define a  (f '() 1))
   (define l (first a))
   (define tot (second a))
   (define len (length l))
   
   (define p (eq? tot target))

   (list len tot p)

   )
)

(show-marginals (model)
                (list  "len"
                       "tot"
                       "p"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


