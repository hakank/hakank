#| 

  Fair coin tosses in Racket Gamble.

  From 
  https://keith-mcnulty.medium.com/the-trick-that-helps-all-statisticians-survive-069ac685e6d7
  """
  A Cambridge University mathematics entrance question in 1991 posed the following 
  question:

  A fair coin is thrown 10,000 times. On each throw, 1 point is scored for a head and 
  1 point is lost for a tail. Find an approximation for the probability that the final 
  score is greater than 100.

  ...

  Our standard deviation is 50, so we are looking for the area under the normal curve 
  to the right of 50.5/50 = 1.01 standard deviations above the mean, so our z-score 
  is +1.01. We can use tables, an appropriate scientific calculator, or a function in 
  R to calculate the appropriate upper-tail p-value for this z-score:

  > pnorm(1.01, lower.tail = FALSE)
  [1] 0.1562476

  Let’s see if this agrees with the R function for calculating the p-value for a 
  binomial distribution:

  > pbinom(5050, 10000, 0.5, lower.tail = FALSE)
  [1] 0.1562476

  A perfect match. And there we are — the probability that the score is greater than 
  100 is approximately 15.62%.
  """

  Here's the corresponding forms in Gamble:

  Normal:
  > (dist-cdf (normal-dist 1.01 1) 0)
  0.15624764502125463

  Binomial
  > (- 1 (dist-cdf (binomial-dist 10000 0.5) 5050))
  0.1562476047553395

  The binomial model below is pretty fast and agrees with the above
  (#f : 0.8437523952446608)
  (#t : 0.15624760475533925)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")


;;; This is too slow (as expected)
(define (fair-coin-tosses)
  (; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define n 10000)

   ;;; (define (toss i) (uniform-draw '(1 -1)))

   (define sum-tosses
     (for/sum ([i (range n)])
       (uniform-draw '(1 -1))
       ))

   (define p (> sum-tosses 100))

   p
   )
  )

; (show-model (fair-coin-tosses) #:num-samples 1000 #:no-stats? #t #:no-cred? #t)

;;; This is quite fast
; (#f : 0.8437523952446608)
; (#t : 0.15624760475533925)
(define (fair-coin-tosses-binom)
  (enumerate

   (define p (binomial 10000 0.5))
   (define prob (> p 5050))
   
   prob
   )
  )

(show-model (fair-coin-tosses-binom)
            ; #:num-samples 1000
            #:no-stats? #t
            #:no-cred? #t)
