#| 

  Weekend problem Racket Gamble.
  

  This is a port of the PSI model test/weekend.psi 
  """
  example from: Disintegration and Bayesian Inversion, Both Abstractly and Concretely 
  by K Cho and B Jacobs
  """
  https://www.cs.ru.nl/B.Jacobs/PAPERS/disintegration.pdf
  """
  Customers calling
  Imagine a call centre that is open for 8 hours on each day of the week. The distribution
  of calls is different on weekends (Sat-Sun) from other days (Mon-Fri). What can we then
  learn from a single call at a given time of the day regarding whether it is weekend or not?

  ...

  In the weekend diagram on the left we see that the calls start coming in later. Now we
  ask ourselves the question: suppose we see one call at (hour) 6. How does this affect the
  prior distribution? Of course, the updated distribution should have a higher likelihood for
  ‘weekend’ since 6 is relatively late

  ---
    
  >>> d(6)
  0.374|W> + 0.626|~W>

  """

  Without the observation of call at hour 6, the probability of weekend is 
   0.286|W> + 0.714|~W>
  (page 36)

  This model: 
 
  * Without any observation

  var : isWeekend
  #f: 0.7125
  #t: 0.2875
  mean: 0.2875


  * With the observation of a call at 6. How likey is it that's on a weekend?

  var : isWeekend
  #f: 0.6256
  #t: 0.3744
  mean: 0.3744


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

; set! model
(define (model)

  (; enumerate ; strange results
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ; We observe that the time is between 0 and 8
   (define (hoursPrior mu v)
     (sample (sampler->discrete-dist (importance-sampler
                              (define r (normal mu v))
                              (observe/fail (and (<= 0 r) (<= r 8)))
                              r)
                             1)))
   
   (define isWeekend (flip 2/7))
   
   (define hour (if isWeekend
                    (hoursPrior 5 4)
                    (hoursPrior 2 4)))

   ; (observe/fail (< (abs (- hour 6.0)) 0.1))
   
   (list isWeekend
         )
  
   )
  )

(show-marginals (model)
                (list "isWeekend"
                      )
                #:num-samples 10000
                #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:show-stats? #t
                )


