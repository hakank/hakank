#| 

  Quake probability in Racket/Gamble 

  From Statistics101 (Resample Stats)
  File quakeProbability.txt
  """
  (From: http://www.physics.utah.edu/~p5720/assgn/a07.html)
  Given: 
  - The distribution of earthquakes is lognormal.
  - The mean time between quakes in a certain region is 1200 years
  - The standard deviation of the time between quakes is 120 years
  - The last quake was 1200 years ago
  Find: The probability that another quake will occur in the next 50 years.
  mu: 121.12402989301995
  sigma: 1200.2861533879125
  probability: 0.1604
  (another run: probability: 0.15739)
  """

  In Statistics101 LOGNORMAL seems to mean something else:
  """
  LOGNORMAL exp 1 1200 120 xxx
  PRINT xxx"
  -> 1124.5607506111548
  """
  What does "exp" indicates? It seems that it's just (normal mu,sigma).

  Mathematica:
  """
  Probability(x - 1200 <= 50 && x - 1200 >= 0 , x ~ NormalDistribution(1200, 120)) ;; N
  -> 0.161539
  """

  variable : next_quake
  mean: 1199.8960786159237

  variable : y
  mean: -0.10392138418919912

  variable : p
  mean: 0.15380000000001273


  Using normal instead of log_normal2 gives about the same probability (p):

  variable : next_quake
  mean: 1201.1589620858667

  variable : y
  mean: 1.1589620857518679

  variable : p
  mean: 0.16520000000001464



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; 
; Definition of lognormal from
; https://web.physics.utah.edu/~p5720/assgn/a07.html
; Testing log_normal2(1200,120) -> 1231.5912119933946
; So this is more like the expected behaviour (i.e. the Statistics101 model).
; But it's not the log_normal distribution as Mathematica (and I) defines it.
;
(define (log_normal2 mu sd)
  (let* ([S (sqrt (log (+ (/ (* sd sd) (* mu mu)) 1.0)))]
         [M (- (log mu) (* 0.5 S S))])
         (exp (+ (* S (normal 0 1)) M))
         ))

; (show "test:" (log_normal2 1200 120)) 

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define mu 1200)
    (define last_quake 1200) ; last quake was 1200 years ago
    (define sigma 120)
    ; (define next_quake (normal mu sigma)) ; Give almost the same as log_normal2
    (define next_quake (log_normal2 mu sigma))
    (define y (- next_quake last_quake))
    (define p (and (<= y 50) (>= y 0))) ; next quake is within next 50 ye
    (list next_quake
          y
          p
          )
   )
)

(show-marginals (model)
                (list  "next_quake"
                       "y"
                       "p"
                     )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


