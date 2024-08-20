#| 

  Changepoint detection (coal miners and text messages) in Racket Gamble.

  Identify the change point in the coal miners data.
  https://pymc-devs.github.io/pymc/tutorial.html  
  """ 
  Consider the following dataset, which is a time series of recorded
  coal mining disasters in the UK from 1851 to 1962 
  [R.G. Jarrett. A note on the intervals between coal mining disasters. Biometrika, 66:191–193, 1979.]
  """

  Also 
  From http://nbviewer.jupyter.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter1_Introduction/Ch1_Introduction_PyMC3.ipynb
  """
  You are given a series of daily text-message counts from a user of your system.
  The data, plotted over time, appears in the chart below. You are curious to
  know if the user's text-messaging habits have changed over time, either
  gradually or suddenly. How can you model this? (This is in fact my own
  text-message data. Judge my popularity as you wish.)
  """


  This is a port of my WebPPL model changepoint_coal_miners.wppl and changepoint_text_messages.wppl

  * Coal miners

Coal miners
var : tau
39: 0.8162084586099843
37: 0.16794857199197216
43: 0.010562774537748433
48: 0.0029659971794298093
32: 0.0014460633571404826
25: 2.4741001132106768e-26
81: 6.612853485450649e-27
30: 1.6350227693181303e-27
63: 1.34109023994574e-27
46: 5.059974555620259e-60
mean: 38.72533884971391
Min: 30 Mean: 41.289 Max: 60 Variance: 3.453479 Stddev: 1.8583538414413978
ix: 3
Credible interval (0.84): 40..42
Credible-interval2 (0.84): 40..42 (ps: (0.08000000000000002 0.9199999999999999))


  * Text messages

Text messages
var : tau
0: 0.4650331189069688
9: 0.24769357551522328
66: 0.21163473048318912
40: 0.057140002163327624
68: 0.01497544780200742
26: 5.09407143008424e-62
41: 2.036705314462516e-67
54: 1.261816676127724e-71
15: 4.0067433020408955e-73
70: 2.2744307723109496e-74
mean: 19.645257173063992
Min: 5 Mean: 42.967 Max: 44 Variance: 1.447911 Stddev: 1.2032917351997394
ix: 1
Credible interval (0.84): 43..43
Credible-interval2 (0.84): 43..43 (ps: (0.08000000000000002 0.9199999999999999))


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (changepoint data)

  (importance-sampler
   
   (define len (length data))  
   (define alpha (avg data)) 
   
   (define lambda-1 (exponential alpha))
   (define lambda-2 (exponential alpha))
   (define tau (random-integer len))
   
   ;; Data is according to Poisson with two different lambdas,
   ;; one before the change point (tau) and one after the change point
   (define (x i) 
     (if (> tau i)
         (poisson-dist lambda-1)
         (poisson-dist lambda-2)))
   
   ;; observe the data
   (for ([i (range len)])
     (observe (sample (x i)) (list-ref data i))
     )
   
   (list tau)
   
   )
  )

;; Coal miners data from https://pymc-devs.github.io/pymc/tutorial.html
(define *coal-miners* '(4 5 4 0 1 4 3 4 0 6 3 3 4 0 2 6 3 3 5 4 5 3 1 
                        4 4 1 5 5 3 4 2 5 2 2 3 4 2 1 3 2 2 1 1 1 1 3 
                        0 0 1 0 1 1 0 0 3 1 0 3 2 2 0 1 1 1 0 1 0 1 0 
                        0 0 2 1 0 0 0 1 1 0 2 3 3 1 1 2 1 1 1 1 2 4 2 
                        0 0 1 4 0 0 0 1 0 0 0 0 0 1 0 0 1 0 1))

;; http://nbviewer.jupyter.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter1_Introduction/Ch1_Introduction_PyMC3.ipynb
(define *text-messages* '(13 24 8 24 7 35 14 11 15 11 22 22 11 57 11 19 29 6
                          19 12 22 12 18 72 32 9 7 13 19 23 27 20 6 17 13 10
                          14 6 16 15 7 2 15 15 19 70 49 7 53 22 21 31 19 11
                          18 20 12 35 17 23 17 4 2 31 30 13 27 0 39 37 5 14
                          13 22))

(displayln "Coal miners")
(show-marginals (changepoint *coal-miners*)
               (list "tau")
               #:truncate-output 5
               #:show-stats? #t
               #:credible-interval 0.84
               #:credible-interval2 0.84
               )
(newline)


(displayln "Text messages")
(show-marginals (changepoint *text-messages*)
               (list "tau")
               #:truncate-output 5
               #:show-stats? #t
               #:credible-interval 0.84
               #:credible-interval2 0.84
               )
(newline)

    
