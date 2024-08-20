#| 

  Laplace birth problem in Racket Gamble.

  From http://mc-stan.org/workshops/vanderbilt2016/carp-1.pdf
  Slide 5ff
  """
 
  Laplace's data on live births in Paris 1745-1770:
     sex        live births
     ----------------------
     female      241 945
     male        251 527    
  
  * Question 1 (Estimation):
    What is the birth rate of boys vs. girls?

  * Question 2 (Event Probability)
    Is a boy more likely to be born than a girl?

    Bayes (1763) set up the 'Bayesian' model  Laplace (1781, 1786) solved for the posterior

  ...

  (Answers:)
  * Q1: θis 99% certain to lie in (0.508,0.512)

  * Q2: Laplace 'morally certain' boys more prevalent
  """

  Result:

var : theta>0.5
#t: 1.0
#f: 1.577755145341712e-42
mean: 1.0
Min: 1 Mean: 1.0 Max: 1 Variance: 0 Stddev: 0
Credible interval (0.99): 1..1

var : theta
0.5097064229717597: 0.030747653309313096
0.5097172747874451: 0.030745616579430243
0.5097913429338031: 0.030541462552237927
0.5098104703326767: 0.03043534904097047
0.5095532473288388: 0.030022363666498006
0.5099113875036669: 0.0295262738566138
...
0.5368529141696051: 4.39417e-319
0.5368565561350237: 3.61187e-319
0.4824269459186897: 2.14e-321
0.48239909978847373: 4e-322
0.48239143062793816: 2.67e-322
0.4823711077225628: 1.33e-322
mean: 0.5098180525186731
Min: 0.507833463245783 Mean: 0.5096741760370481 Max: 0.5115857633799719 Variance: 5.143241141887648e-7 Stddev: 0.0007171639381541468
Credible interval (0.99): 0.5081696291598137..0.5113671506428056

  Which seem to agree with the decription above.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (laplace-births)
  (importance-sampler
   
   (define male2 251527)
   (define female 241945)
    
   (define theta (beta 1 1))
   (define male (binomial-dist (+ male2 female) theta))
   (define theta>0.5 (> theta 0.5))

   (observe-sample male 251527)

   ;; (case var
   ;;   [("theta-gt-half") theta-gt-half]
   ;;   [("theta") theta]
   ;;   )
   (list theta>0.5 theta)
    
   )
  )

;; (for ([var (list "theta-gt-half" "theta")])
;;   (show "var" var)
;;   (show-model (laplace-births var) #:no-dist? #t)
;;   (newline)
;; )

(show-marginals (laplace-births)
                '("theta>0.5" "theta")
                #:truncate-output 6
                #:show-stats? #t
                #:credible-interval 0.99
                )
