#| 

  Required number of tests for credible interval in Racket.Gamble 

  From the Python code in
  https://colab.research.google.com/drive/1s2KuOwNoLuXgLgZ9tKuV7hrek8arP-xz?usp=sharing
  """
  Explanation:

  - Beta distribution: We use scipy.stats.beta.ppf() to compute the percentiles 
    for the Beta distribution. The Beta distribution’s parameters are α=k+1 
    and β=n−k+1, where k is the number of observed successes, and n is the total 
    number of trials.

  - Iterative approach: The loop increases n until the credible interval width is 
    within the desired margin (i.e., ±0.05).

  - Output: The program prints the number of trials n required to achieve a 90% 
    credible interval with the specified width, as well as the actual credible interval bounds.

  Example Output:
 
  Required number of trials (n): 268
  90% credible interval: [0.450, 0.550]
  Credible interval width: 0.099944
  """


  Via an answer of 
  https://www.reddit.com/r/Probability/comments/1fcd48i/question_with_law_of_large_numbers/
  """
  Question with law of large numbers

  Given a random event from which I do not know the probability p but i can run as 
  many tests of this event as i want. So, in theory, i can obtain a pretty good 
  approximation of p (lets call this approximation "r") by repeating the event a 
  looooot of times.

  Is there a way to know how many tests are enough to be, lets say, 90% sure that my 
  approximation r is okay?

  I think that, without knowing p, its not possible but i would love to listen any ideas.

  Thanks in advance
  """

  Note: beta.ppf is the inverse cdf, which is called dist-inv-cdf in Gamble.

  Here are some examples

  Confidence_level: 0.84 error_margin: 0.05
  Required number of trials (n) : 195
  84.0 credible interval: 0.452539..0.552521
  Credible interval width: 0.099982

  Confidence_level: 0.9 error_margin: 0.05
  Required number of trials (n) : 268
  90.0 credible interval: 0.450028..0.549972
  Credible interval width: 0.099944

  Confidence_level: 0.95 error_margin: 0.05
  Required number of trials (n) : 381
  95.0 credible interval: 0.448719..0.548683
  Credible interval width: 0.099963

  Confidence_level: 0.99 error_margin: 0.05
  Required number of trials (n) : 659
  99.0 credible interval: 0.450765..0.55074
  Credible interval width: 0.099975

  Confidence_level: 0.84 error_margin: 0.01
  Required number of trials (n) : 4934
  84.0 credible interval: 0.490001..0.509999
  Credible interval width: 0.019998

  Confidence_level: 0.9 error_margin: 0.01
  Required number of trials (n) : 6762
  90.0 credible interval: 0.490001..0.509999
  Credible interval width: 0.019999

  Confidence_level: 0.95 error_margin: 0.01
  Required number of trials (n) : 9601
  95.0 credible interval: 0.489948..0.509948
  Credible interval width: 0.019999

  Confidence_level: 0.99 error_margin: 0.01
  Required number of trials (n) : 16583
  99.0 credible interval: 0.49003..0.51003
  Credible interval width: 0.02


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;
; (calculate_credible_interval_width credible_interval_width)
; Returns
; - required number of tests to reach credible_interval_width
; - interval_width: The interval width
; - lower_bound: Lower bound
; - upper_bound: Upper bound
;
; Port of the Python code in
; https://colab.research.google.com/drive/1s2KuOwNoLuXgLgZ9tKuV7hrek8arP-xz?usp=sharing#scrollTo=CVvqzHPyJ4bp
; rewritten to not use global variables or re-assignments.
; 
(define (calculate_credible_interval_width confidence_level [error_margin 0.05])

  (define (calculate_credible_interval_width confidence_level credible_interval_width n r) 
    ; Calculate the number of successes k based on r and n
    (define k (inexact->exact (round (* r n))))
    
    ; Alpha and beta for Beta distribution posterior
    (define alpha (add1 k))
    (define beta_val (+ (- n k) 1))

    ; (show2 "n" n "k" k "alpha" alpha "beta_val" beta_val)
    
    ; Calculate the lower and upper percentiles using scipy's beta distribution
    (define q1 (/ (- 1 confidence_level) 2))
    (define lower_bound (dist-inv-cdf (beta-dist alpha beta_val) q1))
    
    (define q2 (- 1 q1))
    (define upper_bound (dist-inv-cdf (beta-dist alpha beta_val) q2))
    
    ; Calculate the interval width
    (define interval_width (- upper_bound lower_bound))
    
    (if (<= interval_width  credible_interval_width)
        (list n interval_width lower_bound upper_bound)
        (calculate_credible_interval_width confidence_level credible_interval_width (add1 n) r)
        )
    )

  (define credible_interval_width (* 2 error_margin))
  (calculate_credible_interval_width confidence_level credible_interval_width 10 0.5)
  
  )


;
; Show the results of calculate_credible_interval_width
;
(define (show-tests-for-confidence-level confidence_level [error_margin 0.05])
  (displayln (format "Confidence_level: ~a error_margin: ~a" confidence_level error_margin))
  (define res (calculate_credible_interval_width confidence_level error_margin))
  (define required-tests (first res))
  (define interval_width (second res))
  (define lower_bound (third res))
  (define upper_bound (fourth res))

  ; Output the results
  (displayln (format "Required number of trials (n) : ~a" required-tests))
  (displayln (format "~a credible interval: ~a..~a" (* 100 confidence_level) (~r lower_bound #:precision 6) (~r upper_bound #:precision 6)))
  (displayln (format "Credible interval width: ~a" (~r interval_width #:precision 6)))
)

(show-tests-for-confidence-level 0.84 0.05)
(newline)
(show-tests-for-confidence-level 0.9 0.05)
(newline)
(show-tests-for-confidence-level 0.95 0.05)
(newline)
(show-tests-for-confidence-level 0.99 0.05)
(newline)


(show-tests-for-confidence-level 0.84 0.01)
(newline)
(show-tests-for-confidence-level 0.9 0.01)
(newline)
(show-tests-for-confidence-level 0.95 0.01)
(newline)
(show-tests-for-confidence-level 0.99 0.01)
(newline)
