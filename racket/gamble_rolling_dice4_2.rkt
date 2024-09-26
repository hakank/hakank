#| 

  Rolling dice problem in Racket Gamble.

  From https://dtai.cs.kuleuven.be/problog/tutorial/basic/03_dice.html
  """
  Let’s consider an infinite number of dice, which we roll one after the other until we 
  see a six for the first time. What is the probability of stopping after n dice? 
  The first die is always rolled, those with higher numbers D are only rolled if the 
  previous roll did not stop the process.
  """

  var : len
  1: 0.16605000000000006
  2: 0.13927000000000006
  3: 0.11507000000000005
  4: 0.09897000000000003
  5: 0.07968000000000003
  6: 0.06779000000000003
  7: 0.05541000000000002
  8: 0.045370000000000014
  9: 0.03799000000000002
  10: 0.032790000000000014
  ...
  48: 3.000000000000001e-5
  49: 3.000000000000001e-5
  51: 2.000000000000001e-5
  52: 1.0000000000000004e-5
  53: 1.0000000000000004e-5
  55: 1.0000000000000004e-5
  56: 1.0000000000000004e-5
  58: 1.0000000000000004e-5
  59: 1.0000000000000004e-5
  60: 1.0000000000000004e-5
  mean: 6.001490000000001

  Closed form 1..10
  n = 1: 1/6 (0.16666666666666666)
  n = 2: 5/36 (0.1388888888888889)
  n = 3: 25/216 (0.11574074074074074)
  n = 4: 125/1296 (0.09645061728395062)
  n = 5: 625/7776 (0.08037551440329219)
  n = 6: 3125/46656 (0.06697959533607682)
  n = 7: 15625/279936 (0.05581632944673068)
  n = 8: 78125/1679616 (0.04651360787227557)
  n = 9: 390625/10077696 (0.03876133989356297)
  n = 10: 1953125/60466176 (0.03230111657796914)

  Cf gamble_rolling_dice4.rkt for an alternative approach.

  This is a port of my WebPPL model rolling_dice4_2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; Closed form of the probability
;; We start at time 1 here as well.
;;  (1/6.0) * ((5/6.0)^(n-1));
(define (closed-form n) 
  (*  (/ 6) (expt (/ 5 6) (sub1 n))))


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define d '(1 2 3 4 5 6))
   (define (roll arr)
     (if (and (> (length arr) 0) (= (last arr) 6))
         arr
         (roll (append arr (list (uniform-draw d))))))
   
   (define a (roll '()))
   (define len (length a))
   
   (list len
         )

   )
  )

(show-marginals (model)
                (list  "len"
                       )
                #:num-samples 100000
                #:truncate-output 10
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(displayln "Closed form 1..10")
(for ([n (range 1 11)])
  (let ([ret (closed-form n)])
    (displayln (format "n = ~a: ~a (~a)" n ret (* 1.0 ret)))
    ))
(newline)

