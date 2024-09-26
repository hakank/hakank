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
  1: 0.16699497045265918
  2: 0.1396774290313871
  3: 0.11526962573368392
  4: 0.09771120599146073
  5: 0.08023277904988547
  6: 0.06625403713665767
  7: 0.05542501174894257
  8: 0.0462058414742673
  9: 0.03917647411732942
  10: 0.032297093261606444
  ...
  51: 2.9997300242978118e-5
  53: 2.9997300242978118e-5
  54: 1.999820016198541e-5
  46: 9.999100080992705e-6
  48: 9.999100080992705e-6
  52: 9.999100080992705e-6
  56: 9.999100080992705e-6
  60: 9.999100080992705e-6
  61: 9.999100080992705e-6
  63: 9.999100080992705e-6
  mean: 5.991230789228964

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

  Cf gamble_rolling_dice4_2.rkt for an alternative approach.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; Closed form of the probability
;; We start at time 1 here as well.
(define (closed-form n) 
  (*  (/ 6) (expt (/ 5 6) (sub1 n))))


(define (model)
  
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define d '(1 2 3 4 5 6))
   
   (define (dice t)
     (let ([r (uniform-draw d)])
       ;; Always throw first time
       (if (= t 0) 
           (dice (add1 t))
           (if (= r 6)
               t
               (dice (add1 t))))))
   
   (define len (dice 0))
      
   (list len
         )
   
   )
  )

(show-marginals (model)
                (list  "len"
                       )
                #:num-samples 100009
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

