#| 

  German tank problem in Racket Gamble.

  https://en.wikipedia.org/wiki/German_tank_problem
  """
  In the statistical theory of estimation, the German tank problem consists of 
  estimating the maximum of a discrete uniform distribution from sampling without 
  replacement. In simple terms, suppose there exists an unknown number of items 
  which are sequentially numbered from 1 to N. A random sample of these items 
  is taken and their sequence numbers observed; the problem is to estimate N 
  from these observed numbers.
  """

  Cf gample_locomotive_prpblem.rkt (which has a single observation: 60).
  This current model is a generalization which can handle multiple observations.

(281 : 0.04204871982177985)
(259 : 0.03884076207819618)
(298 : 0.03324397916889658)
(283 : 0.0272484031779693)
(288 : 0.025404862442513623)
(303 : 0.0207355846368708)
(257 : 0.02003199991853187)
...
(1988 : 5.59488652774626e-6)
(1991 : 5.561241602623976e-6)
(1994 : 5.52784920297809e-6)
(mean: 380.4113373260534)
Min: 257 Mean: 332.574 Max: 1411 Variance: 9466.610524 Stddev: 97.29650828267168
Credible interval (0.84): 257..393



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (german-tank-problem observations max-int )
  (; enumerate ; Too slow
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define len (length observations))
   (define max-obs (apply max observations))

   (define N (add1 (random-integer max-int))) ; the target value
   ;(define N (+ max-obs (random-integer (- max-int max-obs)))) ; the target value
   ; (define N (discrete-uniform-dist max-int)) ; the target value   
   ; (define N (categorical-dist (make-vector max-int (/ max-int))))
  
   (define y (for/list ([i (range len)])
               ; (random-integer N))
               (categorical-dist (make-vector N (/ N))))
     )

   ; (writeln (list "Test" N))   
   (for ([i (range len)])
     ; (observe/fail (= (list-ref observations i) (list-ref y i)))
     ; (observe (sample (list-ref y i)) (list-ref observations i) )
     (observe-sample (list-ref y i) (list-ref observations i))
     )

   (observe/fail (>= N max-obs))
   
   N)
  
  )

(show-model (german-tank-problem '(10 256 202 97) 2000))

; For the locomotive problem (see gamble_locomotive_problem.rkt):
; -> (mean: 339.15383756804323)
; (show-model (german-tank-problem '(60) 1000))


;; (for ([max-int '(300 500 1000)])
;;   (displayln (list "max-int" max-int))
;;   (show-model (german-tank-problem '(10 256 202 97) max-int))
;;   (newline)
;; )


(newline)
