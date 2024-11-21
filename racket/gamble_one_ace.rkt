#| 

  One ace in Racket/Gamble 

  From Statistics101 (Resampling Stats)
  oneAce.txt
  """
  What is the probability of getting exactly one ace of any suit in a 5-card hand?
  ->
  prob: 0.3001
  prob: 0.3073
  prob: 0.2919
  """

  Enumerate: 

  variable : s
  0: 35673/54145 (0.6588419983377967)
  1: 3243/10829 (0.2994736356080894)
  2: 2162/54145 (0.03992981808107859)
  3: 94/54145 (0.0017360790470034167)
  4: 1/54145 (1.846892603195124e-5)
  mean: 5/13 (0.38461538461538464)

  variable : p
  #f: 7586/10829 (0.7005263643919106)
  #t: 3243/10829 (0.2994736356080894)
  mean: 3243/10829 (0.2994736356080894)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define suite (range 1 14))
(define deck (flatten (rep 4 suite)))

(define (model)
  (enumerate

   (define hand (draw-without-replacement 5 deck))
   (define s (count-occurrences 1 hand)) ; how many aces?
   (define p (= s 1))
   (list s p)
   )
)

(show-marginals (model)
                (list  "s" "p"))
