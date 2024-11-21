#| 

  Surfing the web in Racket/Gamble 

  From an exercise in the Wolfram U's Introduction to Probability
  """
  While surfing the web, you encounter ad A 7 times and ads B and C 3 times each. 
  How many arrangements are possible?
  """

  See comments here: 
  https://community.wolfram.com/groups/-/m/t/2848381
  
  The answer should be 
  In(1):= 13!/(7!3!3!)
  Out(1)= 34320

  * Using show-marginals, we see that it's 34320 different solutions
  variable : v
  (c c b a a c b a b a a a a): 1/34320 (2.9137529137529138e-5)
  (b a a a b a a c a c b c a): 1/34320 (2.9137529137529138e-5)
  (b b a a c a a c b a a a c): 1/34320 (2.9137529137529138e-5)
  (b a b a c a c c b a a a a): 1/34320 (2.9137529137529138e-5)
  (b a a a c a a b c a a c b): 1/34320 (2.9137529137529138e-5)
  ...
  (a c c a b a a a b a c b a): 1/34320 (2.9137529137529138e-5)
  (b a a c b b a c a a a a c): 1/34320 (2.9137529137529138e-5)
  (c a b b a a a a b a a c c): 1/34320 (2.9137529137529138e-5)
  (a b a a c c a c a a a b b): 1/34320 (2.9137529137529138e-5)
  (c a b a c c a b a a a b a): 1/34320 (2.9137529137529138e-5)

  * Another approach
    Counting the different solutions using discrete-dist-values: 34320

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate

   (define a 7)
   (define b 3)
   (define c 3)
   (define n (+ a b c))

   (define v (for/list ([i n])
               (categorical-vw2 (vector (/ a n) (/ b n) (/ c n)) (vector "a" "b" "c"))))

   ; Enforce the specific distribution of the characters
   (define num_a (count-occurrences-eq "a" v))
   (define num_b (count-occurrences-eq "b" v))
   (define num_c (count-occurrences-eq "c" v))
   
   (observe/fail (and (= num_a a) (= num_b b) (= num_c c)))
   ; (observe/fail (equal? (list a b c) (list num_a num_b num_c)))

   (list v
         )

   )
)

(show-marginals (model)
              (list  "v"))

(displayln (format "\nCounting the different solutions using discrete-dist-values: ~a"
                   (vector-length (discrete-dist-values (model)))))
