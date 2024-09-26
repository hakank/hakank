#| 

  Meeting collegues at office in Racket.Gamble 

  https://www.reddit.com/r/Probability/comments/1fksuqo/probably_of_two_colleagues_being_in_the_office_on/
  """
  Probably of two colleagues being in the office on the same days

  If two different colleagues independently chose 3 days to go into an office
  Whats the probability that At least 1 match at least 2 match At least 3 match
  1 is 100% as no matter what you will have 1 matching day but beyond that 
  I’m slightly stumped
  """

  * 2 collegues
  For a 5 day week and 3 days in office:

  2: 3/5 (0.6)
  1: 3/10 (0.3)
  3: 1/10 (0.1)
  mean: 9/5 (1.8)

  For a 10 day "week" and 5 days in office (importance-sampler):
  var : num-matches
  2: 0.39752
  3: 0.3964
  4: 0.09978
  1: 0.09765
  5: 0.00444
  0: 0.00421
  mean: 2.50321


  For a 30 day month and 10 days in office
  var : num-matches
  3: 0.3092
  4: 0.2697
  2: 0.1875
  5: 0.1322
  1: 0.0562
  6: 0.0325
  0: 0.0071
  7: 0.0055
  8: 0.0001
  mean: 3.3329

  * 3 collegues
  For a 5 day week and 3 days in office:
  var : num-matches
  1: 57/100 (0.57)
  2: 6/25 (0.24)
  0: 9/50 (0.18)
  3: 1/100 (0.01)
  mean: 27/25 (1.08)

   
  * 4 collegues
  For a 5 day week and 3 days in office:
  var : num-matches
  1: 0.4847
  0: 0.4364
  2: 0.0773
  3: 0.0016
  mean: 0.6441


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")
(require racket/set)

;
; Generalized version: Probability that all colleques meet at some day
;
(define (model)
  (; enumerate
   importance-sampler

   (define num-people 2)
   
   (define num-days-in-week 5) ; work days
   (define num-in-office 3)

   ; Use importance-sampler for these:
   ; (define num-days-in-week 10) 
   ; (define num-in-office 5)
   
   ; (define num-days-in-week 30) 
   ; (define num-in-office 10)


   (defmem (people p) (draw-without-replacement num-in-office (range num-days-in-week)))
   (define all-people (for/list ([p num-people]) (people p))) 

   (define num-matches (length (apply set-intersect all-people)))
   
   (list ; all-people
         num-matches)
   )
)

(show-marginals (model)
                (list  ; "people"
                       "num-matches")
                #:num-samples 10000
                )
