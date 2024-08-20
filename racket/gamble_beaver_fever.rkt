#| 

  Beaver fever problem in Racket Gamble.

  From https://x.com/madeofmistak3/status/1818764350724944062
  """
  I have a question. Exactly 1/5th of the people in a town have Beaver Fever . There are 
  two tests for Beaver Fever, TEST1 and TEST2. When a person goes to a doctor to test for 
  Beaver Fever, with probability 2/3 the doctor conducts TEST1 on him and with probability 
  1/3 the doctor conducts TEST2 on him. When TEST1 is done on a person, the outcome is as 
  follows: If the person has the disease, the result is positive with probability 3/4. If 
  the person does not have the disease, the result is positive with probability 1/4. When 
  TEST2 is done on a person, the outcome is as follows: If the person has the disease, 
  the result is positive with probability 1. If the person does not have the disease, the 
  result is positive with probability 1/2. A person is picked uniformly at random from 
  the town and is sent to a doctor to test for Beaver Fever. The result comes out 
  positive. What is the probability that the person has the disease? 
  """

  The problem is from
  http://allendowney.blogspot.com/2017/02/a-nice-bayes-theorem-problem-medical.html
 

  Enumerate (exact):
  (#f : 8/13 (0.6153846153846154))
  (#t : 5/13 (0.38461538461538464))


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(show-model
 (enumerate
  (define sick (flip 1/5)) 
  (define positive (if (flip 2/3)
                       (if sick (flip 3/4) (flip 1/4))  ; TEST 1
                       (if sick  1         (flip 1/2))) ; TEST 2
    )
 (observe/fail positive)
 sick
 )
 )



