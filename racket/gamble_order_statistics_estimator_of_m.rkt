#| 

  Order statistics - estimator of m in Racket/Gamble 

  See gamble_order_statistics_dist.rkt for more on order statistics.

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes"  

  Estimation of an unknown m (number of possible objects).

  (problem Siegrist problem 1 values (51 3 27 82 65))
  u0: (17.0 80.0 101.0 96.5 97.4)
  u: 97.4
  v: 90.2

  (problem Siegrist problem 1 values (304 125 417 226 192 340 468 499 87 352))
  u0: (956.0 686.5 703.0 620.5 667.8 622.3333333333334 552.1428571428571 572.375 571.0 547.9)
  u: 547.9
  v: 601.0

  (problem German tanks problem values (10 256 202 97))
  u0: (49.0 241.5 335.6666666666667 319.0)
  u: 319.0
  v: 281.5

  (problem Locomotive problem values (60))
  u0: (119.0)
  u: 119.0
  v: 119.0

  (problem Random1 values (11 19 4 3 2 6 6 2 1 4))
  u0: (10.0 10.0 6.333333333333333 7.25 7.8 6.333333333333333 8.428571428571429 7.25 12.444444444444445 19.9)
  u: 19.9
  v: 10.6

  (problem Random2 values (12 6 19 7 2 6 10 11 1 1))
  u0: (10.0 4.5 6.333333333333333 15.5 12.2 11.833333333333334 14.714285714285714 14.125 13.666666666666666 19.9)
  u: 19.9
  v: 14.0

  (problem Random3 values (42 149 126 169 179 145 162 134 196 181))
  u0: (461.0 692.0 490.3333333333333 397.75 326.8 296.0 264.57142857142856 245.125 220.22222222222223 214.6)
  u: 214.6
  v: 295.6

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|
  Example
  """
  Suppose that in a certain war, 5 enemy tanks have been captured. The serial numbers 
  are 51, 3, 27, 82, 65. Compute the estimate of m, the total number of tanks, 
  using all of the estimators discussed above.
  Answer
  1. u1 = 17
  2. u2 = 80
  3. u3 = 101
  4. u4 = 96.5
  5. u5 = 97.4

  6. v = 90.2
  """

  Compare with similar problems:
  - gamble_german_tank_problem.rkt
  - gamble_locomotive_problem.rkt

|#

#|
  Siegrist's problem 1

  u0: (17.0 80.0 101.0 96.5 97.4)
  u: 97.4
  v: 90.2
|#
; (define values '(51 3 27 82 65)) 

#|
  Siegrist's problem 2
  """
  Suppose that in a certain war, 10 enemy tanks have been captured. The serial 
  numbers are 304, 125, 417, 226, 192, 340, 468, 499, 87, 352. Compute the estimate of 
  m, the total number of tanks, using the estimator based on the maximum and the
  estimator based on the mean.
  Answer
  1. u = 548
  2. v = 601
  """

  u0: (956.0 686.5 703.0 620.5 667.8 622.3333333333334 552.1428571428571 572.375 571.0 547.9)
  u: 547.9
  v: 601.0

(define values '(304 125 417 226 192 340 468 499 87 352))
|#

#| 
  
  gamble_german_tank_problem.rkt

  u0: (49.0 241.5 335.6666666666667 319.0)
  u: 319.0
  v: 281.5
|#
; (define values '(10 256 202 97))

#|
  gamble_locomotive_problem.rkt

  u0: (119.0)
  u: 119.0
  v: 119.0

|#
; (define values '(60)) 

#|
  * 10 random samples from 1..20000

  values: (9884 14978 17803 13705 4448 10832 2756 1607 9497 10307)
  u0: (17676.0 15157.0 16308.333333333334 26115.75 21743.8 18895.166666666668 17020.714285714286 18843.375 18305.444444444445 19582.3)
  u: 19582.3
  v: 19162.4

  * 10 random samples from 1..20
  values: (7 18 20 15 16 4 0 8 7 4)
  u0: (-1.0 21.0 13.666666666666666 18.25 14.4 13.666666666666666 22.571428571428573 21.0 21.0 21.0)
  u: 21.0
  v: 18.8


  * 5 random samples from 1..200
  values: (154 16 108 83 67)
  u0: (95.0 200.0 165.0 161.0 183.8)
  u: 183.8
  v: 170.2

  * 1 random sample from 1..200
  values: (12)
  u0: (23.0)
  u: 23.0
  v: 23.0

  values: (58)
  u0: (115.0)
  u: 115.0
  v: 115.0

  values: (17)
  u0: (33.0)
  u: 33.0
  v: 33.0

|#
; (define values (for/list ([i 10]) (random-integer (add1 2000))))


(define all-values (list
                    (list "Siegrist problem 1" '(51 3 27 82 65))
                    (list "Siegrist problem 1" '(304 125 417 226 192 340 468 499 87 352))
                    (list "German tanks problem" '(10 256 202 97))
                    (list "Locomotive problem" '(60))
                    (list "Random1" (for/list ([i 10]) (random-integer (add1 20))))                    
                    (list "Random2" (for/list ([i 10]) (random-integer (add1 20))))
                    (list "Random3" (for/list ([i 10]) (random-integer (add1 200))))
                      ))

(for ([v all-values])
  (let* ([problem (first v)]
         [values (second v)]
         [u0 (order_statistics_m_estimator_u_all values)]
         [u (order_statistics_m_estimator_u values (length values))]
         [v (* 1.0 (order_statistics_m_estimator_v values))]
         )
    (show2 "problem" problem "values" values)
    
    (show "u0" u0)
    (show "u" u)
    (show "v" v)
    (newline)
    ))


