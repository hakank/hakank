#| 

  Locomotive problem in Racket Gamble.

  From Think Bayes, page 22ff
  """
  I found the locomotive problem in Frederick Mosteller's "Fifty Challenging
  Problems in Probability with Solutions" (Dover, 1987):
     'A railroad numbers its locomotives in order 1..N. One day you
      see a locomotive with the number 60. Estimate how many loco-
      motives the railroad has.'

  ...

  The mean of the posterior is 333, so that might be a good guess if you wanted to
  minimize error.
  """

  Port of my WebPPL model locomotive_problem.wppl
   
  As the book (Think Bayes) mentions, note that it's very sensitive to 
  the value of maxInt. Here are some results for different maxInt:
   
  maxInt  N
   -------------------------
    100    77.8166306057923
    200   115.84577808279282
    500   207.2393675826458
   1000   334.04414386751915 
   2000   553.5237331955558

  Mosteller's solution: 2*(60-1)+1: 119.


  This model (enumerate) mirrors the values from the book:
  max-int  (mean N)
  --------------------------
  100     77.81663060579233
  200    115.84577808279299
  500    207.23936758264531
 1000    334.04414386751756
 2000    553.5237331955535

  For max-int 100:
(60 : 0.03230189778925914)
(61 : 0.031780899437819476)
(62 : 0.03127644071658426)
(63 : 0.030787746330387635)
(64 : 0.03031408869453552)
(65 : 0.029854784320375916)
(66 : 0.02940919052454938)
(67 : 0.028976702428600107)
(68 : 0.02855675021948996)
(69 : 0.028148796644925818)
(70 : 0.027752334720349405)
(71 : 0.02736688562701122)
(72 : 0.026991996782805616)
(73 : 0.026627240069524417)
(74 : 0.026272210201930754)
(75 : 0.025926523225589602)
(76 : 0.025589815131750723)
(77 : 0.025261740578779588)
(78 : 0.02494197171069377)
(79 : 0.024630197064310088)
(80 : 0.0243261205573433)
(81 : 0.02402946055054643)
(82 : 0.0237399489776483)
(83 : 0.023457330537438176)
(84 : 0.02318136194288011)
(85 : 0.022911811222614032)
(86 : 0.022648457070629995)
(87 : 0.022391088240281878)
(88 : 0.02213950297915517)
(89 : 0.021893508501608958)
(90 : 0.021652920496096816)
(91 : 0.021417562664617456)
(92 : 0.02118726629187963)
(93 : 0.020961869841966043)
(94 : 0.020741218580471663)
(95 : 0.0205251642202584)
(96 : 0.020313564589121712)
(97 : 0.02010628331780417)
(98 : 0.019903189546917256)
(99 : 0.019704157651448086)
(mean: 77.81663060579233)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (locomotive-problem max-int )
  (enumerate
   
   (define obs 60)
   
   ; Using float for the prior probability -> faster and neater output.
   ; Note: this is for 0..max-int-1 (to get the same values as in the book)
   ; With (range 1 (add1 max-int)), the mean is slightly higher
   (define priors (for/list ([i (range max-int)]) (cons i  (/ 1.0 (+ i 1)))))

   ; What is the estimated number of locomotives?
   ; This is value to find out.
   (define N (discrete priors))

   (define locomotive (+ 1 (discrete-uniform max-int)))

   (observe/fail (= locomotive obs))
   (observe/fail (>= N locomotive))

   
   N)
  
  )

  
(for ([max-int '(100 200 500 1000 2000)])
  (displayln (list "max-int" max-int))
  (show-model (locomotive-problem max-int))
  (newline)
)
