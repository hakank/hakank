#| 

  Tourist with a short memory in Racket.Gamble 

  From Gunnar Blom, Lars Holst, Dennis Sandell:
  "Problems and Snapshots from the World of Probability"
  page 2, Problem 1.2

  A tourist wants to visits all the four cities, A, B, C, and D. 
  When he is in A he then visit the other cities (B, C, or D) 
  with equal probability, and the same for the other cities.
  Unfortunately, he don't remember which cities he has already visited
  so he always select the other 3 cities woth equal probability.

  How many cities will he visit until he had visited all four cities?

  The text state the "theoretical" expectation (geometry distribution) as:
  """
    E(N) = 1 + 1 + 3/2 + 3 = 13/2 (= 6.5)
  """

var : len
5: 0.2230900000000001
4: 0.22229000000000007
6: 0.17127000000000006
7: 0.12476000000000004
8: 0.08513000000000003
9: 0.05695000000000002
10: 0.038190000000000016
11: 0.025480000000000006
12: 0.018210000000000007
13: 0.011570000000000004
14: 0.0077300000000000025
15: 0.005250000000000002
16: 0.003260000000000001
17: 0.002300000000000001
18: 0.0014200000000000005
19: 0.0008900000000000003
20: 0.0007800000000000002
21: 0.0005000000000000002
22: 0.0002600000000000001
23: 0.0002100000000000001
24: 0.00012000000000000004
25: 0.00010000000000000003
26: 8.000000000000003e-5
27: 5.0000000000000016e-5
28: 3.000000000000001e-5
29: 2.000000000000001e-5
30: 2.000000000000001e-5
32: 1.0000000000000004e-5
34: 1.0000000000000004e-5
36: 1.0000000000000004e-5
31: 1.0000000000000004e-5
mean: 6.499760000000005

var : theoretical
13/2: 1.0000000000000004
mean: 6.500000000000003



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;;
;; The theoretical expectation according to the cited text.
;; 
(define (theoretical numCities)
  ; return 1 + (numCities-1)*sum(mapN(function(i) ( return 1/(1+i) ),numCities-1));
  (+ 1 (* (sub1 numCities) (for/sum ([i (sub1 numCities)])  (/ 1 (add1 i) ))))
)
    
(define (model numCities)
  (; enumerate ; #:limit 1e-03
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define cities (range numCities))
   
   (define (visitCities a)
     (let ([len (length a)])
       (if (= (length (remove-duplicates a)) numCities)
           a
           (if (= len 0)
               (visitCities (append a (list (uniform-draw cities))))
               (let* ([lastCity (last a)]
                      [nextCity (uniform-draw (remove lastCity cities))])
                 (visitCities (append a (list nextCity))))))))
   
   (define a (visitCities '()))
   (define len (length a))
   (define theo (theoretical numCities))
   
   (list ; a
         len
         theo
         )

   )
)

(show-marginals (model 4)
                (list  ; "a"
                       "len"
                       "theoretical"
                     )
                    #:num-samples 100000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


