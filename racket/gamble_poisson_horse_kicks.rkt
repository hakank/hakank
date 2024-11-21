#| 

  Horse kicks (Poisson) in Racket/Gamble 

  https://www.randomservices.org/random/data/HorseKicks.html
  """
  The data below give the number of soilders in the Prussian cavalry killed 
  by horse kicks, by corp membership and by year. The years are from 1875 to 1894, 
  and there are 14 different cavalry corps: the first column corresponds to the 
  guard corp and the other columns to corps 1 through 11, 14, and 15. The data are 
  from Distributome project and are derived from the book by Andrews and Herzberg. 
  The original source of the data is the classic book by von Bortkiewicz (references 
  are given below). The data are famous because they seem to fit the Poisson 
  model reasonably well.

  """
  First from Mathematica:
  """
  ...
  FindDistributionParameters(data, PoissonDistribution(lambda))
  -> 
  (lambda -> 0.7)
  """

  Note that 0.7 is the mean of the data (which is the mean of a poisson distribution).

  """
  Table(PDF(PoissonDistribution(0.7), k), (k, 0, 7))
  -> (0.496585, 0.34761, 0.121663, 0.0283881, 0.00496792, 0.000695509, 0.0000811427, 8.11427*10^-6)
  %*280
  (139.044, 97.3307, 34.0658, 7.94868, 1.39102, 0.194743, 0.02272, 0.002272)
  """

  (data length: 280 mean: 0.7)
  variable : lambda_
  0.6995455439913724: 0.0032198254783926904
  0.7004958311335959: 0.0032198002912239105
  0.7006380636554019: 0.0032196965216205937
  0.7014912715903914: 0.0032185287180330232
  0.6981332016204731: 0.003217711053635258
  ...
  3.329982381462198: 4.3369423119353323e-190
  3.3277624762092244: 4.3369423119353323e-190
  3.32933372457079: 4.3369423119353323e-190
  3.3263082718178913: 4.3369423119353323e-190
  0.00036441257125647154: 0.0
  mean: 0.7034772887846339
  HPD interval (0.84): 0.6387131262692461..0.7752522195811528

  variable : post
  0: 0.5029109858472214
  1: 0.3541197680203881
  2: 0.11296503936927431
  3: 0.026340450108363188
  5: 0.002660096643798934
  ...
  4: 0.00040180295754759914
  7: 1.1404712388898487e-13
  8: 7.581903256712064e-52
  9: 9.521903215248542e-69
  10: 6.130151486729685e-165
  mean: 0.6775900344537583
  HPD interval (0.84): 0..2


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; Total number of death by horse kicks per year
; (define data '(3 5 7 9 10 18 6 14 11 9 5 11 15 6 11 17 12 15 8 4))

;; Death per cavalry corp per year
(define data '(
 ;; A B C D E F G H I J K L M N   
    0 0 0 0 0 0 0 1 1 0 0 0 1 0  ; year 1875
    2 0 0 0 1 0 0 0 0 0 0 0 1 1 
    2 0 0 0 0 0 1 1 0 0 1 0 2 0 
    1 2 2 1 1 0 0 0 0 0 1 0 1 0 
    0 0 0 1 1 2 2 0 1 0 0 2 1 0 
    0 3 2 1 1 1 0 0 0 2 1 4 3 0 
    1 0 0 2 1 0 0 1 0 1 0 0 0 0 
    1 2 0 0 0 0 1 0 1 1 2 1 4 1 
    0 0 1 2 0 1 2 1 0 1 0 3 0 0 
    3 0 1 0 0 0 0 1 0 0 2 0 1 1 
    0 0 0 0 0 0 1 0 0 2 0 1 0 1 
    2 1 0 0 1 1 1 0 0 1 0 1 3 0 
    1 1 2 1 0 0 3 2 1 1 0 1 2 0 
    0 1 1 0 0 1 1 0 0 0 0 1 1 0 
    0 0 1 1 0 1 1 0 0 1 2 2 0 2 
    1 2 0 2 0 1 1 2 0 2 1 1 2 2 
    0 0 0 1 1 1 0 1 1 0 3 3 1 0 
    1 3 2 0 1 1 3 0 1 1 0 1 1 0 
    0 1 0 0 0 1 0 2 0 0 1 3 0 0 
    1 0 0 0 0 0 0 0 1 0 1 1 0 0  ; year 1894
    ))

(show2 "data length:" (length data) "mean:" (* 1.0 (avg data)))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define lambda_ (uniform (apply min data) (apply max data)))
   ; (define lambda_ (normal (avg data) 0.1)) ; A little more informative prior
    
   (for ([i (length data)]) 
     (observe-sample (poisson-dist lambda_) (list-ref data i)))

   (define post (poisson lambda_))

   (list lambda_
         post
         )
   )
)

(show-marginals (model)
                (list  "lambda_"
                       "post"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

(newline)

; Estimated
(define est_lambda (vals*weights (first (get-probs (model) #:ix 0 #:num-samples 1000))))
(show "est_lambda" est_lambda)




#|
  Number of deaths
  https://mindyourdecisions.com/blog/2013/06/21/what-do-deaths-from-horse-kicks-have-to-do-with-statistics/
  From the book Bulmer "Principles of Statistics":
  https://www.google.se/books/edition/Principles_of_Statistics/BZi8AQAAQBAJ?hl=sv&gbpv=1&dq=prussian+soldier+horse+deaths+poisson&pg=PT125&printsec=frontcover&bshm=rimc/1

  Num deaths  Occurrences   Expected frequence (from the book)
  ------------------------------------------------------------
   0           144            139
   1            91             97 
   2            32             34
   3            11             8
   4             2             1
  >5             0             0

  Output:
  est_lambda: 0.7034923740254868

  Histogram data:
  #hash((0 . 144) (1 . 91) (2 . 32) (3 . 11) (4 . 2))

  Histogram poisson(0.7034923740254868)
  #hash((0 . 137) (1 . 102) (2 . 31) (3 . 8) (4 . 2))

  Histogram poisson(0.7) from Mathematica:
  #hash((0 . 134) (1 . 104) (2 . 34) (3 . 6) (4 . 2))

  poisson_pdf 0.7034923740254868 k
  (0 0.49485406699782575)
  (1 0.3481260623884677)
  (2 0.12245201504490395)
  (3 0.028714686256048037)
  (4 0.005050140700916063)
  (5 0.0007105470941700353)
  (6 8.331074368909823e-5)

  280*poisson_dist_pdf 0.7034923740254868 k
  (0 138.5591387593912)
  (1 97.47529746877096)
  (2 34.286564212573104)
  (3 8.04011215169345)
  (4 1.4140393962564977)
  (5 0.1989531863676099)
  (6 0.023327008232947505)

  200 * poisson_dist_pdf 0.7 k:
  (0 139.04388506159466)
  (1 97.33071954311626)
  (2 34.06575184009069)
  (3 7.948675429354493)
  (4 1.3910182001370366)
  (5 0.19474254801918506)
  (6 0.02271996393557159)


|#

(displayln "\nHistogram data:")
(displayln (collect data))
(displayln (format "\nHistogram poisson(~a)" est_lambda))
(displayln (collect (repeat (lambda () ( poisson est_lambda)) (length data))))

(displayln "\nHistogram poisson(0.7) from Mathematica:")
(displayln (collect (repeat (lambda () (poisson 0.7)) (length data))))

(displayln (format "\npoisson_dist_pdf ~a k" est_lambda))
(for ([k 7]) (displayln (list k (poisson_dist_pdf est_lambda k))))

(displayln (format "\n280*poisson_dist_pdf ~a k" est_lambda))
(for ([k 7]) (displayln (list k (* 280 (poisson_dist_pdf est_lambda k)))))

(displayln "\n200 * poisson_dist_pdf 0.7 k:")
(for ([k 7]) (displayln (list k (* 280 (poisson_dist_pdf 0.7 k)))))

(newline)
