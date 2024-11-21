#| 

  Chinese restaurant process in Racket/Gamble 

  https://en.wikipedia.org/wiki/Chinese_restaurant_process
  """
  An equivalent, but subtly different way to define the Chinese restaurant process, is to 
  let new customers choose companions rather than tables.[4] Customer n+1 chooses to sit at 
  the same table as any one of the n seated customers with probability 
    1/(n+theta) 
  or chooses to sit at a new, unoccupied table with probability 
    theta/(n + theta)
 
  Notice that in this formulation, the customer chooses a table without having to count table 
  occupancies- we don't need |b|.

  ... 

  It can be understood as the sum of n independent Bernoulli random variables, each with 
  a different parameter:
  
    K = sum(bi,i=1..n)
    bi ~ Bernoulli theta / (i - 1 + theta)
    PMF:  gamma(theta)
          ------------     * |s(n,k)*theta^k   k = 1..N
          gamma(n + theta)

  where s denotes Stirling numberf of the first kind
  """

  Note: The model below supports two variants:
  - sort-tables? #t: This means that the tables are sort to break symmetries.
                     It make sense if one interpreted this as the customer
                     is seated by an already occupied table (1 / (n + theta)).
                     Using this option, the theoretical value of K is seem 
                     directly as probability on K filled tables (b).

  - sort-tables? #f: Here the tables are not sorted.
                     For this option, it's harder to related the tables (b)
                     and the probabilities.

  * For 10 (n) guests and theta 0.5, using enumerate.
    Note, the "tables" are sorted to break symmetries.


  variable : b
  (1 1 0 0 0 0 0 0 0 0): 29200384/72747675 (0.40139267681063345)
  (1 0 0 0 0 0 0 0 0 0): 65536/230945 (0.2837731927515209)
  (1 1 1 0 0 0 0 0 0 0): 667136/2909907 (0.2292636843720435)
  (1 1 1 1 0 0 0 0 0 0): 9263104/130945815 (0.07073997744792379)
  (1 1 1 1 1 0 0 0 0 0): 32/2431 (0.01316330728095434)
  (1 1 1 1 1 1 0 0 0 0): 48208/31177575 (0.0015462395648154163)
  (1 1 1 1 1 1 1 0 0 0): 16/138567 (0.00011546760772766965)
  (1 1 1 1 1 1 1 1 0 0): 232/43648605 (5.3151755938133645e-6)
  (1 1 1 1 1 1 1 1 1 0): 2/14549535 (1.3746143777103528e-7)
  (1 1 1 1 1 1 1 1 1 1): 1/654729075 (1.5273493085670588e-9)
  (0 0 0 0 0 0 0 0 0 0): 0 (0.0)

  variable : K
  2: 29200384/72747675 (0.40139267681063345)
  1: 65536/230945 (0.2837731927515209)
  3: 667136/2909907 (0.2292636843720435)
  4: 9263104/130945815 (0.07073997744792379)
  5: 32/2431 (0.01316330728095434)
  6: 48208/31177575 (0.0015462395648154163)
  7: 16/138567 (0.00011546760772766965)
  8: 232/43648605 (5.3151755938133645e-6)
  9: 2/14549535 (1.3746143777103528e-7)
  10: 1/654729075 (1.5273493085670588e-9)
  0: 0 (0.0)
  mean: 31037876/14549535 (2.133255530159555)


  * An alternative is to not sort the tables
  variable : b
  (1 0 0 0 0 0 0 0 0 0): 65536/230945 (0.2837731927515209)
  (1 1 0 0 0 0 0 0 0 0): 32768/230945 (0.14188659637576045)
  (1 0 1 0 0 0 0 0 0 0): 16384/230945 (0.07094329818788023)
  (1 0 0 1 0 0 0 0 0 0): 32768/692835 (0.04729553212525349)
  (1 1 1 0 0 0 0 0 0 0): 8192/230945 (0.035471649093940114)
  (1 0 0 0 1 0 0 0 0 0): 8192/230945 (0.035471649093940114)
  (1 0 0 0 0 1 0 0 0 0): 32768/1154725 (0.028377319275152094)
  (1 1 0 1 0 0 0 0 0 0): 16384/692835 (0.023647766062626745)
  (1 0 0 0 0 0 1 0 0 0): 16384/692835 (0.023647766062626745)
  (1 0 0 0 0 0 0 1 0 0): 32768/1616615 (0.02026951376796578)
  (1 1 0 0 1 0 0 0 0 0): 4096/230945 (0.017735824546970057)
  ...
  (1 0 0 1 1 1 1 1 1 1): 8/654729075 (1.221879446853647e-8)
  (1 1 1 0 1 1 1 1 1 1): 2/218243025 (9.164095851402353e-9)
  (1 1 0 1 1 1 1 1 1 1): 4/654729075 (6.109397234268235e-9)
  (1 0 1 1 1 1 1 1 1 1): 2/654729075 (3.0546986171341176e-9)
  (1 1 1 1 1 1 1 1 1 1): 1/654729075 (1.5273493085670588e-9)
  (0 0 0 0 0 0 0 0 0 0): 0 (0.0)

  variable : K
  2: 29200384/72747675 (0.40139267681063345)
  1: 65536/230945 (0.2837731927515209)
  3: 667136/2909907 (0.2292636843720435)
  4: 9263104/130945815 (0.07073997744792379)
  5: 32/2431 (0.01316330728095434)
  6: 48208/31177575 (0.0015462395648154163)
  7: 16/138567 (0.00011546760772766965)
  8: 232/43648605 (5.3151755938133645e-6)
  9: 2/14549535 (1.3746143777103528e-7)
  10: 1/654729075 (1.5273493085670588e-9)
  0: 0 (0.0)
  mean: 31037876/14549535 (2.133255530159555)


  * For n=100, theta 0.1 there's just a few tables needed when the tables are sorted.
    Here using importance-sampler (10000 samples)

   variable : b
   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0): 0.60212
   (1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0): 0.31025
   (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0): 0.07525
   (1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0): 0.01118
   (1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0): 0.00112
   (1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0): 8e-5

  variable : K
  1: 0.60212
  2: 0.31025
  3: 0.07525
  4: 0.01118
  5: 0.00112
  6: 8e-5
  mean: 1.4991700000000003

   The quantile 0.99999 is 6, i.e. 6 tables.
   The quantile 0.999999 is 7.


  See below for probability distribution of K (pdf, cdf, quantile, and mean)


  Also, see gamble_chinese_restaurant_process.rkt for an alternative model
  but that model has different probability "dynamics".

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model [sort-tables? #t])
  (show "sort-tables?" sort-tables?)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define max_guests 10)
   
   (define theta 5/10)
   
   (define b
     (let ([t (for/list ([i max_guests]) (bernoulli (/ theta (+ (add1 i) -1 theta))))])
       (if sort-tables?
           (reverse (sort t <))
           t)))

   ; Number of filled tables
   (define K (sum b)) 
    
   (list  b
          K
          )
   )
)

(for ([sort-tables? '(#t)])
  (show-marginals (model sort-tables?)
                  (list  "b"
                         "K"
                         )
                  #:num-samples 100000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:hpd-interval (list 0.84)
                  ; #:show-histogram? #t2
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 0
                  )
  )


#|

  PDF, CDF, Quantile, and mean of (crp 0.1 10)

  PDF
  crp theta: 0.5 n: 10

  crp_pdf 0.5 10 0: 0
  crp_pdf 0.5 10 1: 0.2837731927515211
  crp_pdf 0.5 10 2: 0.4013926768106336
  crp_pdf 0.5 10 3: 0.22926368437204359
  crp_pdf 0.5 10 4: 0.0707399774479238
  crp_pdf 0.5 10 5: 0.013163307280954346
  crp_pdf 0.5 10 6: 0.001546239564815417
  crp_pdf 0.5 10 7: 0.00011546760772766971
  crp_pdf 0.5 10 8: 5.315175593813367e-6
  crp_pdf 0.5 10 9: 1.3746143777103534e-7
  crp_pdf 0.5 10 10: 1.5273493085670594e-9

  CDF
  crp_cdf 0.5 10 0: 0
  crp_cdf 0.5 10 1: 0.2837731927515211
  crp_cdf 0.5 10 2: 0.6851658695621548
  crp_cdf 0.5 10 3: 0.9144295539341983
  crp_cdf 0.5 10 4: 0.9851695313821222
  crp_cdf 0.5 10 5: 0.9983328386630765
  crp_cdf 0.5 10 6: 0.9998790782278919
  crp_cdf 0.5 10 7: 0.9999945458356195
  crp_cdf 0.5 10 8: 0.9999998610112133
  crp_cdf 0.5 10 9: 0.9999999984726511
  crp_cdf 0.5 10 10: 1.0000000000000004

  Quantile
  crp_quantile 0.5 10 0.001: 1
  crp_quantile 0.5 10 0.01: 1
  crp_quantile 0.5 10 0.02: 1
  crp_quantile 0.5 10 0.05: 1
  crp_quantile 0.5 10 0.1: 1
  crp_quantile 0.5 10 0.25: 1
  crp_quantile 0.5 10 0.5: 2
  crp_quantile 0.5 10 0.75: 3
  crp_quantile 0.5 10 0.9: 3
  crp_quantile 0.5 10 0.95: 4
  crp_quantile 0.5 10 0.98: 4
  crp_quantile 0.5 10 0.99: 5
  crp_quantile 0.5 10 0.999: 6
  crp_quantile 0.5 10 0.9999: 7
  crp_quantile 0.5 10 0.99999: 7
  crp_quantile 0.5 10 0.999999: 8

  crp_mean 0.5 10: 2.1332555301595546

  0.99999999 quantile for different n (theta 0.5)
  crp_quantile 0.5 3 0.99999999: 3
  crp_quantile 0.5 6 0.99999999: 6
  crp_quantile 0.5 10 0.99999999: 9
  crp_quantile 0.5 20 0.99999999: 11
  crp_quantile 0.5 50 0.99999999: 13
  crp_quantile 0.5 100 0.99999999: 15
  crp_quantile 0.5 150 0.99999999: 15

|#
(displayln "\nPDF, CDF, quantile, mean")
(define qs '(0.001  0.01 0.02 0.05 0.10 0.25 0.50 0.75 0.90 0.95 0.98 0.99 0.999 0.9999 0.99999 0.999999 0.9999999 0.99999999 0.999999999))
(let* ([theta 1/2]
       [n 10]
       ; The probability of higher values is very small
       [max-k (+ 3 (crp_quantile theta n 0.999999999))])
  (displayln (format  "crp theta: ~a n: ~a" theta n))
  (newline)
  (displayln "PDF")
  (for ([k max-k])
        (let ([v (crp_pdf theta n k)])
          (displayln (format  "crp_pdf ~a ~a ~a: ~a (~a)" theta n k v (* 1.0 v)))
          ))
  (newline)
  (displayln "CDF")
  (for ([k max-k])
    (let ([v (crp_cdf theta n k)])
      (displayln (format  "crp_cdf ~a ~a ~a: ~a (~a)" theta n k v (* 1.0 v)))
      ))
  (newline)
  (displayln "Quantile")
  (for ([q qs])
    (displayln (format  "crp_quantile ~a ~a ~a: ~a" theta n q (crp_quantile theta n q)))
    )
  (newline)
  (displayln (format  "crp_mean ~a ~a: ~a" theta n (crp_mean theta n)))

  )
(newline)
(displayln "\n0.99999999 quantile for different n (theta 0.5)")
(for ([n '(3 6 10 20 50 100 150)])
    (displayln (format  "crp_quantile ~a ~a ~a: ~a" 0.5 n 0.99999999 (crp_quantile 0.5 n 0.99999999)))
  )
(newline)


#|
  crp distribution  crp 1/2 10

  variable : c
  2: 0.4077
  1: 0.2849
  3: 0.223
  4: 0.07
  5: 0.0129
  6: 0.0014
  7: 0.0001
  mean: 2.1229000000000005

|#

(displayln "\nModel 2 crp dist")
(define (model2)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define theta 1/2)
   (define n 10)
    
   (define c (crp theta n))

   (list  c
          )
   )
)

(show-marginals (model2)
                (list  "c"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t2
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

