#| 

  8 boys and 2 girls in Racket/Gamble 

  A friend of mine has 10 grandchildren: 8 boys and 2 girls. 
  How unusual/surprising it this?

  Let's assume that the birth of girls is slighly more common than
  the birth of boys: 0.51 vs 0.40.

  The probability of 8 boys of 10 children is about 3.9%.
  * p: the (exact) result of the model:                   0.038897483585189546
  * p-binomial: the exact result from (binomial 10 0.49): 0.038897483585189484

  var : num-boys
  5: 0.2456019560925314
  4: 0.2130221047741344
  6: 0.19664208902833397
  3: 0.12669536260619083
  7: 0.10796036260379126
  2: 0.04944997571108986
  8: 0.038897483585189546
  1: 0.011437409348143234
  9: 0.0083049093493433
  0: 0.0011904242382761324
  10: 0.0007979226629761214
  mean: 4.899999999999999
  Credible interval (0.94): 2..7
  Percentiles:
  (0.01 2)
  (0.025 2)
  (0.1 3)
  (0.05 2)
  (0.25 4)
  (0.5 5)
  (0.75 6)
  (0.84 6)
  (0.9 7)
  (0.95 7)
  (0.975 8)
  (0.99 8)
  (0.999 9)
  Histogram:
  1: 7  
  2: 45 
  3: 148
  4: 233
  5: 243
  6: 189
  7: 94 
  8: 37 
  9: 4  

  var : num-girls
  5: 0.2456019560925314
  6: 0.2130221047741344
  4: 0.19664208902833397
  7: 0.12669536260619083
  3: 0.10796036260379126
  8: 0.04944997571108986
  2: 0.038897483585189546
  9: 0.011437409348143234
  1: 0.0083049093493433
  10: 0.0011904242382761324
  0: 0.0007979226629761214
  mean: 5.1000000000000005
  Credible interval (0.94): 2..7
  Percentiles:
  (0.01 2)
  (0.025 2)
  (0.1 3)
  (0.05 3)
  (0.25 4)
  (0.5 5)
  (0.75 6)
  (0.84 7)
  (0.9 7)
  (0.95 8)
  (0.975 8)
  (0.99 8)
  (0.999 9)
  Histogram:
  1: 4  
  2: 37 
  3: 94 
  4: 189
  5: 243
  6: 233
  7: 148
  8: 45 
  9: 7  

  var : p
  #f: 0.9611025164148105
  #t: 0.038897483585189546
  mean: 0.038897483585189546
  Histogram:
  #f: 963
  #t: 37 

  var : p-binomial
  0.038897483585189484: 1.0
  mean: 0.038897483585189484


  Compare with some other rather unsusual distrobutions:
  n:10 num-boys:1: 0.011437409348143213
  n:10 num-boys:0: 0.0011904242382761302
  n:6 num-boys:1: 0.101437423794

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model [p-boy 0.49] [p-girl 0.51] [n 10] [observed-num-boys 8])
  (show2 "p-boy" p-boy "p-girl" p-girl "n" n "observed-num-boys"2)
  (enumerate
  
   (define (child i) (categorical-vw2 (vector p-boy p-girl) (vector "boy" "girl")))
   (define children (for/list ([i n]) (child i)))

   (define num-boys (count-occurrences-eq "boy" children))
   (define num-girls (count-occurrences-eq "girl" children))

   (define p (= num-boys observed-num-boys))
   (define p-binomial (dist-pdf (binomial-dist n p-boy) observed-num-boys))
   
   (list num-boys
         num-girls
         p
         p-binomial)
         
    

   )
)

(show-marginals (model)
                (list  "num-boys"
                       "num-girls"
                       "p"
                       "p-binomial"
                       )
                #:credible-interval 0.94
                #:show-histogram? #t
                #:show-percentiles? #t
                )

(newline)
; Some other distributions of n and number of boys
(displayln (format "n:10 num-boys:1: ~a"(binomial_dist_pdf 10 0.49 1)))
(displayln (format "n:10 num-boys:0: ~a"(binomial_dist_pdf 10 0.49 0)))
(displayln (format "n:6 num-boys:1: ~a"(binomial_dist_pdf 6 0.49 1)))

