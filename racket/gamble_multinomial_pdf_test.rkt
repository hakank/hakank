#| 

  Multinomial PDF in Racket.Gamble 

  Some tests of multinomial_pdf, mean, and variance.

  (multinomial_objects_3_pdf 20 10  3 5 10   3 5 1): 5/46189
  (multinomial_objects_2_pdf 120 10   50 40   2 7): 26250/4446889

  (multinomial_pdf 10 1/2 1/3   4 6): 35/1944
  (multinomial_mean 10 1/2 1/3): (5 10/3)
  (multinomial_variance 10 1/2 1/3): (5/2 20/9)

  (multinomial_pdf 10 2/10 8/10   3 7): 393216/1953125
  (multinomial_mean 10 2/10 8/10): (2 8)
  (multinomial_variance 10 2/10 8/10): (8/5 8/5)

  (multinomial_pdf 10 1/2 1/3 1/6 8 1 1): 5/256
  (multinomial_mean 10 (1/2 1/3 1/6) (8 1 1)): (5 10/3 5/3)
  (multinomial_variance 10 (1/2 1/3 1/6) (8 1 1)): (5/2 20/9 25/18)

  (multinomial_pdf 10 (1/2 1/3 1/12 1/12) (6 2 1 1)): 35/1152
  (multinomial_mean 10 (1/2 1/3 1/12 1/12)): (5 10/3 5/6 5/6)
  (multinomial_variance 10 (1/2 1/3 1/12 1/12)): (5/2 20/9 55/72 55/72)

  All combinations for (multinomial_pdf 10 (list 4/10 4/10 2/10):
  (part (0 0 10) 1.024e-7)
  (part (0 1 9) 2.048e-6)
  (part (0 2 8) 1.8432e-5)
  (part (0 3 7) 9.8304e-5)
  (part (0 4 6) 0.000344064)
  (part (0 5 5) 0.0008257536)
  (part (0 6 4) 0.001376256)
  (part (0 7 3) 0.001572864)
  (part (0 8 2) 0.001179648)
  (part (0 9 1) 0.000524288)
  (part (0 10 0) 0.0001048576)
  (part (1 0 9) 2.048e-6)
  (part (1 1 8) 3.6864e-5)
  (part (1 2 7) 0.000294912)
  (part (1 3 6) 0.001376256)
  (part (1 4 5) 0.004128768)
  (part (1 5 4) 0.008257536)
  (part (1 6 3) 0.011010048)
  (part (1 7 2) 0.009437184)
  (part (1 8 1) 0.004718592)
  (part (1 9 0) 0.001048576)
  (part (2 0 8) 1.8432e-5)
  (part (2 1 7) 0.000294912)
  (part (2 2 6) 0.002064384)
  (part (2 3 5) 0.008257536)
  (part (2 4 4) 0.02064384)
  (part (2 5 3) 0.033030144)
  (part (2 6 2) 0.033030144)
  (part (2 7 1) 0.018874368)
  (part (2 8 0) 0.004718592)
  (part (3 0 7) 9.8304e-5)
  (part (3 1 6) 0.001376256)
  (part (3 2 5) 0.008257536)
  (part (3 3 4) 0.02752512)
  (part (3 4 3) 0.05505024)
  (part (3 5 2) 0.066060288)
  (part (3 6 1) 0.044040192)
  (part (3 7 0) 0.012582912)
  (part (4 0 6) 0.000344064)
  (part (4 1 5) 0.004128768)
  (part (4 2 4) 0.02064384)
  (part (4 3 3) 0.05505024)
  (part (4 4 2) 0.08257536)
  (part (4 5 1) 0.066060288)
  (part (4 6 0) 0.022020096)
  (part (5 0 5) 0.0008257536)
  (part (5 1 4) 0.008257536)
  (part (5 2 3) 0.033030144)
  (part (5 3 2) 0.066060288)
  (part (5 4 1) 0.066060288)
  (part (5 5 0) 0.0264241152)
  (part (6 0 4) 0.001376256)
  (part (6 1 3) 0.011010048)
  (part (6 2 2) 0.033030144)
  (part (6 3 1) 0.044040192)
  (part (6 4 0) 0.022020096)
  (part (7 0 3) 0.001572864)
  (part (7 1 2) 0.009437184)
  (part (7 2 1) 0.018874368)
  (part (7 3 0) 0.012582912)
  (part (8 0 2) 0.001179648)
  (part (8 1 1) 0.004718592)
  (part (8 2 0) 0.004718592)
  (part (9 0 1) 0.000524288)
  (part (9 1 0) 0.001048576)
  (part (10 0 0) 0.0001048576)
  sum: 1.0


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(show "(multinomial_objects_3_pdf 20 10  3 5 10   3 5 1)" (multinomial_objects_3_pdf 20 10  3 5 10   3 5 1))
(show "(multinomial_objects_2_pdf 120 10   50 40   2 7)" (multinomial_objects_2_pdf 120 10   50 40   2 7))
(newline)

;
; CDF
;
; Interestingly, Mathematica gives the same result for PDF and CDF.
; Why? Isn't a CDF defined for Multinomial?
; Idea from
; https://stackoverflow.com/questions/31934245/functions-to-give-quantile-and-probability-for-multinomial-distribution-in-r
; Generate all pdf for  i=1..ps.length: x[i] <= xs[i]  and sum(x[i]) = 1, and sums to n
; Well, this shows the same result: The only instance where the pdf > 0 is when the generated values == xs!
; -> The CDF == PDF.
#|
(define (multinomial_cdf_test n ps xs)
  (show2 "multinomial_cdf_test" n ps xs )
  (let* ([len (length xs)]
         [partitions (apply cartesian-product (for/list ([i len]) (range (add1 n))))]
         )
    (show2 "partitions" partitions)
    (for/sum ([pp partitions]
              #:when (and (<= (sum pp) n)
                          (for/and ([i len])
                            (<= (list-ref pp i) (list-ref xs i))))
                          )
      (show2 "pp" pp "pdf" (multinomial_pdf n ps pp))
      (multinomial_pdf n ps pp)
      )
    )
  )
|#

(show "(multinomial_pdf 10 1/2 1/3   4 6)" (multinomial_pdf 10 (list 1/2 1/3) (list 4 6)))
(show "(multinomial_mean 10 1/2 1/3)" (multinomial_mean 10 (list 1/2 1/3)))
(show "(multinomial_variance 10 1/2 1/3)" (multinomial_variance 10 (list 1/2 1/3)))
(newline)
(show "(multinomial_pdf 10 2/10 8/10   3 7)" (multinomial_pdf 10 (list 2/10 8/10)   (list 3 7)))
(show "(multinomial_mean 10 2/10 8/10)" (multinomial_mean 10 (list 2/10 8/10)))
(show "(multinomial_variance 10 2/10 8/10)" (multinomial_variance 10 (list 2/10 8/10)))

(newline)
(show "(multinomial_pdf 10 1/2 1/3 1/6 8 1 1)" (multinomial_pdf 10 (list 1/2 1/3 1/6) (list 8 1 1)))
(show "(multinomial_mean 10 (1/2 1/3 1/6) (8 1 1))" (multinomial_mean 10 (list 1/2 1/3 1/6)))
(show "(multinomial_variance 10 (1/2 1/3 1/6) (8 1 1))" (multinomial_variance 10 (list 1/2 1/3 1/6)))
(newline)
(show "(multinomial_pdf 10 (1/2 1/3 1/12 1/12) (6 2 1 1))" (multinomial_pdf 10 (list 1/2 1/3 1/12 1/12) (list 6 2 1 1)))
(show "(multinomial_mean 10 (1/2 1/3 1/12 1/12))" (multinomial_mean 10 (list 1/2 1/3 1/12 1/12)))
(show "(multinomial_variance 10 (1/2 1/3 1/12 1/12))" (multinomial_variance 10 (list 1/2 1/3 1/12 1/12)))
(newline)
(displayln "All combinations for (multinomial_pdf 10 (list 4/10 4/10 2/10):")
(show "sum" (for/sum ([part (for*/list ([p (cartesian-product (range 11) (range 11) (range 11))] #:when (= (sum p) 10)) p) ])
  (show2 "part" part (* 1.0 (multinomial_pdf 10 (list 4/10 4/10 2/10) part)))
  (* 1.0 (multinomial_pdf 10 (list 4/10 4/10 2/10) part))))

