#| 

  Test of my distributions in in Racket.Gamble 

  Here are some tests of the distributions in gamble_distributions.rkt


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define ps '(0.0001 0.001 0.01 0.05 0.25 0.5 0.75 0.9 0.95 0.99 0.999 0.9999 0.99999))


#|
  Laplace

|#
(displayln "Testing laplace")

(let ([mu 10]
      [b 3.4])
  (displayln (format "laplace mu:~a b:~a" mu b))
  (displayln "Some samples")
  (displayln (repeat (lambda () (laplace mu b)) 10))
  (for ([x (range 0 10 0.5)])
    (show2 "laplace_pdf" mu b x ":" (laplace_pdf mu b x))
    )
  (newline)
  (for ([x (range 0 10 0.5)])
    (show2 "laplace_cdf" mu b x ":" (laplace_cdf mu b x))
    )
  (newline)
  (for ([q ps])
    (show2 "laplace_quantile" mu b q ":" (laplace_quantile mu b q))
    )
  (newline)
  (displayln (format "laplace_mean ~a ~a: ~a" mu b (laplace_mean mu b)))
  )
(newline)

#|
  Extreme value dist1
  Extreme value dist2

|#
(displayln "Testing extreme_value_dist1")
(displayln "Samples:")
(displayln (repeat (lambda () (extreme_value_dist1)) 10))

(newline)
(displayln "Testing extreme_value_dist2")
(let ([a 1]
      [b 2])
  (displayln (format "extreme_value_dist2 a:~a b:~a" a b))
  
  (for ([q (range -10 10 2)])
    (let ([v (extreme_value_dist2_pdf a b q)])
      (show2 "extreme_value_dist2_pdf " a b q ":" v)
      )
    )
  (newline)
  (for ([q (range -10 10 2)])
    (let ([v (extreme_value_dist2_cdf a b q) ])
      (show2 "extreme_value_dist2_cdf " a b q ":" v)
      )
    )
  (newline)
  (for ([q ps])
    (let ([v (extreme_value_dist2_quantile a b q)])
      (show2 "extreme_value_dist2_quantile " a b q ":" v)
      )
    )
  (newline)
  (displayln (format "extreme_value_dist2_mean ~a ~a: ~a" a b (extreme_value_dist2_mean a b)))
  
  )


#|
  Bernoulli
|#
(displayln "\nTesting Bernoulli")
(let ([p 8/10])
  (displayln (format "bernoulli_dist p:~a" p))

  (for ([q '(0 1)])
    (let ([v (bernoulli_dist_pdf p q)])
      (show2 "bernoulli_dist_pdf " p q ":" v)
      )
    )
  (newline)
  (for ([q '(0 1)])
    (let ([v (bernoulli_dist_cdf p q)])
      (show2 "bernoulli_dist_cdf " p q ":" v)
      )
    )
  (newline)
  (for ([q ps])
    (let ([v (bernoulli_dist_quantile p q)])
      (show2 "bernoulli_dist_quantile " p q ":" v)
      )
    )
  )

#|
  Binomial

|#
(displayln "\nTesting Binomial")
(let ([n 10]
      [p 8/10])
  (displayln (format "binomial_dist n:~a p:~a" n p))

  (for ([q (add1 n)])
    (let ([v (binomial_dist_pdf n p q)])
      (show2 "binomial_dist_pdf " n p q ":" v "(" (* 1.0 v) ")" )
      )
    )
  (newline)
  (for ([q (add1 n)])
    (let ([v (binomial_dist_cdf n p q)])
      (show2 "binomial_dist_cdf " n p q ":" v "(" (* 1.0 v) ")" )
      )
    )
  (newline)
  (for ([q ps])
    (let ([v (binomial_dist_quantile n p q)])
      (show2 "binomial_dist_quantile " n p q ":" v)
      )
    )
  (newline)
  (displayln (format "binomial_dist_mean ~a ~a: ~a" n p (binomial_dist_mean n p)))
  
  )

#|
  Hypergeometric
  - dist
  - pdf
  - cdf
  - quantile

  For a Mathematica compliant variant, see hypergeometric2 below.

|#
(displayln "\nTesting Hypergeometric")
(displayln  "Using (hypergeometric (N: total number of objects
                            K: total number of success objects
                            n: number of draws
                            k: number of successes")

(let ((K 5)  ;; total green marbles: 4 drawn + 1 not drawn
      (N 50) ;; total marbles: 5 green + 45 red marbles
      (k 4)  ;; drawn 4 green_marbles
      ; (k 5) ;; drawn 5 green_marbles
      (n 10))   ;; total drawn green + red marbles
  (show2 "K:" K "N:" N "k:" k "n:" n)
  
  (for* ([kk 6])
    (let ([res (hypergeometric_pdf N K n kk)])
      (show2 "(hypergeometric_pdf" N K n kk "):" res ":(" (exact->inexact res) ")")))
  (newline)
  (for* ([kk 6])
    (let ([res (hypergeometric_cdf N K n kk)])
      (show2 "(hypergeometric_cdf" N K n kk "):" res ":(" (exact->inexact res) ")")))  
  (newline)
  
  (for ([p ps])
    (show2 "(hypergeometric_quantile" N K n p "):" (hypergeometric_quantile N K n p)))

  (newline)
  )
(newline)


#|
  Hypergeometric2 

  This is the Mathematica compliant version.
|#
(displayln "\nTesting Hypergeometric2")
(let ({num-drawn 10}
      [num-successes 40]
      [num-objects 100])
  (show2 "(hypergeometric2 num-drawn:" num-drawn "num-successes:" num-successes "num-objects:" num-objects)
  (newline)
  
  
  (displayln "Frequency of 1000 random samples")
  (show-freq (repeat (lambda () (hypergeometric2 num-drawn num-successes num-objects)) 1000))
  
  (displayln "\nPercentiles (random samples)")
  (percentiles (repeat (lambda () (hypergeometric2 num-drawn num-successes num-objects)) 1000) ps)
  (newline)
  
  (displayln "\nExact calculations:")
  (for ([n 11])
    (let ([res (hypergeometric2_pdf num-drawn num-successes num-objects n)])
      (show2 "(hypergeometric2_pdf" num-drawn num-successes num-objects n "):" res "(" (exact->inexact res) ")")
      )
    )
  (newline)
  (for ([n 11])
    (let ([res (hypergeometric2_cdf num-drawn num-successes num-objects n)])
      (show2 "(hypergeometric2_cdf" num-drawn num-successes num-objects n "):" res "(" (exact->inexact res) ")")
      )
    )
  (newline)
  (for ([p ps])
    (let ([res (hypergeometric2_quantile num-drawn num-successes num-objects p)])
      (show2 "(hypergeometric2_quantile" num-drawn num-successes num-objects p "):" res)
      )
    )
  (newline)
  (displayln (format "hypergeometric2_mean ~a ~a ~a: ~a" num-drawn num-successes num-objects (hypergeometric2_mean num-drawn num-successes num-objects)))
  )


#|
  Negative binomial
|#
(displayln "\nNegative binomial")
(displayln "(negative_binomial n p)")

(let ([n 3]
      [p 0.7])
  (show2 "negative_binomial n:" n "p:" p)
  (for ([k 13])
    (show2 "(negative_binomial_pdf" n p k "):" (negative_binomial_pdf n p k))
    )
  (newline)
  (for ([k 13])
    (show2 "(negative_binomial_cdf" n p k "):" (negative_binomial_cdf n p k))
    )
  (newline)
  (define ps '(0.0001 0.001 0.01 0.05  0.5 0.9 0.95 0.99 0.999 0.9999 0.99999))
  (for ([q ps])
    (show2 "(negative_binomial_quantile" n p q "):" (negative_binomial_quantile n p q))
    )
  (newline)
  (displayln (format "negative_binomial_mean ~a ~a: ~a" n p (negative_binomial_mean n p)))
  )
(newline)

#|
  Beta-binomial 

|#
(displayln "\nBeta binomial")
(displayln "(beta_binomial n a b)")

(let ([n 12]
      [a 10]
      [b 3])
  
  (show2 "beta_binomial n:" n "a:" a "b:" b)
  (show2 (for/list ([i 10]) (beta_binomial n a b)))
  (for ([k 13])
    (let ([v (beta_binomial_pdf n a b k)])
      (show2 "(beta_binomial_pdf" n a b k "):" v (* 1.0 v))
      )
    )
  (newline)
  (for* ([k 13])
    (let ([v (beta_binomial_cdf n a b k)])
      (show2 "(beta_binomial_cdf" n a b k "):" v (* 1.0 v))
      )
    )
  (newline)
  (define ps '(0.0001 0.001 0.01 0.05  0.5 0.9 0.95 0.99 0.999 0.9999 0.99999))
  (for ([q ps])
    (show2 "(beta_binomial_quantile" n a b q "):" (beta_binomial_quantile n a b q))
    )
  (newline)
  (displayln (format "beta_binomial_mean ~a ~a ~a: ~a" n a b (beta_binomial_mean n a b)))  
  )


#|
  Cauchy distribution

  The PDF, CDF, and quantiles for cauchy_dist* and the built-in cauchy are
  almost identical.

|#

(displayln "\ncauchy_dist_pdf(1,2,k):")
(for ([k 11]) (show (* k 20) (cauchy_dist_pdf 1 2 (* k 20))))
(displayln "\ncauchy_dist_cdf(1,2,k):")
(for ([k 11]) (show (* k 20) (cauchy_dist_cdf 1 2 (* k 20))))
(show2 "cauchy_quantile(1 2 0.001:" (cauchy_dist_quantile 1 2 0.001))
(show2 "cauchy_quantile(1 2 0.9:" (cauchy_dist_quantile 1 2 0.9))
(show2 "cauchy_quantile(1 2 0.99:" (cauchy_dist_quantile 1 2 0.99))
(show2 "cauchy_quantile(1 2 0.999:" (cauchy_dist_quantile 1 2 0.999))
(show2 "cauchy_quantile(1 2 0.9999:" (cauchy_dist_quantile 1 2 0.9999))

(displayln "\nbuilt-in cauchy-dist")
(displayln "PDF")
(for ([k 11]) (show (* k 20) (dist-pdf (cauchy-dist 1 2) (* k 20))))
(displayln "\nCDF")
(for ([k 11]) (show (* k 20) (dist-cdf (cauchy-dist 1 2) (* k 20))))
(show2 "cauchy quantile 1 2 0.001:" (dist-inv-cdf (cauchy-dist 1 2) 0.001))
(show2 "cauchy quantile 1 2 0.9:" (dist-inv-cdf (cauchy-dist 1 2) 0.9))
(show2 "cauchy quantile 1 2 0.99:" (dist-inv-cdf (cauchy-dist 1 2) 0.99))
(show2 "cauchy quantile 1 2 0.999:" (dist-inv-cdf (cauchy-dist 1 2) 0.999))
(show2 "cauchy quantile 1 2 0.9999:" (dist-inv-cdf (cauchy-dist 1 2) 0.9999))


#| 

  Chi distribution
|#

(displayln "\nChi distribution")
(let ([k 2])
  (let ([samples (repeat (lambda () (chi_dist k)) 20)])
    (show2 "samples" samples)
    (show-stats samples)
    )

  (displayln "PDF")
  (for ([x (range 0 3.1 0.2)])
    (show2 "chi_dist pdf 2 " x ":" (chi_dist_pdf k x)))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 3.1 0.2)])
    (show2 "chi_dist cdf 2 " x ":" (chi_dist_cdf k x)))
  (newline)
  ; Quantile: TODO.
  (displayln (format "(chi_dist_mean ~a: ~a" k (chi_dist_mean k)))

  )
(newline)
  
#| 
  Chi squared distribution
|#

(displayln "\nChi squared distribution")
(let ([k 2])
  (let ([samples (repeat (lambda () (chi_squared_dist k)) 20)])
    (show2 "samples" samples)
    (show-stats samples)
    )

  (displayln "PDF")
  (for ([x (range 0 3.1 0.2)])
    (show2 "chi_squared_dist pdf 2 " x ":" (chi_squared_dist_pdf k x)))
  (newline)
  (displayln "CDF")
  (for ([x (range 0 3.1 0.2)])
    (show2 "chi_squared_dist cdf 2 " x ":" (chi_squared_dist_cdf k x)))
  (newline)
  ; Quantile: TODO.
  (displayln (format "(chi_squared_dist_mean ~a: ~a" k (chi_squared_dist_mean k)))

  )
(newline)

  
#| 
  Chi squared inverse distribution
|#

(displayln "\nChi squared inverse distribution")
(let ([k 2])
  (let ([samples (repeat (lambda () (chi_squared_inverse_dist k)) 20)])
    (show2 "samples" samples)
    (show-stats samples)
    ))
(newline)
   
#|
  Exponential 

  Note that the parameter lambda is the inverse of the lambda parameter
  for the built-in exponential distribution.

|#
(displayln "Testing exponential")

(let ([lambda_ 1/3000])
  (displayln (format "exponential_dist lambda:~a" lambda_))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (exponential_dist lambda_)) 10))
  (displayln "PDF")
  (for ([x (range 0 5001 300)])
    (show2 "exponential_dist_pdf" lambda_ x ":" (exponential_dist_pdf lambda_ x))
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 0 15001 1000)])
    (show2 "exponential_dist_cdf" lambda_ x ":" (exponential_dist_cdf lambda_ x))
    )
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "exponential_dist_quantile" lambda_ q ":" (exponential_dist_quantile lambda_ q))
    )
  (newline)
  (displayln (format "(exponential_dist_mean ~a: ~a" lambda_ (exponential_dist_mean lambda_)))
  )
(newline)

#|
  Erlang
|#
(displayln "Testing erlang")
(let ([k 7]
      [lambda_ 2])
  (displayln (format "erlang_dist k:~a lambda:~a" k lambda_))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (erlang k lambda_)) 10))
  (displayln "PDF")
  (for ([x (range 0 7 0.5)])
    (show2 "erlang_pdf" k lambda_ x ":" (erlang_pdf k lambda_ x))
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 0 10)])
    (show2 "erlang_cdf" k lambda_ x ":" (erlang_cdf k lambda_ x))
    )
  (newline)
  (displayln "Mean")
  (show2 "erlang_mean" k lambda_ ":" (erlang_mean k lambda_))
  
)
(newline)


#|
  Inverse exponential 

|#
(displayln "Testing inverse_exponential ")
(let ([lambda_ 2])
  (displayln (format "inverse_exponential lambda:~a" lambda_))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (inverse_exponential lambda_)) 10))
)  
(newline)


#|
  Shifted exponential 

|#
(displayln "Testing shifted_exponential ")
(let ([lambda_ 2]
      [t 1])
  (displayln (format "shifted_exponential lambda:~a t: ~a" lambda_ t))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (shifted_exponential lambda_ t)) 10))
)  
(newline)


#|
  Frechet
|#
(displayln "Testing Frechet ")
(let ([alpha 1.71]
      [beta 6.3])
  (displayln (format "frechet_dist alpha:~a beta: ~a" alpha beta))
  (displayln "Some samples")   
  (displayln (repeat (lambda () (frechet_dist alpha beta)) 10))
  (displayln "PDF")
  (for ([x (range 0 100 10)])
    (show2 "frechet_dist_pdf" alpha beta x ":" (frechet_dist_pdf alpha beta x))
    )
  (newline)
  (displayln "CDF")
  (for ([x (range 1 20 2)])
    (show2 "frechet_dist_cdf" alpha beta x ":" (frechet_dist_cdf alpha beta x))
    )
  (newline)
  (displayln "Quantile")
  (for ([q ps])
    (show2 "frechet_dist_quantile" alpha beta q ":" (frechet_dist_quantile alpha beta q))
    )
  (newline)
  (displayln "Mean")
  (displayln (frechet_dist_mean alpha beta))
)
(newline)
