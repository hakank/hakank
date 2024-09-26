#| 

  Distributions in Racket.Gamble 

  Here are some distributions that are not implemented in Gamble,
  some also includes pdf, cdf, and quantile.

  - laplace: dist, pdf, cdf, quantile
  - extreme_value_dist1: dist (same as extreme_value_dist2 0 1)
  - extreme_value_dist2: dist, pdf, cdf, quantile, mean
  - bernoulli_dist: dist, pdf, cdf, quantile
  - binomial_dist: dist, pdf, cdf, quantile
  - hypergeometric: dist, pdf, cdf, quantile
  - hypergeometric2: dist, pdf, cdf, quantile, mean
  - negative_binomial: dist, pdf, cdf, quantile
  - beta_binomial: dist, pdf, cdf, quantile
  - multinomial_dist: dist (experimental!)
  - polya: dist
  - polya_eggenberg: dist
  - cauchy_dist: dist, pdf, cdf, quantile
  - chi_dist: dist
  - chi_squared_dist: dist 
  - chi_squared_inverse_dist: dist
  - exponential_dist: dist, pdf, cdf, quantile
  - erlang_dist: dist, pdf, cdf, mean
  - erlang2_dist: dist
  - inverse_exponential: dist
  - inverse_shifted: dist
  - frechet_dist: dist, pdf, cdf, quantile, mean

  Most of them are ports from my WebPPL programs, especially 
  node_modules/hakank_utils/distributions.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(provide (all-defined-out))

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(require (only-in math/special-functions
                  gamma-inc))

(require (only-in math/base
                  gamma.0 ; EulerGamma
                  )
         )

(require (only-in "gamble_utils.rkt"
                  dist-quantiles))

(require (rename-in math/special-functions
                    (gamma  gammaf)
                    ))


(define (sign v)
  (cond
    [(< v 0) -1]
    [(= v 0) 0]
    [(> v 0) 1]
    )
  )

; Binomial coefficient
(define (binomialf n k)
  (define (iter n k acc)
    (if (or (= k 0) (= n k))
        acc
        (iter (- n 1) (- k 1) (* acc (/ n k)))))
  (iter n k 1))


;;
;; Gamma function
;; This only handles integers. math/special-function's gamma is renamed to gammaf.
;; https://en.wikipedia.org/wiki/Gamma_function
;; (define (gammaf n) 
;;    (factorial (sub1 n)))

;
; 
(define (gamma-regularized k x)
  (gamma-inc k x #t #t)
)

; Beta function
;  https://en.wikipedia.org/wiki/Beta_function
(define (betaf a b)
    (/ (* (gammaf a) (gammaf b)) (gammaf (+ a b))))



#| 
  Laplace distribution

  From Handbook on probability distributions
  page 72ff

  """
  Let U be a uniform variate. Then the algorithm is
  * V = U − 1/2
  * X = m + sigma sign(V ) log(1 − 2|V |)
  return X
  """

  See for example
  - gamble_laplace_dist.rkt
  - gamble_fairness_hiring_model1.rkt

|#
(define (laplace mu sigma)
  (define u (uniform 0 1))
  (define v (- u 1/2))
  (define x (+ mu (* sigma (sign v) (log (- 1 (* 2 (abs v)))))))
  x
)

#|
  Laplace
  https://en.wikipedia.org/wiki/Laplace_distribution
|#
(define (laplace_pdf mu b x)
  (/ (exp (- (/ (abs (- x mu)) b))) (* 2 b))
)

; CDF[LaplaceDistribution[mu, b], x]
(define (laplace_cdf mu b x)
  (if (>= x mu)
      (- 1 (/ (exp (- (/ (- x mu) b))) 2))
      (/ (exp (- (/ (+ (- x) mu) b))) 2)
      ))


; Quantile[LaplaceDistribution[mu, sigma], q]
(define (laplace_quantile mu sigma q)
  ;  q <= 1/2 ? mu + sigma*Math.log(2*q) : mu - sigma*Math.log(2*(1-q))
  (if (<= q 0.5)
      (+ mu (* sigma (log (* 2 q))))
      (- mu (* sigma (log (* 2 (- 1 q)))))))

(define (laplace_mean mu sigma)
  mu
  )

#|
  Extreme value distributions

  From https://www.randomservices.org/random/special/ExtremeValue.html
  """
  The distribution is also known as the standard Gumbel distribution in honor of Emil Gumbel. 
  As we will show below in (13), it arises as the limit of the maximum of independent random 
  variables, each with the standard exponential distribution (when this maximum is 
  appropriately centered). This fact is the main reason that the distribution is special, 
  and is the reason for the name. For the remainder of this discussion, suppose that random variable 
  has the standard Gumbel distribution.

  ...

  The quantile function G^-1 of V given by 
     G^-1(p) = -ln(-ln(p))  p in 0..1
   """

  This is implemented as extreme_value_dist1.

  This seems to be about the same as Mathematica's ExtremeValueDistribution(0,1)

  From Mathematica ExtremeValueDistribution
  """
  ExtremeValueDistribution(alpha,beta)
  represents an extreme value distribution with location parameter alpha and scale parameter beta

  --- 

  The extreme value distribution gives the asymptotic distribution of the maximum value 
  in a sample from a distribution such as the normal distribution.

  ...
   
  Quantile(ExtremeValueDistribution(a,b), x)
  -> 
  a-b Log(-Log(x))    0 < x < 1
  Infinity            x <= 0   if 0 <= x <= 1
  Infinity             True
  """

  See 
  - extreme_value_dist.rkt
  - extreme_value_test.rkt
   for some tests.

|#

(define (extreme_value_dist1)
  (define u (uniform 0 1))
  (define x (- (log (- (log u)))))
  x
  )


(define (extreme_value_dist2 a b)
  (define u (uniform 0 1))
  (define x (- a (* b (log (- (log u))))))
  x
  )

(define (extreme_value_dist2_pdf a b x)
  (/ (exp (+ (- (exp (/ (+ (- x) a) b))) (/ (+ (- x) a) b))) b)
  )

(define (extreme_value_dist2_cdf a b x)
  (exp (- (exp (/ (+ (- x) a) b))))
  )

(define (extreme_value_dist2_quantile a b q)
  (- a (* b (log (- (log q)))))
  )

(define (extreme_value_dist2_mean a b)
  (+ a (* b gamma.0))
  )


#|
  Bernoulli distribution
  https://en.wikipedia.org/wiki/Bernoulli_distribution

  Note: It is definitely recommended to use Gamble's built-in bernoulli-dist.

|#
(define (bernoulli_dist p) 
  (define u (uniform 0 1))
  (if (<= u p) 1 0))

; Integer version, experimental
(define (bernoulli_int_dist p)
  (let ((t 100))
    (define u (random-integer t))
    (if (<= u (* p t)) 1 0)))


(define (bernoulli_dist_pdf p k)
    (if (= k 1) p (- 1 p)))


; From Mathematica
(define (bernoulli_dist_cdf p k) 
  (if (< k 0)
      0
      (if (and (>= k 0) (<= k 1))
          (- 1 p)
          1)))

;; Quantile(BernoulliDistribution(p), k)
(define (bernoulli_dist_quantile p q)
  (if (> q (- 1 p))
      1 0
))

(define (bernoulli_dist_mean p)
  p
)


#|
  Binomial distribution

  Note: It is definitely recommended to use Gamble's built-in binomial-dist.

  From Handbook on probability distributions
  page 8
  """
  It is easy to simulate Bernoulli distribution with the following heuristic:
  * generate U from a uniform distribution,
  * compute X as 1 if U <= p and 0 otherwise.

  The binomial distribution is obtained by summing n i.i.d. Bernoulli random variates.
  """
|#
(define (binomial_dist n p)
  (for/sum ((i n)) (bernoulli_dist p)))

; Experimental
(define (binomial_int_dist n p)
  (for/sum ((i n)) (bernoulli_int_dist p)))


(define (binomial_dist_pdf n p k) 
  (* (binomialf n k) (expt p k) (expt (- 1 p) (- n k))))

#|
  binomial_cdf(p,n,k) 
  https://www.radfordmathematics.com/probabilities-and-statistics/binomial-distribution/binomial-cumulative-distribution-function.html
|#
(define (binomial_dist_cdf n p k) 
    (for/sum ([r (add1 k)]) 
      (binomial_dist_pdf n p r)))

(define (binomial_dist_quantile n p q)
  ; Reversing the CDF.
  (for/first ([i (add1 n)]
              #:when (>= (binomial_dist_cdf n p i) q))
    i
    ))

(define (binomial_dist_mean n p)
  (* p n)
  )

#|
  Hypergeometric distribution

  What is the probability that we draw exactly k "success" objects
  of the n drawn objects of total N objects where there are in total
  K "success" objects

  k: number of successes we want to check
  N: total number of objects
  K: total number of success objects
  n: number of draws

|#
(define (hypergeometric1 k N K n count)
  (if (or (= n 0) (<= K 0))
      count
      ;; we have K successes left and N objects left
      (let ((p (/ K N))) ;; probability of drawing a success object
        (if (flip p)
            ;; We drew a success:
            ;; - decrement the total objects (N)
            ;; - decrement the number of "success" objects (K)
            ;; - decrement the number of drawn objects (n)
            ;; - increment the number of successful draws (count)
            (hypergeometric1 k (sub1 N) (sub1 K) (sub1 n) (add1 count))
            ;; We drew a failure:
            ;; - decrement the total objects (N)
            ;; - decrement the number of drawn objects (n)
            (hypergeometric1 k (sub1 N) K (sub1 n) count))
        )
      )
    )

(define (hypergeometric k N K n)
    (define res (hypergeometric1 k N K n 0))
    (= res k))


;; Return the number of found successes.
(define (hypergeometricCount k N K n)
    (hypergeometric1 k N K n 0))


;; (define (hypergeometric2 k N K n)
;;     (define res (hypergeometric1 k N K n 0))
;;     (= res k)
;; )

#|  
   Hypergeometric 
   From https://en.wikipedia.org/wiki/Hypergeometric_distribution
|#
;; Same order of arguments as hypergeometric_count (except that k is last)
(define (hypergeometric_pdf N K n k)
  (/ (* (binomialf K k) (binomialf (- N K) (- n k))) (binomialf N n)))

(define (hypergeometric_cdf N K n k)
  (for/sum ([r (add1 k)]) (hypergeometric_pdf N K n r)))

(define (hypergeometric_quantile N K n q)
  (for/first ([i (add1 N)]
              #:when (>= (hypergeometric_cdf N K n i) q))
    i))


#|
  hypergeometric2 distribution
  Mirrors Mathematica's version of Hypergeometric(n,n_succ,n_tot)

  """
  A hypergeometric distribution gives the distribution of the number of successes in 
  n draws from a population of size n_tot containing n_succ successes.
  """

  (hypergeometric2 n_draws n_succ n_tot)
  n_draws: number of drawn objects
  n_succ: number of successes
  n_total: total number of objects

|#
(define (hypergeometric2 n_draws n_succ n_tot)
  (hypergeometricCount 1 n_tot n_succ n_draws)
  )

(define (hypergeometric2_pdf n_draws n_spec n_tot k)
  (hypergeometric_pdf n_tot n_spec n_draws k)
  )

;; CDF
(define (hypergeometric2_cdf n_draws n_spec n_tot k)
  (for/sum ([r (add1 k)])
    (hypergeometric2_pdf n_draws n_spec n_tot r)))

(define (hypergeometric2_quantile N K n q)
  (for/first ([i (add1 N)]
              #:when (>= (hypergeometric2_cdf N K n i) q))
    i))

(define (hypergeometric2_mean N K n)
  (/ (* N K) n)
)



#|

  Negative Binomial distribution
  
  From Handbook on probability distributions, page 22ff
  """
  The algorithm to simulate a negative binomial distribution NB(m, p) 
  is simply to generate m random variables geometrically distributed 
  and to sum them.
  """

  Example:
  (define (model) 
    (define d (negative_binomial_dist 3 0.7))
    (list d
          (d >= 4))
    )

  This is a port of my WebPPL functions negative_binomial* in
  http://www.hakank.org/webppl/node_modules/hakank_utils/distributions.wppl

|#
(define (negative_binomial m p)
  (for/sum ([i m]) (geometric p)))

(define (negative_binomial_pdf r p k) 
  (* (binomialf (- (+ k r) 1) k) (expt p r) (expt (- 1 p)  k))
)

(define (negative_binomial_cdf r p k)
  (for/sum ([i (add1 k)]) (negative_binomial_pdf r p i)))

(define  (negative_binomial_quantile r p q)
  (when (or (< q 0) (> q 1))
    (error "The quantile q should be between 0 and 1"))
  (for/first ([k (in-naturals)]
              #:when (>= (negative_binomial_cdf r p k) q))
    k) 
)

(define  (negative_binomial_mean n p)
  (/ (* n (- 1 p)) p)
  )

#|
  Beta-binomial distributions

  https://en.wikipedia.org/wiki/Beta-binomial_distribution#Generating_beta_binomial-distributed_random_variables
  """
  The beta-binomial distribution is the binomial distribution in which the 
  probability of success at each of n trials is not fixed but randomly drawn 
  from a beta distribution. It is frequently used in Bayesian statistics, 
  empirical Bayes methods and classical statistics to capture overdispersion 
  in binomial type distributed data.

  The beta-binomial is a one-dimensional version of the Dirichlet-multinomial 
  distribution as the binomial and beta distributions are univariate versions 
  of the multinomial and Dirichlet distributions respectively. The special case 
  where α and β are integers is also known as the negative hypergeometric distribution. 

  ...

  To draw a beta-binomial random variate X ~ BetaBin(n,a,b) 
  simply draw p ~ Beta(a,b) and then draw X ~ B(n,p)
  """

|#

#|
  Generating Beta-binomial distribution

  https://en.wikipedia.org/wiki/Beta-binomial_distribution#Generating_beta_binomial-distributed_random_variables
  """
  To draw a beta-binomial random variate X ~ BetaBin(n,a,b) 
  simply draw p ~ Beta(a,b) and then draw X ~ B(n,p)
  """
|#
(define (beta_binomial n a b) 
    (define p (beta a b))
    (define x (binomial n p))
    x
)

#|
  beta_binomial_pdf(n,a,b,x) 

  Example: beta_binomial_pdf(12,10,10,4): 0.1231688303313781

|#

(define (beta_binomial_pdf n a b k)
  ; binomialf(n,k)*betaf(k+a,n-k+b)/betaf(a,b)
  (/ (* (binomialf n k) (betaf (+ k a) (+ n (- k) b))) (betaf a b)))


#|
  (beta_binomial_cdf n a b k)
|#
(define (beta_binomial_cdf n a b k)                           
    (for/sum ([r (add1 k)]) 
        (beta_binomial_pdf n a b r)))

(define  (beta_binomial_quantile r a b q)
  (when (or (< q 0) (> q 1))
    (error "The quantile q should be between 0 and 1"))
  (for/first ([k (in-naturals)]
              #:when (>= (beta_binomial_cdf r a b k) q))
    k) 
)

(define  (beta_binomial_mean n a b)
  (/ (* n a) (+ a b))
  )

#|
  Multinomial distribution (EXPERIMENTAL)

  Here's a definition of the multinomial distribution that WebPPL etc use,
  i.e. that the values sums to 1. The existing multinomial-dist in Gamble 
  "representing counts of n iterated samples from the corresponding categorical 
  distribution with weights for weights.", which is not what I want (and not
  really understand how to use).

  Note: This should be considered experimental since it don't give exact the
        same results as WebPPL's or Mathematical versions.

|#
(define (multinomial_dist n probs)
  (let ([m (for/list ([p probs]) (binomial n p))])
    (if (= (sum m) n)
        m
        (multinomial_dist n probs))
  ))

#|
  Pólya Distribution

  https://www.randomservices.org/random/urn/Polya.html
  """
  An urn initially contains a red and b green balls, where a and b  are positive integers. 
  At each discrete time (trial), a ball is selected from the urn and then returned to the urn 
  along with c new balls of the same color. The random process is known as Pólya's urn process, 
  named for George Pólya.

  ...

  In terms of the colors of the selected balls, Pólya's urn scheme generalizes the standard 
  models of sampling with and without replacement.

  c = 0: corresponds to sampling with replacement.
  c = -1: corresponds to sampling without replacement.
  """
|#

; See gamble_beta_binomial_urn_model.rkt for an example

(define (polya n a b c)
    (beta_binomial n (/ a c) (/ b c)))


#|
  From Mathematica BetaBinomialDistribution:
  """
  The distribution models an urn scheme. An urn contains w white balls and b black balls. 
  When a ball is drawn it is returned to the urn together with c additional balls of 
  the same color. The distribution gives the probability of drawing k white balls in n draws.
  """
|#
; See gamble_beta_binomial_urn_model.rkt for an example
(define (polya_eggenberg n w b c)
  (beta_binomial n (/ w c) (/ b c)))



#|

  Cauchy distribution

  From Handbook on probability distributions
  page 87
  """
  Since the quantile function is F^(-1)(u) = delta+gamma*tan((u-1/2)*pi), we can
  use the inversion function method.
  """

 
  Compared with the built-in cauchy function:
     delta: mode
     gamma: scale

  Note: The built-in cauchy distribution should be used instead!

|#

(define (cauchy_dist delta gamma) 
  (define u (uniform 0 1))
  (+ delta (* gamma (tan (* (- u 1/2) pi))))
)

#|
  Cauchy
  https://en.wikipedia.org/wiki/Cauchy_distribution
|#
(define (cauchy_dist_pdf delta gamma x)
  (/ 1 (* pi gamma (+ 1  (expt (/ (- x delta) gamma) 2)))))


;; From Mathematica: CDF(CauchyDistribution(delta,gamma),x)
(define (cauchy_dist_cdf delta gamma x)
  (+ 1/2 (/ (atan (+ (- delta) x) gamma) pi)))

(define (cauchy_dist_quantile delta gammav q)
  (+ delta (* gammav (tan (* (- q 1/2) pi))))
  )

; Both mean and variance for the cauchy distribution are indetermintate

#|
  Chi distribution

  From Handbook on probability distributions
  page 78
  """
  Take the square root of a chi-squared random variable.
  """

|#
(define (chi_dist v) 
  (sqrt  (for/sum ([i v]) (expt (normal 0 1) 2))))

; From Mathematica
(define (chi_dist_pdf v x)
  (/ (* (expt 2 (- 1 (/ v 2))) (exp (- (/ (expt x 2) 2))) (expt x (- v 1))) (gammaf (/ v 2)))
  )

(define (chi_dist_cdf v x)
  (gamma-inc (/ v 2) (/ (expt x 2) 2) #f #t)
  )

; Mathematica's definition includes InverseGammaRegularized which I cannot find in the math package
; (define (chi_dist_quantile v x)
; )

(define (chi_dist_mean v)
  (/ (* (sqrt 2) (gammaf (/ (+ v 1) 2))) (gammaf (/ v 2)))
  )


#|
  Chi squared distribution

  From Handbook on probability distributions
  page 76
  """
  For an integer k, just sum the square of k normal variable.
  Otherwise use the algorithm for the gamma distribution.
  """
|#
(define (chi_squared_dist k) 
  (for/sum  ([i k]) (expt (normal 0 1) 2)))

; From Mathematica
(define (chi_squared_dist_pdf v x)
  (/ (* (expt 2 (- (/ v 2))) (exp (- (/ x 2))) (expt x (- (/ v 2) 1))) (gammaf (/ v 2)))
  )

(define (chi_squared_dist_cdf v x)
  (gamma-inc (/ v 2) (/ x 2) #f #t)
  )

; Mathematica's definition includes InverseGammaRegularized which I cannot find in the math package
; (define (chi_dist_quantile v x)
; )

(define (chi_squared_dist_mean v)
  v
  )


#|
  Chi square inverse distribution

  From Handbook on probability distributions
  page 82
  """
  Simply inverse a chi-squared random variable
  """
|#
(define (chi_squared_inverse_dist k)
    (/ 1 (chi_squared_dist k)))


#|
  Exponential 

  Note: The parameter lambda for these function is the inverse of the 
        parameter lambda for the built-in exponential distribution.
        This means that it conforms with Mathematica's 
        ExponentialDistribution.

  I.e. 
     (exponemtial lambda_)   ; built-in
   -> 
     (exponential_dist (/ 1 lambda_) ) 
     (exponential_dist2 (/ 1 lambda_) )

  Sorry for any confusion.

|#

(define (exponential_dist lambda_)
  (let ([u (uniform 0 1)])
    (/ (* -1 (log u) ) lambda_)))

; Quantile(ExponentialDistribution(lambda), x)
(define (exponential_dist_quantile lambda_ q)
    (/ (* -1 (log (- 1 q))) lambda_))

; From https://en.wikipedia.org/wiki/Inverse_transform_sampling
(define (exponential_dist2 lambda_)
  (let ([u (uniform 0 1)])
   (* (/ -1 lambda_) (log (- 1 u)))))

(define (exponential_dist2_quantile lambda_ q) 
  (* (/ -1 lambda_) (log (- 1 q))))

; https://en.wikipedia.org/wiki/Exponential_distribution
(define (exponential_dist_pdf lambda_ x)
  (* lambda_ (exp (* (- lambda_) x))))

(define (exponential_dist_cdf lambda_ x)
  (- 1 (exp (* (- lambda_) x))))


(define (exponential_dist_mean lambda_)
  (/ 1 lambda_))



#|
  Erlang distribution
  From Handbook on probability distributions
  page 64
  """
  The algorithm is very easy simulate independently d random variables
  exponentially E(lambda_j) distributed and sum them.
  """
|#
; From Mathematica's ErlangDistribution
(define (erlang k lambda_) 
  (sum (for/list ([i k]) (exponential_dist lambda_))))

; From Mathematica's ErlangDistribution
(define (erlang_pdf k lambda_ x) 
  (/ (* (exp (* (- x) lambda_)) (expt x (+ k -1)) (expt lambda_ k)) (gammaf k)))

(define (erlang_cdf k lambda_ x)
  (gamma-inc k (* x lambda_) #f #t))

(define (erlang_mean k lambda_ ) (/ k lambda_))

; I'm not sure if it's relevant with different lambdas...
(define (erlang2 lambdas) 
  (sum (map (lambda (lambda_) (exponential_dist lambda_)) lambdas)))




#|
  Inverse exponential

  From Handbook on probability distributions
  page 60
  """
  The algorithm is simply to inverse an exponential variate of parameter
  1/lambda, i.e. (−lambda log(U))−1 for an uniform variable U.
  """
|#
(define (inverse_exponential lambda_)
  (let ([u (uniform 0 1)])
    (- (* lambda_ (log u)))))




#| 
  Shifted_exponential distribution

  From Handbook on probability distributions
  page 60
  """
  The random generation is simple: just add τ to the
  algorithm of exponential distribution
  """
|#
(define (shifted_exponential lambda_ t)
  (let ([u (uniform 0 1)])
    (+ (/ (* -1 (log u)) lambda_) t)))



#|
  Frechet

  From Mathematica (FrechetDistribution)
  """
  Quantile(FrechetDistribution(alpha, beta), x)
  -> 
  beta*(-Log(x))^(-1/alpha)    0 < x < 1
  0                             x <= 0
  Infinity                      True
   """
|#
(define (frechet_dist a b)
  (let ([u (uniform 0 1)])
    (* b (expt (- (log u)) (/ -1 a)))))


(define (frechet_dist_pdf a b x)
  (if (> x 0)
      (/ (* (exp (- (expt (/ x b) (- a))))
            (* a (expt (/ x b) (+ -1 (- a)))))
         b)
      0
      ))

(define (frechet_dist_cdf a b x)
  (if (> x 0)
      (exp (- (expt (/ x b) (- a))))
      0
      ))

(define (frechet_dist_quantile a b x)
  (if (> x 0)
      (* b (expt (- (log x)) (/ -1 a)))
      0)
  )

(define (frechet_dist_mean a b)
  (if (< 1 a)
      (* b (gammaf (- 1 (/ 1 a))))
      +inf.0)
  )

                      
