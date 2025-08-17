#| 

  Distributions in Racket.Gamble 

  Here are some distributions that are not implemented in Gamble,
  some also includes pdf, cdf, and quantile.
  Quantiles marked with (est) are estimations, not closed form,
  using (dist-quantiles).

  - laplace: dist, pdf, cdf, quantile
  - extreme_value_dist1: dist (same as extreme_value_dist2 0 1)
  - extreme_value_dist2: dist, pdf, cdf, quantile, mean
  - bernoulli_dist: dist, pdf, cdf, quantile
  - binomial_dist: dist, pdf, cdf, quantile, mean, variance
  - hypergeometric: dist, pdf, cdf, quantile
  - hypergeometric2: dist, pdf, cdf, quantile, mean
  - negative_binomial: dist, pdf, cdf, quantile
  - beta_binomial: dist, pdf, cdf, quantile
  - multinomial_dist: dist, pdf, mean, variance
  - polya: dist
  - polya_eggenberg: dist
  - cauchy_dist: dist, pdf, cdf, quantile
  - chi_dist: dist
  - chi_squared_dist: dist, pdf, cdf, quantile (est), mean
  - chi_squared_inverse_dist: dist
  - exponential_dist: dist, pdf, cdf, quantile, mean
  - erlang_dist: dist, pdf, cdf, mean
  - erlang2_dist: dist
  - inverse_exponential: dist
  - inverse_shifted: dist
  - frechet_dist: dist, pdf, cdf, quantile, mean
  - gamma_dist: dist, pdf, cdf, quantile (est), mean
  - geometric_dist: dist, pdf, cdf, quantile, mean
  - geometric_dist: dist, pdf, cdf, quantile, mean
  - geometric_zero_truncated_dist: dist
  - gaussian_dist: dist, pdf, cdf, quantile (est), mean
  - gumbel_dist: dist, pdf, cdf, quantile, mean
  - kumaraswamy_dist: dist, pdf, cdf, quantile, mean
  - discrete_uniform_dist: dist, pdf, cdf, quantile, mean, variance
  - negative_hypergeometric: dist, pdf, cdf, quantile, mean
  - dirichlet_dist: dist
  - pareto: dist, pdf, cdf, quantile, mean
  - pareto2: dist, pdf, cdf, quantile, mean, variance
  - pareto3: dist, pdf, cdf, quantile, mean, variance
  - pareto4: dist, pdf, cdf, quantile, mean, variance
  - poisson_dist: dist, pdf, cdf, quantile, mean
  - triangular_dist: dist, pdf, cdf, quantile, mean
  - zipf: dist, pdf, cdf, quantile, mean
  - zipf1: pdf, cdf, mean (dist and quantiles are very slow and unreliable)
  - crp (Chinese Restaurant Process): dist, pdf, cdf, quantile, mean
  - weibull: dist, pdf, cdf, quantile, mean
  - log_gamma: dist
  - log_normal: dist
  - pascal_dist: dist, pdf, cdf, quantile, mean, variance
  - rbirthday,pbirthday,qbirthday (birthday/coincidences): dist, pdf, quantile  
    a.k.a.
    birthday_dist, birthday_pdf, birthday_quantile
  - matching: dist, pdf, cdf, quantile, mean, variance
  - coupon_collector_dist: dist, pdf, cdf, quantile, mean, variance
  - order statistics: estimator_of_m_u, estimator_of_m_u_all, estimator_of_m_v
  - order_statistics_continuous: pdf, cdf
  - order_statistics_with_replacement: dist, pdf, cdf, quantile, mean, variance
  - order_statistics_with_replacement_discrete: pdf, cdf
  - record: k_record: pdf, cdf, quantile (exact as well as faster float versions)
  - record: num_records: dist, pdf, cdf, quantile
  - multivariate_hypergeometric: dist, pdf, (cdf), mean
  - poisson_process: dist, pdf, cdf, quantile, mean, variance
  - random_walk_process: dist, pdf, cdf, quantile, mean, variance
  - binomial_process: dist, pdf, cdf, quantile, mean, variance
  - probability-of-run-size: (not a proper distribution)
  - prob-n-heads-after-k-in-max-m-tosses: dist, pdf, cdf, quantile, mean, variance
  - wiener_process: dist, pdf, cdf, quantile, mean, variance
  - discrete_markov_process: dist, pdf, cdf, quantile, stationary
  - sum_prob: dist, pdf, cdf, quantile, mean

  Some of these distributions are ports from my WebPPL programs, especially 
  node_modules/hakank_utils/distributions.wppl
  Other references: Wikipedia and Mathematica
 
  Also, some are already implemented in Gamble, and should be used instead 
  for performance.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(provide (all-defined-out))

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(require (only-in math/special-functions
                  gamma-inc
                  beta-inc
                  erfc))

(require (only-in math/base
                  gamma.0 ; EulerGamma
                  )
         )

(require (only-in "gamble_utils.rkt"
                  dist-quantiles))

(require (only-in math/statistics
                  quantile))

(require (rename-in math/special-functions
                    (gamma  gammaf)
                    (beta betaf) ; The beta function somehow leaked from math/special-functions!
                    (zeta zetaf)
                    (psi0 digammaf)
                    (log-gamma lgammaf)
                    ))

(require (only-in math/number-theory
                  (multinomial multinomialf)))

(define (sign v)
  (cond
    [(< v 0) -1]
    [(= v 0) 0]
    [(> v 0) 1]
    )
  )

(defmem (factorial-mem n) (factorial n))

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

;
; beta regularized a b z
; using beta-inc.
; Note: Mathematica's parameter are in different order: BetaRegularized[z, a, b]
(define (beta-regularized a b z)
  (beta-inc a b z #f #t))

; Beta function
;  https://en.wikipedia.org/wiki/Beta_function
;; (define (betaf a b)
;;     (/ (* (gammaf a) (gammaf b)) (gammaf (+ a b))))


#|
  General binary search for quantile estimation based on a given CDF
  Used for the quantiles which there's no available close form/method
|#
(define (binary-search-quantile cdf-fn target-p tolerance [lower 0] [upper 10])
  (define (binary-search lower upper)
    (define mid (/ (+ lower upper) 2))
    (define cdf-mid (cdf-fn mid))
    (cond
      ;; Stop if within tolerance of the target probability
      [(< (abs (- cdf-mid target-p)) tolerance) mid]
      ;; Stop if lower and upper bounds are very close
      [(< (abs (- upper lower)) tolerance) mid]
      ;; Continue searching by adjusting bounds
      [(< cdf-mid target-p) (binary-search mid upper)]
      [else (binary-search lower mid)]))
  (binary-search lower upper))



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
  Generalized extreme value distribution

  Wikipedia 
  https://en.wikipedia.org/wiki/Generalized_extreme_value_distribution
  GEV(mu, sigma, xi)

|#
(define (generalized_extreme_value_pdf mu sigma xi x)
  (define (t x)
    (if (= xi 0)
        (exp (- (/ (- x mu) sigma)))
        (expt (+ 1 (* xi (/ (- x mu) sigma))) (/ -1 xi) )
        ))
  (cond
    [(or (= xi 0)
         (and (> xi 0) (>= x (- mu (/ sigma xi))))
         (and (< xi 0) (<= x (- mu (/ sigma xi)))))  
     (* (/ 1 sigma) (expt (t x) (+ xi 1)) (exp (- (t x))))]
    [else 0]))
    
(define (generalized_extreme_value_cdf mu sigma xi x)
  (define (t x)
    (if (= xi 0)
        (exp (- (/ (- x mu) sigma)))
        (expt (+ 1 (* xi (/ (- x mu) sigma))) (/ -1 xi) )
        ))
  (cond
    [(or (= xi 0)
         (and (> xi 0) (>= x (- mu (/ sigma xi))))
         (and (< xi 0) (<= x (- mu (/ sigma xi)))))
     (exp (- (t x)))]
    [else 0]))


(define (generalized_extreme_value_quantile mu sigma xi q)
  (if (= xi 0)
      (- mu (* sigma (log (- (log q)))))
      (+ mu (* (/ sigma xi) (- (expt (- (log q)) (- xi)) 1)))))

(define (generalized_extreme_value_mean mu sigma xi)
  (cond
    [(and (not (= xi 0)) (< xi 1))
     (+ mu (/ (* sigma (- (gammaf (- 1 xi)) 1)) xi))]
    [(= xi 0)
     (+ mu (* sigma gamma.0))]
     [(>= xi 1)
      +inf.0])
  )

(define (generalized_extreme_value_dist mu sigma xi)
  (let ([u (uniform 0 1)])
    (generalized_extreme_value_quantile mu sigma xi u)))

; Reduced domain for avoiding extreme values
(define (generalized_extreme_value_dist_reduced mu sigma xi)
  (let* ([e 0.01]
         [u (uniform (+ 0 e) (- 1 e))])
    (generalized_extreme_value_quantile mu sigma xi u)))
    


#|
   Generalized extreme value distribution
   Only generating random values.

   This is the old definition. Note that the parameter order is different
   from generalized_extreme_value_dist

   From Handbook on probability distributions,
   page 113ff
   """
   xi the shape parameter, mu the location parameter and sigma > 0 the scale parameter.

   The quantile function of the generalized extreme value distribution 
   is F^-1(u) = mu + sigma/xi*((-log u)^-xi)-1 
   for xi != 0. So we can use the inverse function method.
   """

   xi: shape (!= 0)
   mu location parameter
   sigma: scale (> 0) 
|#
(define (generalized_extreme_value_dist_old xi mu sigma) 
    (let ([u (uniform 0 1)])
      (- (+ mu (* (/ sigma xi) (expt (- (log u)) (- xi)))) 1)))


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

(define (binomial_dist_variance n p)
  (* n (- 1 p) p))

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

  Generating Beta-binomial distribution

  https://en.wikipedia.org/wiki/Beta-binomial_distribution#Generating_beta_binomial-distributed_random_variables
  """
  To draw a beta-binomial random variate X ~ BetaBin(n,a,b) 
  simply draw p ~ Beta(a,b) and then draw X ~ B(n,p)
  """
|#

(define (beta_binomial n a b)
  (let ([p (beta a b)])
    (binomial n p)))

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
  (for/first ([k (in-naturals)]
              #:when (>= (beta_binomial_cdf r a b k) q))
    k) 
)

(define  (beta_binomial_mean n a b)
  (/ (* n a) (+ a b))
  )

#|

  Multinomial distribution

  Here's a definition of the multinomial distribution that WebPPL etc use,
  i.e. that the values sums to 1. The existing multinomial-dist in Gamble 
  "representing counts of n iterated samples from the corresponding categorical 
  distribution with weights for weights.", which is not what I want (and not
  really understand how to use).

|#

;
; This works with enumerate
;
(define (multinomial_dist n probs)
  (define (loop remaining_trials ps counts remaining_prob)
    (if (or (= (length ps) 1) (<= remaining_prob 0))
        (cons remaining_trials counts)
        (let* ([p (first ps)]
               [prob (/ p remaining_prob)]
               [count (binomial remaining_trials prob)])
          (loop (- remaining_trials count)
                (rest ps)
                (cons count counts)
                (- remaining_prob p)))))
  (reverse (loop n probs '() 1))
  )


;
; This is correct but does not work with enumerate.
;
(define (multinomial_dist2 n probs)
  ; Find the slot to update
  (define (get-slot slot probs r cum-prob)
  (if (or (empty? probs) (< r cum-prob))
      slot
      (get-slot (add1 slot) (rest probs) r (+ cum-prob (first probs)))))
    
  (let* ([len (length probs)]
         [counts (ones-list len 0)])
    (for/list ([i n])
      (let* ([r (uniform 0 1)]
             [slot (get-slot 0 (rest probs) r (first probs))])
        (set! counts (list-update counts slot add1))
        )      
      )
    counts)
  )


; THIS IS NOT CORRECT
; Note: This version does not work well with enumerate since
;       enumerate does not work well with recursion
(define (multinomial_dist-bad n probs)
  (let ([m (for/list ([p probs]) (binomial n p))])
    (if (= (sum m) n)
        m
        (multinomial_dist n probs))
  ))

; This works with enumerate
; but is still not correct
(define (multinomial_dist2-bad n probs)
  (sample (enumerate
           (define d (for/list ([p probs]) (binomial n p)))
           (observe/fail (= (sum d) n))
           d)))



#| 
  From Siegrist "Probability Mathematical Statisics and Stochastic Processes"
  ; X,Y,Z and 1-X+Y+Z
  ; m objects
  """
  Suppose that a population consists of m objects, and that each object is one 
  of four types. There are 
     a type 1 objects, 
     b type 2 objects, 
     c type 3 objects and 
     m − a − b − c type 0 objects. 
   We sample n objects from the population at random, and without replacement. 
   The parameters m, a, b, c, and n are nonnegative integers with 
   a + b + c <= m and n <= m . 
   Denote the number of type 1, 2, and 3 objects in the sample by X, Y, and Z,
   respectively. Hence, the number of type 0 objects in the sample is n-X-Y-Z.
   x,y,z in N.
   """

   Note: This is different from my multinomial_dist which - in the case of 4 objects
   would have 4 parameters. And my multinomial_dist uses probabilities.
   See multinomial_pdf instead.
   
|#
(define (multinomial_objects_3_pdf m n a b c   x y z)
  (/ (* (binomialf a x) (binomialf b y) (binomialf c z) (binomialf (- m a b c) (- n x y z)))
     (binomialf m n)))

(define (multinomial_objects_2_pdf m n a b  x y)
  (/ (* (binomialf a x) (binomialf b y) (binomialf (- m a b) (- n x y)))
     (binomialf m n)))


#|
  Multinomial trials, page 316

  Example: 
  """
  Suppose that a system consists of 10 components that operate independently. Each component 
  is 'working' with probability 1/2, 'idle' with probability 1/3, or failed with 
  probability 1/6. 
  Let X denote the number of working components and Y the number of idle components. Give the
  probability density function of each of the following:
  """

  Note: This is more like my (multinomial_dist n ps xs) (and Mathematica's MultinomialDistribution).

|#

#|
(define (multinomial3_pdf n p q r   x y z)
  (* (multinomialf n (list x y z))
     (expt p x)
     (expt q y)
     (expt r z)
     (expt (- 1 p q r) (- n x y z))))

(define (multinomial2_pdf n p q   x y)
  (* (multinomialf n (list x y))
     (expt p x)
     (expt q y)
     (expt (- 1 p q) (- n x y))))
|#
#|
  Generalized 

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes"

  Assumptions:
    sum xs = n
    for all p in ps: p > 0
  Otherwise the result is 0

  Note: multinomial_pdf allows that for n objects there can be n-1 parameters: 
        the last (omitted) parameter is automatically calculated, and that missing parameter 
        from xs is set to 0.
        Mathematica's MultinomialDistribution does not have this feature.

|#
(define (multinomial_pdf n ps xs)
  (let ([es (for/product ([p ps]
                          [x xs])
              (expt p x))]
        [t (expt (apply - 1 ps) (apply - n xs))])
    (* (multinomialf n xs)
       es
       t)))

; According to some experimentation in Mathematica,
; the CDF is the same as the PDF
(define (multinomial_cdf n ps xs)
  (multinomial_pdf n ps xs))

(define (multinomial_mean n ps)
  (for/list ([p ps]) (* n p))) 

(define (multinomial_variance n ps)
  (for/list ([p ps]) (* n p (- 1 p))))


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
(define (chi_dist_quantile v q [tolerance 1e-13] [n 1000])
  ;; Initial rough estimate with sampling
  (define samples (for/list ([i (in-range n)]) (chi_dist v)))
  (define rough-estimate (* 2 (quantile q < samples)))  ; Double estimate as upper bound

  ;; Use binary search for more accurate quantile
  (binary-search-quantile (lambda (x) (chi_dist_cdf v x)) q tolerance 0 rough-estimate))



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
; We do an estimation instead
(define (chi_squared_dist_quantile_est v x)
  (second (first (dist-quantiles (lambda () (chi_squared_dist v)) (list x))))
)

;; ; Using "step approach". This works but is slower than chi_squared_quantile
;; (define (chi_squared_dist_quantile_step v x [step 0.0001])
;;   (let ([est (chi_squared_dist_quantile_est v 0.99999)])
;;     ; (show "est" est)
;;     (for/first ([t (in-range 0 (* 3 est) step)]
;;                 ; #:do [(show t (chi_squared_dist_cdf v t))]
;;                 #:when (>= (chi_squared_dist_cdf v t) x))
;;       t)
;;     )
;;   )


; Chi-squared quantile function using binary-search-quantile
(define (chi_squared_dist_quantile v q [tolerance 1e-13] [n 1000])
  ;; Initial rough estimate with sampling
  (define samples (for/list ([i (in-range n)]) (chi_squared_dist v)))
  (define rough-estimate (* 2 (quantile q < samples)))  ; Double estimate as upper bound

  ;; Use binary search for more accurate quantile
  (binary-search-quantile (lambda (x) (chi_squared_dist_cdf v x)) q tolerance 0 rough-estimate))


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

; Mathematica's function includes InverseGammaRegularized which is not supported
; in math package
(define (erlang_quantile_est k lambda_ x)
  (second (first (dist-quantiles (lambda () (erlang k lambda_)) (list x)))))

; Much better
(define (erlang_quantile k lambda_ q [tolerance 1e-13] [n 1000])
  ;; Initial rough estimate with sampling
  (define samples (for/list ([i (in-range n)]) (erlang k lambda_)))
  (define rough-estimate (* 2 (quantile q < samples)))  ; Double estimate as upper bound

  ;; Use binary search for more accurate quantile
  (binary-search-quantile (lambda (x) (erlang_cdf k lambda_ x)) q tolerance 0 rough-estimate))


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
  (* b (expt (- (log x)) (/ -1 a)))
  )

(define (frechet_dist_mean a b)
  (if (< 1 a)
      (* b (gammaf (- 1 (/ 1 a))))
      +inf.0)
  )

                      
#|
  Gamma distribution
|#

(define (gamma_dist a b)
  (for/sum ([i a]) (exponential_dist (/ 1 b))))

; From Mathematica ExponentialDistrution
(define (gamma_dist_pdf a b x )
  (/ (* (exp (- (/ x b))) (expt x (- a 1)) (expt b (- a))) (gammaf a))
  )

(define (gamma_dist_cdf a b x)
  (if (> x 0)
      (gamma-inc a (/ x b) #f #t)
      0)
  )

;; Mathematica's definition contains InverseGammaRegularized
;; (define (gamma_dist_quantile a b x)
;;   )
;; Here we simulate it instead
(define (gamma_dist_quantile_est a b x #:num-samples [num-samples 1000])
  (second (first  (dist-quantiles (lambda () (gamma_dist a b)) (list x) num-samples)))
   )

; Better version
(define (gamma_dist_quantile a b q [tolerance 1e-13] [n 1000])
  ;; Initial rough estimate with sampling
  (define samples (for/list ([i (in-range n)]) (gamma_dist a b)))
  (define rough-estimate (* 2 (quantile q < samples)))  ; Double estimate as upper bound

  ;; Use binary search for more accurate quantile
  (binary-search-quantile (lambda (x) (gamma_dist_cdf a b x)) q tolerance 0 rough-estimate))



(define (gamma_dist_mean a b)
  (* a b))



#|
  Geometric distribitions

  From Handbook on probability distributions
  page 19
  Expectation: (1-p)/p
  """
  A basic algorithm is to use i.i.d. Bernoulli variables as follows:
  * initialize X to 0 and generate U from an uniform distribution,
  * while U > p do ; generate U from an uniform distribution; X = X + 1;
  * return X.
  """

  The generator functions geometric_dist, geometric_dist2 cannot be used with enumerate.
  However, geometric_dist3 can be used with enumerate (with #:limit).

|#
(define (geometric1_dist p x)
    (let ([u (uniform 0 1)])
      (if (> u p) (geometric1_dist p (add1 x)) x)))

; Mathematica GeometricDistribution
(define (geometric_dist p) 
  (geometric1_dist p 0))

; Alternative version using quantiles
; https://www.randomservices.org/random/bernoulli/Geometric.html
(define (geometric_dist2 p)
  (let ([u (uniform 0 1)])
    (inexact->exact (floor (/ (log (- 1 u)) (log (- 1 p)))))
  ))

; Using flip: This can be used with enumerate (with #:limit)
(define (geometric_dist3 p)
  (define (loop p x)
    (if (flip p) x (loop p (add1 x))))
  (loop p 0)
  )

; Mathematica GeometricDistribution
(define (geometric_dist_pdf p k)
  (* (expt (- 1 p) k) p)
  )

(define (geometric_dist_cdf p k)
  (- 1 (expt (- 1 p) (+ 1 (floor k))))
  )

; https://www.randomservices.org/random/bernoulli/Geometric.html
(define (geometric_dist_quantile p q)
  (inexact->exact (floor (/ (log (- 1 q)) (log (- 1 p)))))
  )

(define (geometric_dist_mean p)
  (- (/ 1 p) 1)
  )


#|
  Zero truncated Geometric distribution

  From Handbook on probability distributions, page 21
  Zero truncated Geometric distribution is a Geometric distribution 
  but zero is not a possible value.
   
  It's used for generating a Pascal distribution, see pascal_dist

|#
(define (geometric_zero_truncated1 p n)
  (let ([u (uniform 0 1)])
    (if (> u p) (geometric_zero_truncated1 p (add1 n)) n)))


(define (geometric_zero_truncated_dist p)
  (geometric_zero_truncated1 p 1))


#|
  Gaussian / Normal distribution

  From Handbook on probability distributions
  page 49
  """
  The Box-Muller algorithm produces normal random variates:
  * generate U, V from a uniform U(0, 1) distribution,
  * compute X = sqrt(−2*log(U))*cos(2*π*V) and Y = sqrt(−2*log(U))*sin(2*π*V ).
  In outputs, X and Y follow a standard normal distribution (independently).
  ...

  But there appears that this algorithm under estimates the tail
  of the distribution (called the Neave effect, cf. Patard (2007)),
  most softwares use the inversion function method, consist in
  computing the quantile function Φ-1 of a uniform variate. 
  """

|#
(define (gaussian01)
  (let ([u (uniform 0 1)]
        [v (uniform 0 1)])
    (* (sqrt (* -2 (log u))) (sin (* 2 pi v)))
))


(define (gaussian_dist mean std)
  (+ mean (* (gaussian01) std)))

; Mathematica NormalDistribution
(define (gaussian_dist_pdf mean std x)
  (/ (exp (- (/ (expt (- x mean) 2) (* 2 std std)))) (* (sqrt (* 2 pi)) std))
  )

(define (gaussian_dist_cdf mean std x)
  (/ (erfc (/ (- mean x) (* (sqrt 2) std))  ) 2)
  )

; Mathematica's definition contains InverseErfc which math/special-functions does not support
(define (gaussian_dist_quantile_est mean std x)
  (second (first (dist-quantiles (lambda () (gaussian_dist mean std)) (list x))))
  )

; Better version
(define (gaussian_dist_quantile mean std q [tolerance 1e-13] [n 10000])
  ;; Initial rough estimate with sampling
  (define samples (for/list ([i (in-range n)]) (gaussian_dist mean std)))
  (define rough-estimate (* 2 (quantile q < samples)))  ; Double estimate as upper bound

  ;; Use binary search for more accurate quantile
  (binary-search-quantile (lambda (x) (gaussian_dist_cdf mean std x)) q tolerance 0 rough-estimate))



(define (gaussian_dist_mean mean std)
  mean
  )


#|
  Gumbel distribution 

  From Handbook on probability distributions
  page 112
  """
  The quantile function of the Gumbel I distribution is simply
  F^-1(u) = mu − sigma log(- log(u)), thus
  we can use the inverse function method.
  
  The expectation of a Gumbel type I distribution is E(X) = gamma, the
  Euler constant, roughly 0.57721.
  Its variance is Var(X) = pi^2*6 . Thus for the Fisher-Tippett distribution,
  we have E(X) = mu + sigma*gamma and Var(X) = pi^2sigma^2 / 6 .
  """
|#
; (define (gumbel_dist1 mu sigma) 
;  (let ([u (uniform 0 1)])
;    (- mu (* sigma (log (- (log u)))))))

; This is from the quantile and give better mean than the one suggested
; by The Handbook
(define (gumbel_dist mu sigma)
  (let ([u (uniform 0 1)])  
    (+ mu (* sigma (log (- (log (- 1 u))))))))


; Mathematica GumbelDistribution
(define (gumbel_dist_pdf mu sigma x)
  (/ (exp (+ (- (exp (/ (- x mu) sigma)  )) (/ (- x mu) sigma)))  sigma))

(define (gumbel_dist_cdf mu sigma x)
  (- 1 (exp (- (exp (/ (- x mu) sigma))))))

(define (gumbel_dist_quantile mu sigma x) 
  (+ mu (* sigma (log (- (log (- 1 x)))))))

(define (gumbel_dist_mean mu sigma)
  (- mu (* sigma gamma.0)))


#|
  Kumaraswamy distribution

  From Handbook on probability distributions
  """
  Since the quantile function is explicit
     F^-1(u) = (1 - (1 - u)^(1/b))^(1/a)
  an inversion function method F^-1(u) with u uniformly distributed is easily computable.
  """

|#
(define (kumaraswamy_dist a b)
  (let ((u (uniform 0 1)))
    (expt (- 1 (expt (- 1 u) (/ 1 b))) (/ 1 a))
))

; Mathemttica KumaraswamyDistribution
(define (kumaraswamy_dist_pdf a b x)
  (if (and (< 0 x) (< x 1))
      (* (expt x (+ a -1)) (expt (- 1 (expt x a)) (+ b -1)) a b)
      0
      )
  )

(define (kumaraswamy_dist_cdf a b x)
  (cond
    [(>= x 1) 1]
    [(and (< 0 x) (< x 1)) (- 1 (expt (- 1 (expt x a)) b))]
    [else 0])
  )

(define (kumaraswamy_dist_quantile a b q)
  (cond
    [(and (< 0 q) (< q 1))  (expt (- 1 (expt (- 1 q) (/ 1 b))) (/ 1 a)) ]
    [(<= q 0) 0]
    [else 1])
)

(define (kumaraswamy_dist_mean a b)
  (* b (betaf b (+ 1 (/ 1 a))))
  )


#|
  Logistic regression

  logistic01_dist 
  logistic_dist

  From Mathematica LogisticDistribution

|#

; logistic dist (0 1)
(define (logistic01_dist)
  (let ([u (uniform 0 1)])
    (if (and (< 0 u) (< u 1))
        (- (log (+ (/ 1 u) -1)))
        +inf.0)
    ))

(define (logistic01_dist_pdf x)
  (/ (exp (- x)) (expt (+ 1 (exp (- x))) 2)))

(define (logistic01_dist_cdf x)
  (/ 1 (+ 1 (exp (- x)))))

(define (logistic01_dist_quantile x)
  (- (log (+ (/ 1 x) -1))))

(define (logistic01_dist_mean)
  0
  )

; logistic dist mu s
(define (logistic_dist mu s)
  (let ([u (uniform 0 1)])
    (if (and (< 0 u) (< u 1))
        (+ mu (* s (log (/ u (- 1 u)))))
        +inf.0
        )
    ))

(define (logistic_dist_pdf mu s q)
  (/  (exp (- (/ (- q mu) s))) (* (expt (+ 1 (exp (- (/ (- q mu) s)))) 2) s)))

(define (logistic_dist_cdf mu s q)
  (/ 1 (+ 1 (exp (- (/ (- q mu) s))))))

(define (logistic_dist_quantile mu s q)
  (+ mu (* s (log (/ q (- 1 q))))))

(define (logistic_dist_mean mu s)
  mu)


#|
  Discrete uniform dist 

  Mathematica: DiscreteUniformDist
|#


(define (discrete_uniform_dist a b)
  (+ (random-integer (- (add1 b) a)) a))

(define (discrete_uniform_dist_pdf a b k)
  (if (and (<= a k) (<= k b))
      (/ 1 (+ 1 (- a) b))
      0))

(define (discrete_uniform_dist_cdf a b k)
  (cond
    ((and (<= a k) (<= k b)) (/ (+ 1 (- a) (floor k)) (+ 1 (- a) b)))
    ((>= k b) 1)
    (else 0)))

(define (discrete_uniform_dist_quantile a b k)
  (cond
    ((and (< 0 k) (< k 1)) (+ -1 a (max 1 (ceiling (* (+ 1 (- a) b) k)))))
    ((<= 0 k) a)
    (else b)))

(define (discrete_uniform_dist_mean a b)
  (/ (+ a b) 2))

(define (discrete_uniform_dist_variance a b)
  (/ (- (expt (+ b (- a) 1) 2) 1) 12))


#|
  Negative Hypergeometric 

  See https://en.wikipedia.org/wiki/Negative_hypergeometric_distribution

  Note: This is just piggy backing on the beta_binomial distribution functions.

|#
(define (negative_hypergeometric w wtot btot) 
    (beta_binomial btot w (+ (- wtot w) 1)))

(define (negative_hypergeometric_pdf w wtot btot x) 
    (beta_binomial_pdf btot w (+ (- wtot w) 1) x))

(define (negative_hypergeometric_cdf w wtot btot x) 
    (beta_binomial_cdf btot w (+ (- wtot w) 1) x))

(define (negative_hypergeometric_quantile w wtot btot q) 
    (beta_binomial_quantile btot w (+ (- wtot w) 1) q))

(define (negative_hypergeometric_mean w wtot btot) 
    (beta_binomial_mean btot w (+ (- wtot w) 1)))

#|
  Dirichlet distribution

  https://en.wikipedia.org/wiki/Dirichlet_distribution#Random_variate_generation
  Only generating  
|#
(define (dirichlet_dist params)
  (let* ([samples (for/list ([p params]) (gamma p 1))]
         [s (sum samples)]
         )
    (map (lambda (v) (/ v s)) samples)))


#|
  Pareto type I distribution

  https://en.wikipedia.org/wiki/Pareto_distribution
  Mathematica: ParetoDistribution[k, alpha]

  The built-in pareto-dist has parameters (pareto scale shape)
  My pareto_dist has parameters (pareto_dist x alpha) according to the Wikipedia page (op.cit).
  Mathematica's type I: ParetoDistribution[k, alpha].

  Note: It seems that Gamble's quantile function
    (dist-inv-cdf (pareto-dist scale shape) val)
  actually gives 
    (- 1 (dist-inv-cdf (pareto-dist scale shape) val))
  E.g. that the quantile for 0.9 is _less_ than for 0.1
   > (dist-inv-cdf (pareto-dist 6820 4) 0.9)
   7002.026455267925
   > (dist-inv-cdf (pareto-dist 6820 4) 0.1)
   12127.865576465454
  
|#
(define (pareto_dist x alpha) 
  (let ([u (uniform 0 1)])
    ; (/ x (expt (- 1 u) (/ 1 alpha)))))
    (pareto_dist_quantile x alpha u)))

; https://en.wikipedia.org/wiki/Pareto_distribution
; scale: x     (Mathematica: k)
; shape: alpha (Mathematica: alpha)
(define (pareto_dist_pdf x alpha v)
  (if (>= v x)
      (/ (* alpha (expt x alpha)) (expt v (+ alpha 1)))
      0))

(define (pareto_dist_cdf x alpha v)
  (if (>= v x)  
      (- 1 (expt (/ x v) alpha))
      0))

(define (pareto_dist_quantile x alpha q) 
  ; (/ x (expt (- 1 q) (/ 1 alpha))))
  (if (< q 1)
      (* x (expt (- 1 q) (- (/ 1 alpha))))
      +inf.0))

(define (pareto_dist_mean x alpha)
  (if (<= alpha 1)
      +inf.0
      (/ (* x alpha) (- alpha 1))))


#|
  Pareto II distribution

  Mathematica ParetoDistribution

|#
; From Mathematica ParetoDistribution
(define (pareto2_pdf k alpha mu x)
  (if (>= x mu)
      (/ (* alpha (expt (/ (+ k x (- mu)) k) (+ -1 (- alpha)))) k)
      0
      ))

(define (pareto2_cdf k alpha mu x)
  (if (>= x mu)
      (- 1 (expt (+ 1 (/ (- x mu) k)) (- alpha)))
      0
      ))

(define (pareto2_quantile k alpha mu q)
  (cond
    [(and (< 0 q) (< q 1))
     (+ (* k (+ -1 (expt (- 1 q) (/ -1 alpha)))) mu)]
    [(<= q 0)
     mu]
    [else
     +nan.0]))

(define (pareto2_dist k alpha mu)
  (let ([u (uniform 0 1)])
    (pareto2_quantile k alpha mu u)))

(define (pareto2_mean k alpha mu)
  (if (> alpha 1)
      (+ (/ k (- alpha 1)) mu)
      +nan.0))

(define (pareto2_variance k alpha mu)
  (if (> alpha 2)
      (/ (* (expt k 2) alpha)
         (* (- alpha 2) (expt (- alpha 1) 2)))
      +nan.0))

#|
  Pareto type III

  From Mathematica ParetoDistributionxs

|#
(define (pareto3_pdf k gamma mu x)
  (pareto4_pdf k 1 gamma mu x))

(define (pareto3_cdf k gamma mu x)
  (pareto4_cdf k 1 gamma mu x))

(define (pareto3_quantile k gamma mu q)
  (pareto4_quantile k 1 gamma mu q))

(define (pareto3_dist k gamma mu)
  (pareto4_dist k 1 gamma mu))

(define (pareto3_mean k gamma mu)
  (pareto4_mean k 1 gamma mu))

(define (pareto3_variance k gamma mu)
  (pareto4_variance k 1 gamma mu))

#|
  Pareto type IV

  From Mathematica ParetoDistributionxs

|#
(define (pareto4_pdf k alpha gamma mu x)
  (if (>= x mu)
      (/ (* (expt k (/ -1 gamma))
            alpha
            (expt (+ 1 (expt (/ k (- x mu)) (/ -1 gamma))) (+ -1 (- alpha)))
            (expt (- x mu) (+ -1 (/ 1 gamma))))
         gamma)
      0))

(define (pareto4_cdf k alpha gamma mu x)
  (if (>= x mu)
      (- 1 (expt (+ 1 (expt (/ (- x mu) k) (/ 1 gamma))) (- alpha)))
      0))

(define (pareto4_quantile k alpha gamma mu q)
  (cond
    [(and (< 0 q) (< q 1))
     (+ (* k (expt (+ -1 (expt (- 1 q) (/ -1 alpha))) gamma)) mu)]
    [(<= q 0)
     mu]
    [else +inf.0]))

(define (pareto4_dist k alpha gamma mu)
  (let ([u (uniform 0 1)])
    (pareto4_quantile k alpha gamma mu u)))

(define (pareto4_mean k alpha gamma mu)
  (if (> alpha gamma)
      (+ mu (/ (* k (gammaf (- alpha gamma)) (gammaf (+ 1 gamma))) (gammaf alpha)))
      +nan.0))

(define (pareto4_variance k alpha gamma mu)
  (if (> alpha (* 2 gamma))
      (/ (+ (* (expt k 2)
               (- (expt (gammaf (- alpha gamma)) 2))
               (expt (gammaf (+ 1 gamma)) 2))
            (* (gammaf alpha)
               (gammaf (- alpha (* 2 gamma)))
               (gammaf (+ 1 (* 2 gamma)))))
         (expt (gammaf alpha) 2))
            
      +nan.0))



#|
  Poisson distribution
|#
#|
  Algorithm from Handbook on probability distributions, page 14
  """
  A basic way to generate Poisson random variate is the following:
  * initialize variable n to 0, l to exp(-lambda) and P to 1,
  * do
    – generate U from a uniform distribution,
    – P = P * U,
    – n = n 0 1,
    while P >= l,
  return n − 1.
  See Knuth (2002) for details.
  """
  Note: n is the counter, p2 is the acculumated value
;;
|#
(define (poisson_dist1 lambda_ n p2)
  (let* ((l (exp (- lambda_)))
         (u (uniform 0 1))
         (p (* p2 u)))
    (if (>= p l) (poisson_dist1 lambda_ (add1 n) p) n)))

(define (poisson_dist lambda_)
  (poisson_dist1 lambda_ 0 1))

#|
  Poisson
  https://en.wikipedia.org/wiki/Poisson_distribution
|#
(define (poisson_dist_pdf lambda_ k) 
  ; Math.pow(lambda,k)*Math.exp(-lambda)/factorial(k)
  (/ (* (expt lambda_ k) (exp (- lambda_))) (factorial k)))

(define (poisson_dist_cdf lambda_ k) 
  (for/sum ([r (add1 k)]) (poisson_dist_pdf lambda_ r))
)

(define (poisson_dist_quantile lambda_ q)
  (let ([cumulativeProbability 0])
    (for/first ((k (in-naturals))
                 #:do ((set! cumulativeProbability
                             (+ cumulativeProbability (poisson_dist_pdf lambda_ k))))
                #:when (>= cumulativeProbability q))
         k)
    )
  )

(define (poisson_dist_mean _lambda)
  _lambda
  )


#|
  Triangular distributions
  
  https://en.wikipedia.org/wiki/Triangular_distribution
  Mathematica's TriangleDistribution[{min,max},mode]

|#
(define (triangular_dist min-val max-val mode)
  (let ([u (uniform 0 1)]
        [v (uniform 0 1)]
        [cc (/ (- mode min-val) (- max-val min-val))])
    (+ min-val (* (- max-val min-val) (+ (* (- 1 cc) (min u v)) (* cc (max u v)))))))

(define (triangular_dist_pdf min-val max-val mode x)
  (cond
    [(and (<= min-val x) (<= x mode))  (/ (* 2 (+ x (- min-val)))
                                             (* (- mode min-val) (- max-val min-val)))]
    
    [(and (< mode x) (<= x max-val))  (/ (* 2 (- max-val x))
                                         (* (+ max-val (- mode)) (- max-val min-val)))]
    [else 0])
  )

(define (triangular_dist_cdf min-val max-val mode x)
  (cond
    [(and (<= min-val x) (<= x mode)) (/ (expt (+ x (- min-val)) 2)
                                            (* (- mode min-val) (- max-val min-val)))]
    
    [(and (< mode x) (<= x max-val))     (- 1 (/ (expt (- max-val x) 2)
                                                 (* (+ max-val (- mode)) (- max-val min-val))))]
    [(> x max-val) 1]
    [else 0]
    )
  )

(define (triangular_dist_quantile min-val max-val mode x)
  (let ([t (/ (- mode min-val) (- max-val min-val))])
    (cond
      [(and (<= 0 x) (<= x t))
       (+ min-val (sqrt (* (- mode min-val) (- max-val min-val) x)))]
      [(and (>= 1 x) (> x t))
       (- max-val (sqrt (* (+ max-val (- mode))  (- max-val min-val) (- 1 x))))]
      [(< x 0) min-val]
      [else max-val]
      ))
  )
  
(define (triangular_dist_mean min-val max-val mode)
  (/ (+ min-val max-val mode) 3))



#| 
  Zipf distribution

  Note that "the" Zipf distribution is zipf1 below, but that's slow and unreliable...

|#

(define (zipf n s)
  (let ([pdf (for/vector ([i n]) (zipf_pdf n s (add1 i)))])
    (categorical-vw2 pdf (for/vector ([i n]) (add1 i)))))

; Zipf PDF
(define (zipf_pdf n s k)
  (if (and (<= 1 k) (<= k n))
      (/ (expt k (+ -1 (- s))) (harmonic_number_generalized n (add1 s)))
      0))


; Zipf CDF
(define (zipf_cdf n s k)
  (cond
    [(and (<= 1 k) (<= k n)) (/ (harmonic_number_generalized (floor k) (add1 s)) (harmonic_number_generalized n (add1 s)))]
    [(> k 1) 1]
    [else 0]))

; Alternative
; (define (zipf_cdf2 n s k)
;   (for/sum ([r k]) (zipf_pdf n s (add1 r))))


; There is no closed form of the quantile so we "reverse" the CDF.
(define (zipf_quantile n s q) 
  (for/first ([i (add1 n)]
              #:when (>= (zipf_cdf n s i) q))
              i))


(define (zipf_mean n s)
  (/ (harmonic_number_generalized n s) (harmonic_number_generalized n (add1 s))))

#|
  Zipf1
  Only parameter s, n is assmed to be Infinity, but is approximated by 1000 in zipf1_dst(s).
  If you want a better approximation, use zipf_dist(<a large n>, s) instead.
  This is slow and for s < 1 it's quite unreliable
 
|#
; This is just an approximation. For s < 1 it's quite unreliable.
;; (define (zipf1 s) 
;;     (zipf 1000 s))

; This is not very reliable especially for values near or < 1
(define (zipf1 alpha)
  (let ([u (uniform 0 1)])
    (zipf1_quantile alpha u)))



(defmem (zipf1_pdf s k)
  (if (>= k 1)
      (/ (expt k (+ -1 (- s))) (zetaf (add1 s)))
      0))


(defmem (zipf1_cdf s k)
  (if (>= k 1)
      (/ (harmonic_number_generalized (floor k) (add1 s)) (zetaf (add1 s)))
      0))

; Not reliable for large quantiles, and slow
; (define (zipf1_quantile s q)
;   (zipf_quantile 1000 s q))

; Slow and unreliable for large quantiles, especially for small alpha (near 1) 
(define (zipf1_quantile alpha q)
  (for/first ([k (in-naturals 0)]
              ; #:do [(show2 "k" k (zipf1_cdf alpha k))]
              #:when (>= (zipf1_cdf alpha k) q))
              k))

; Not reliable and too slow
(define (zipf1_quantile_est s q)
  (second (first (dist-quantiles (lambda () (zipf1 s)) (list q)))))

; Binary search: This is not better/faster for s values < 1
; since it relies on zipf1 which relies on zipf
(define (zipf1_quantile-bs s q [tolerance 1e-13] [n 1000])
  ;; Initial rough estimate with sampling
  (define samples (for/list ([i (in-range n)]) (zipf1 s)))
  (define rough-estimate (* 2 (quantile q < samples)))  ; Double estimate as upper bound

  ;; Use binary search for more accurate quantile
  (inexact->exact (round (binary-search-quantile (lambda (x) (zipf1_cdf s x)) q tolerance 0 rough-estimate))))



(define (zipf1_mean k)
  (/ (zetaf k) (zetaf (add1 k))))


#|
  Chinese restaurant process (CRP) distribution
  https://en.wikipedia.org/wiki/Chinese_restaurant_process

|#

(define (crp theta n)
  (let ([u (uniform 0 1)])
    (crp_quantile theta n u)))

(define (crp_pdf theta n k)
  (/  (* (gammaf theta) (stirling-first-kind n k) (expt theta k)) (gammaf (+ n theta))))

(define (crp_cdf theta n k)
  (for/sum ([r (add1 k)]) 
    (crp_pdf theta n r)))

(define (crp_quantile theta n q)
  ; Reversing the CDF.
  (for/first ([i (add1 n)]
              #:when (>= (crp_cdf theta n i) q))
    i)
  )

(define (crp_mean theta n)
  (* theta (- (digammaf (+ theta n)) (digammaf theta))))


#|
  Weibull distribution

  https://en.wikipedia.org/wiki/Weibull_distribution
  lambda_: scale
  k      : shape

  Note: Mathematica's WeibullDistribution has reversed parameters: 
        WeibullDistribution[shape,scale]

|#
(define (weibull lambda_ k)  
  (let ([u (uniform 0 1)])
    (weibull_quantile lambda_ k u)))

(define (weibull_pdf lambda_ k x)
  (if (>= x 0)
      (* (/ k lambda_) (expt (/ x lambda_) (sub1 k)) (exp (- (expt (/ x lambda_) k))))
      0))

(define (weibull_cdf lambda_ k x)
  (if (>= x 0)
      (- 1 (exp (- (expt (/ x lambda_) k))))
      0))

(define (weibull_quantile lambda_ k x)
  (if (>= x 0)
      (* lambda_ (expt (- (log (- 1 x))) (/ 1 k)))
      0))

(define (weibull_mean lambda_ k)
  (* lambda_ (gammaf (+ 1 (/ 1 k)))))

#|  
  Log gamma distribution

  From Handbook on probability distributions
  page 69
  """
  Simply simulate a gamma G(k, 1) distributed variable and returns a + b log(X).
  """
|#
(define (log_gamma k a b) 
    (+ a (* b (log (gamma k 1)))))

#|
  Log normal distribution
  From Handbook on probability distributions
  page 51
  """
  One way to characterize a random variable follows a log-normal
  distribution is to say that its logarithm is normally distributed.
  ...
  Once we have generated a normal variate, it is easy to generate
  a log-normal variate just by taking the exponential of normal
  variates.
  """
|#
(define (log_normal mu sigma) 
  (let ([g (normal mu sigma)])
    (exp g)))



#|
  Pascal distribution

  From Handbook on probability distributions
  page 25
  """
  The negative binomial distribution can be constructed by summing 
  m geometric distributed variables G(p). The Pascal distribution is 
  got from summing n geometrically distributed G0(p) variables.
  Thus possible values of the Pascal distribution are in (n, n+ 1, ...).

  ...

  The link between Pascal distribution Pa(n,p) and the negative 
  binomial distribution BN(n,p) is to substract the constant n, i.e. 
  if X ~ Pa(n,p) then X-n ~ BN(n, p).
  """

  From Mathematica PascalDistribution

|#
(define (pascal_dist_old m p)
  (let ((g (for/list ((i m)) (geometric_zero_truncated_dist p))))
    (sum g)))

; From Mathematica PascalDistribution
(define (pascal_pdf n p x)
  (if (>= x n)
      (* (expt (- 1 p) (- x n)) (expt p n) (binomialf (- x 1) (- n 1)))
      0))

(define (pascal_cdf n p x)
  (if (>= x n)
      (beta-inc n
                (+ 1 (- n) (floor x))
                p
                #f #t)
      0))

(define (pascal_quantile n p q)
  (for/first ([k (in-naturals)]
              #:when (>= (pascal_cdf n p k) q))
    k))

(define (pascal_dist n p)
  (let ([u (uniform 0 1)])
    (pascal_quantile n p u)))

(define (pascal_mean n p)
  (/ n p))

(define (pascal_variance n p)
  (/ (* n (- 1 p)) (expt p 2)))


#|
 Birthday / Coincidence distribution

  Port of R's birthday/coincidence functions (from stats library)
  """
  Probability of coincidences

  Description:

     Computes answers to a generalised _birthday paradox_ problem.
     ‘pbirthday’ computes the probability of a coincidence and
     ‘qbirthday’ computes the smallest number of observations needed to
     have at least a specified probability of coincidence.

  Usage:

     qbirthday(prob = 0.5, classes = 365, coincident = 2)
     pbirthday(n, classes = 365, coincident = 2)
     
  Arguments:

 classes: How many distinct categories the people could fall into

    prob: The desired probability of coincidence

       n: The number of people

    coincident: The number of people to fall in the same category

  Details:

     The birthday paradox is that a very small number of people, 23,
     suffices to have a 50-50 chance that two or more of them have the
     same birthday.  This function generalises the calculation to
     probabilities other than 0.5, numbers of coincident events other
     than 2, and numbers of classes other than 365.

     The formula used is approximate for ‘coincident > 2’.  The
     approximation is very good for moderate values of ‘prob’ but less
     good for very small probabilities.

  Value:

  qbirthday: Minimum number of people needed for a probability of at
             least ‘prob’ that ‘k’ or more of them have the same one out
             of ‘classes’ equiprobable labels.

  pbirthday: Probability of the specified coincidence.  
  """

  And I added rbirthday for generating random variates.
|#

; Generate random variates
(define (rbirthday classes coincident)
  (let ([p (uniform 0 1)])
    (qbirthday classes coincident p ))
  )

; Discrete values of p (for using with enumerate)
; Experimental
(define (rbirthday2 classes coincident)  
  (let ([p (random-integer (* 10 classes))])
    (qbirthday classes coincident (/ p (* 10 classes))))
  )



#|
  This is a port of the R code for pbirthday
|#
(define (expm1 x) (- (exp x) 1))
(define (pbirthday classes coincident n)
  (define k coincident)
  (define c classes)  
  (cond
    [(< k 2) 1] ; If k < 2, return 1 (special case)
    [(= k 2)
     ; For k == 2, compute the complement of the product probability
     (- 1 (for/product ([i (in-range n)]) (/ (- c i) c)))]    
    [(> k n) 0] ; If k > n, return 0
    [(> n (* c (- k 1))) 1] ; If n > c * (k - 1), return 1
    [else
     ; General case for k >= 3
     (let* ([LHS (/ (* n (exp (/ (- n) (* c k))))
                    (expt (- 1 (/ n (* c (+ k 1)))) (/ 1 k))
                    )
                 ]
            [lxx (- (* k (log LHS))
                    (* (- k 1) (log c))
                    (lgammaf (+ k 1)))])
       (- (expm1 (- (exp lxx)))))
     ]
    )
  )

#|
  This is a port of R's 
  qbirthday(prob = 0.5, classes = 365, coincident = 2) 
|#
(define (log1p x) (log (+ x 1)))
(define (qbirthday classes coincident prob)
  (define k coincident)
  (define c classes)
  (define p prob)

  ; Handle special cases for probability
  (cond
    [(<= p 0) 1]
    [(>= p 1) (+ (* c (- k 1)) 1)]
    [else
     ; Calculate initial estimate for N
     (define N
       (inexact->exact (ceiling
        (exp (/ (+ (* (- k 1) (log c))
                   (lgammaf (+ k 1))
                   (log (- (log1p (- p)))))
                k)))))
     ; Define a recursive function to adjust N upwards if pbirthday(N) < prob
     (define (adjust-up N)
       (if (< (pbirthday c k N) p)
           (adjust-up (+ N 1))
           N))

     ; Define a recursive function to adjust N downwards if pbirthday(N-1) >= prob
     (define (adjust-down N)
       (if (>= (pbirthday c k (- N 1) ) p)
           (adjust-down (- N 1))
           N))

     ; Adjust N based on pbirthday probabilities
     (cond
       [(< (pbirthday c k N) p) (adjust-up N)]
       [(>= (pbirthday c k (- N 1)) p) (adjust-down N)]
       [else N])]))

; And with a more standard names
(define (birthday_dist classes coincident)
  (rbirthday classes coincident))

(define (birthday_pdf classes coincident n)
  (pbirthday classes coincident n))

(define (birthday_quantile classes coincident q)
  (qbirthday classes coincident q))


#|
  Matching distribution

  Probability of correct guessing drawn numbers without replacement.

  https://ora.ox.ac.uk/objects/uuid:478fa6d8-bc7f-458d-92f5-7e5d63d9824a/files/mf8a85f79596ca2150078192a755dc020
  https://stats.libretexts.org/Bookshelves/Probability_Theory/Probability_Mathematical_Statistics_and_Stochastic_Processes_(Siegrist)/12:_Finite_Sampling_Models/12.05:_The_Matching_Problem

  See gamble_matching_distribution.rkt for more on this.

|#
; Generate random variates
(define (matching_dist n)
  (let ([u (uniform 0 1)])
    (matching_quantile n u))
  )

(define (matching_pdf n r)
  (* (/ 1 (factorial r))
     (for/sum ([t (range (add1 (- n r)))])
             (/ (expt -1 t) (factorial t)))))

(define (matching_cdf n r)
  (for/sum ([i (range 0 (add1 r))])
    (matching_pdf n i)))

(define (matching_quantile n q)
  (for/first ([i (add1 n)]
              #:when (>= (matching_cdf n i) q))
    i))

(define (matching_mean n)
  1)

(define (matching_variance n)
  1)


#|
  Coupon collector distribution

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes"

|#
(define (coupon_collector_dist m k)
  (let ([u (uniform 0 1)])
    (coupon_collector_quantile m k u))
  )

; Discrete version to use with enumerate
; Note: This does not give the exact correct probabilities.
; Increasing the scale will improve the precision
; (though it will take longer time).
(define (coupon_collector_dist2 m k [scale 100])
  (let* ([w (* m scale)]
         [u (/ (random-integer w) w)])
    (coupon_collector_quantile m k u))
  )

; For k in 1..m
; The PDF of Wk is given by
; ...
; n in (k+1,k+2,k+3,...)

;
; What is the probability of having to buy n coupons
; in order to get k different coupons of m different coupons.
; The normal coupon collector problem is thus
;   (coupon_collector_pdf m m n)
; I.e. that we want all m different coupons
;
(define (coupon_collector_pdf m k n)
  (if (< n k)
      0
      (* (binomialf (- m 1) (- k 1))
         (for/sum ([j (range 0 k)])
           (* (expt -1 j)
              (binomialf (- k 1) j)
              (expt (/ (+ k (- j) -1) m) (- n 1)))))))

(define (coupon_collector_cdf m k n)
  (for/sum ([i (add1 n)])
    (coupon_collector_pdf m k i)
    ))

; This is _very_ slow
(define (coupon_collector_quantile1 m k q)
  (for/first ([i (in-naturals)]
              #:when (>= (coupon_collector_cdf m k i) q))
    i
    ))

; This is slow for larger m and large q
(define (coupon_collector_quantile m k q)
  (define (loop i t)
    (if  (>= t q)
         i
         (let ([i1 (add1 i)])
           (loop i1 (+ t (coupon_collector_pdf m k i1))))))
  (loop 0 0)
  )

(define (coupon_collector_mean m k)
  (for/sum ([i (range 1 (add1 k))])
    (/ m (+ m (- i) 1))))

(define (coupon_collector_variance m k)
  (for/sum ([i (range 1 (add1 k))])
    (/ (* m (- i 1)) (expt (+ m (- i) 1) 2))))


#|
  Order statistics
|#



#|

  Estimators of m

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes"  
  """
  Estimators of m Based on Order Statistics

  Suppose that the population size m is unknown. In this subsection we consider estimators of m 
  constructed from the various order statistics.

  For i ∈ {1, 2, … , n} , the following statistic is an unbiased estimator of m :
         (n + 1)
   Ui =  ------* X(i) - 1
         i

   E(Ui)=m 
  """


  See gamble_order_statistics_estimator_of_m.rkt
|#

; U(i) estimator
; Note that i is 1-based, so we have to adjust it for ith-smallest
(define (order_statistics_m_estimator_u xs i)
  (let ([n (length xs)])
    (* 1.0 (- (* (/ (+ n 1) i)
                 ; Indices are 1-based in the formula!
                 (ith-smallest xs (sub1 i)))
              1))))

(define (order_statistics_m_estimator_u_all xs)
  (for/list ([i (range 1 (add1 (length xs)))]) (order_statistics_m_estimator_u xs i)))

; V(i) estimator
; 2*M-1
; where M = average xs
(define (order_statistics_m_estimator_v xs)
  (- (* 2 (avg xs)) 1))


#|
  Order statistics for continuous distributions

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes",
  section "Distribution of the k th order statistic"

|#

; CDF.
(define (order_statistics_continuous_cdf f n k x)
  (sum (for/list ([j (range k (add1 n))])         
         (* (binomialf n j)
            (expt (f x) j) (expt (- 1 (f x)) (- n j))))))

; PDF
(define (order_statistics_continuous_pdf pdf cdf n r x)
  (* (/ (factorial n) (* (factorial (- r 1)) (factorial (- n r))))
     (pdf x)
     (expt (cdf x) (- r 1))
     (expt (- 1 (cdf x)) (- n r))))

#|
  Order statistics discrete without replacement

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes",
  Chapter "12.4: Order Statistics"

|#
#|
  """
  Suppose that the objects in our population are numbered from 1 to m , so that 
  D = {1, 2, … , m} . 
  For example, the population might consist of manufactured items, and the labels 
  might correspond to serial numbers. As in the basic sampling model we select
  n objects at random, without replacement from D .

  ...

  The probability density function of X(i) is

               binom(x-1,i-1) * binom(m-x,n-i)
  P(X(i)=x) =  ...............................
                   binom(m n)
  x in (i,i+1,...m-m+i)
 """
|#
(define (order_statistics_without_replacement_pdf m n i x)
  (/ (* (binomialf (- x 1) (- i 1)) (binomialf (- m x) (- n i)))
     (binomialf m n)))

(define (order_statistics_without_replacement_cdf m n i x)
  (for/sum ([k (range 1 (add1 x))])
    (order_statistics_without_replacement_pdf m n i k)
    ))

(define (order_statistics_without_replacement_quantile m n i q)
  (for/first ([x (range 1 (add1 (+ (- m n) i)))]
              #:when (>= (order_statistics_without_replacement_cdf m n i x) q))
    x))

(define (order_statistics_without_replacement_dist m n i)
  (let ([u (uniform 0 1)])
    (order_statistics_without_replacement_quantile m n i u)))

(define (order_statistics_without_replacement_mean m n i)
  (* i (/ (+ m 1) (+ n 1))))

(define (order_statistics_without_replacement_variance m n i)
  (* i
     (+ (- n i) 1)
     (/ (* (+ m 1) (- m n)) (* (expt (+ n 1) 2) (+ n 2)))))


#|
  Order statistics with replacement (discrete)
|#



; From https://en.wikipedia.org/wiki/Order_statistic
; "Dealing with discrete variables"
; Three values are needed
;  p1 = P(X < x) = F(x) - f(x)
;  p2 = P(X = x) = f(x)
;  p3 = P(X > x) = 1- F(x)
; The CDF:
;   P(X(k) <= x) = sum(j=0..n-k,p3^j*(p1+p2)^(n-j))

; For P(X(k) <= x) note <=
(define (order_statistics_with_replacement_discrete_<=_cdf pdf cdf n k x)
  (let ([p1 (- (cdf x) (pdf x))]
        [p2 (pdf x)]
        [p3 (- 1 (cdf x))])
    (sum (for/list ([j (range 0 (add1 (- n k)))])
           (* (binomialf n j)
              (expt p3 j)
              (expt (+ p1 p2) (- n j)))))))

; For P(X(k) <= x) note <
; Hmm, this does give 0 for x = 1, and for n it's not fully 1!
; But it does make some sense, since there can be no values < 1 and there are
; not a possibility of getting 6, but for n+1 (e.g. 7) the CDF will become 1.
; I'm not sure how useful this really is, perhaps for other distributions
; such as poisson?
(define (order_statistics_with_replacement_discrete_<_cdf pdf cdf n k x)
  (let ([p1 (- (cdf x) (pdf x))]
        [p2 (pdf x)]
        [p3 (- 1 (cdf x))])
    (sum (for/list ([j (range 0 (add1 (- n k)))])
           (* (binomialf n j)
              (expt (+ p2 p3) j)
              (expt p1 (- n j))
              )))))

;; I'm not sure how useful this really is.
;; (displayln "order_statistics_with_replacement_discrete_<_cdf pdf cdf n k x")
;; (displayln "Testing with discrete_uniform_dist")
;; (for/list ([i (range 1 17)])
;;   (let ([t (order_statistics_with_replacement_discrete_<_cdf
;;             (lambda (x) (discrete_uniform_dist_pdf 1 6 x))
;;             (lambda (x) (discrete_uniform_dist_cdf 1 6 x))
;;             4 1 i)])
;;     (show2 i t (* 1.0 t ))
;;     t))
;; (newline)

; "Note that the probability mass function of X(k) is just the difference of these values,"
; Note that this is for taking n (all) samples. This does not support
; problems 
(define (order_statistics_with_replacement_discrete_pdf pdf cdf n k x)
  (let ([p1 (- (cdf x) (pdf x))]
        [p2 (pdf x)]
        [p3 (- 1 (cdf x))])
    (sum (for/list ([j (range 0 (add1 (- n k)))])
           (* (binomialf n j)
              (- (* (expt p3 j) (expt (+ p1 p2) (- n j)))
                 (* (expt (+ p2 p3) j) (expt p1 (- n j)))))))))


#|
  Record statistics distributions

  From
  * Alexei Stepanov "On the Mathematical Theory of Records"
    https://cm.episciences.org/9528/pdf
    page 153
  * Blom, Holst, Sandell: "Problems and Snapshots from the World of Probability", p108ff

|#
;
; Returns the probability that the kth record is n
;
(defmem (k_record_pdf n k)
  ; (/ (stirling-first-kind (- n 1) (- k 1)) (factorial n)))
  ; Speeding up a little
  (/ (stirling-1 (- n 1) (- k 1)) (factorial-mem n)))

(defmem (k_record_cdf n k)
  (for/sum ([i (range 1 (add1 n))])
    (k_record_pdf i k)))

; This is slow for high q
; For example q=0.99 for n=10 and k=3: 830 and is slow
; Use k_record_quantile-f (float values) to speed it up
(defmem (k_record_quantile n q)
  (for/first ([k (in-naturals 0)]
              ; #:do [(show2 i (* 1.0 (k_record_cdf i k)))]
              #:when (>= (* 1.0 (k_record_cdf n k)) q))
    k))

; Float versions
(defmem (k_record_pdf-f n k)
  ; (* 1.0 (k_record_pdf n k)))
  (/ (stirling-1 (- n 1) (- k 1)) (factorial-mem n)))
  ; Using (expt (lgamma n+1)) instead of factorial. Quite fast,
  ; BUT, it get stuck on 0.9941176470588228 when using large quantiles (e.g. 0.999)
  ; (/ (stirling-1 (- n 1) (- k 1)) (exp (lgammaf (add1 n)))))

(defmem (k_record_cdf-f n k)
  (for/sum ([i (range 1 (add1 n))])
    (k_record_pdf-f i k)))

; Float version is faster, but still slow for q > 0.99
(defmem (k_record_quantile-f n q)
  (for/first ([k (in-naturals 0)]
              ; #:do [(show2 i (k_record_cdf-f i k))]
              #:when (>= (k_record_cdf-f n k) q))
    k))


;
; Probability of n number of records for the kth record.
;
(define (num_records_pdf n k)
  (/ (stirling-first-kind n k) (factorial-mem n)))

(define (num_records_cdf n k)
  (for/sum ([i (range 1 (add1 k))])
    (num_records_pdf n i)))

(define (num_records_quantile n q)
  (for/first ([k (range 1 (add1 n))]
              #:when (>= (num_records_cdf n k) q))
    k))

(define (num_records n)
  (let ([u (uniform 0 1)])
    (num_records_quantile n u)))


#|
  Multivariate Hypergeometric dist

  PDF and Mean from Mathematica MultivariateHypergeometricDistribution

|#
(define (multivariate_hypergeometric_pdf n num-balls ps)
  (/ (for/product ([c (length num-balls)]) (binomialf (list-ref num-balls c) (list-ref ps c)))
     (binomialf (sum num-balls) n)))

; According to Mathematica: CDF = PDF, i.e. CDF does not seems to be defined
(define (multivariate_hypergeometric_cdf n num-balls ps)
  (multivariate_hypergeometric_cdf n num-balls ps))

(define (multivariate_hypergeometric_mean n num-balls)
  (for/list ([c (length num-balls)]) (/ (* (list-ref num-balls c) n) (sum num-balls))))

;
; Generate random instances (cf gamble_urn_model_generalized.rkt)
;
(define (multivariate_hypergeometric_dist n num-balls)
  (let* ([len (length num-balls)]
         [v (list->vector (range len))])
    (define (loop remaining-trials remaining-balls aux)
      (if (or (= remaining-trials 0) (memf (lambda (v) (<= v 0)) remaining-balls))
          aux
          (let ([ball (categorical-vw2 (list->vector remaining-balls) v)])
            (loop (sub1 remaining-trials)
                  (for/list ([i len])
                    (if (= ball i)
                        (sub1 (list-ref remaining-balls i))
                        (list-ref remaining-balls i)))
                  (for/list ([i len])
                    (if (= ball i)
                        (add1 (list-ref aux i))
                        (list-ref aux i)))
                  )
            )))
    (loop n num-balls (rep len 0))
    )
  )


#|
  max_stable dist

  From Mathematica MaxStableDistribution

  This a generalized extreme value distribution for maximum values.

|#
(define (max_stable_pdf mu sigma xi x)
    (cond
    [(= xi 0) (/ (exp (- (- (/ (- x mu) sigma)) (exp (/ (- mu x) sigma)))) sigma)]
    [(and (not (= xi 0)) (> (+ 1 (/ (- x mu)  sigma)) 0))
     (let ([t (+ (/ (* xi (- x mu)) sigma) 1)])
       (/ (* (exp (- (expt t (- (/ 1 xi))))) (expt t (- (- (/ 1 xi)) 1))) sigma))]
    [else 0]))

(define (max_stable_cdf mu sigma xi x)
  (let ([t (/ (- x mu) sigma)])
  (cond
    [(= xi 0) (exp (- (exp (- t))))]
    [(and (not (= xi 0)) (> (+ 1 (* xi t)) 0))
     (exp (- (expt (+ 1 (* xi t)) (- (/ 1 xi)))))]
    [(and (not (> xi 0)) (<= (+ 1 (* xi t)) 0)) 
     0]
    [else 1]
    )))

(define (max_stable_quantile mu sigma xi x)
  (cond
    [(and (= xi 0) (< 0 x) (< x 1))
     (- mu (* sigma (log (- (log x)))))]
    [(and (not (= xi 0)) (< 0 x) (< x 1))
     (- mu (/ (* sigma (- 1 (expt (- (log x)) (- xi)))) xi))]
    [(or (and (<= x 0) (> xi 0)) (and (>= x 1) (< xi 0)))
     (- mu (/ sigma xi))]
    [(and (<= x 0) (<= xi 0))
     -inf.0]
    [else +inf.0]))


(define (max_stable_dist mu sigma xi)
  (let ([u (uniform 0 1)])
    (cond
      [(and (= xi 0) (< 0 u) (< u 1))
       (- mu (* sigma (log (- (log u)))))]
      [(and (not (= xi 0)) (< 0 u) (< u 1))
     (- mu (/ (* sigma (- 1 (expt (- (log u)) (- xi)))) xi))]
      [(or (and (<= u 0) (> xi 0)) (and (>= u 1) (< xi 0)))
       (- mu (/ sigma xi))]
      [(and (<= u 0) (<= xi 0))
       -inf.0]
      [else +inf.0]))
  )

(define (max_stable_mean mu sigma xi)
  (cond
    [(= xi 0) (+ mu (* sigma gamma.0))]
    [(and (not (= xi 0)) (< xi 1))
     (/ (+ (* mu xi) (- sigma) (* sigma (gammaf (- 1 xi)))) xi)]
    [else +inf.0]))


#|
  min_stable distribution

  From Mathematica's MinStableDistribution

  This a generalized extreme value distribution for minimum values.

|#

(define (min_stable_pdf mu sigma xi x)
    (cond
    [(= xi 0) (/ (exp (- (- (/ (- mu x) sigma)) (exp (/ (- x mu) sigma)))) sigma)]
    [(and (not (= xi 0)) (> (+ 1 (/ (- mu x)  sigma)) 0))
     (let ([t (+ (/ (* xi (- mu x)) sigma) 1)])
       (/ (* (exp (- (expt t (- (/ 1 xi))))) (expt t (- (- (/ 1 xi)) 1))) sigma))]
    [else 0]))


(define (min_stable_cdf mu sigma xi x)
  (let ([t (/ (- mu x) sigma)])
  (cond
    [(= xi 0) (- 1 (exp (- (exp (- t)))))]
    [(and (not (= xi 0)) (> (+ 1 (* xi t)) 0))
     (- 1 (exp (- (expt (+ 1 (* xi t)) (- (/ 1 xi))))))]
    [(and (not (> xi 0)) (<= (+ 1 (* xi t)) 0)) 
     0]
    [else 1]
    )))

(define (min_stable_quantile mu sigma xi x)
  (cond
    [(and (= xi 0) (< 0 x) (< x 1))
     (+ mu (* sigma (log (- (log (- 1 x))))))]
    [(and (not (= xi 0)) (< 0 x) (< x 1))
     (+ mu (/ (* sigma (- 1 (expt (- (log (- 1 x))) (- xi)))) xi))]
    [(or (and (<= x 0) (> xi 0)) (and (>= x 1) (< xi 0)))
     (+ mu (/ sigma xi))]
    [(and (<= x 0) (<= xi 0))
     -inf.0]
    [else +inf.0]))

(define (min_stable_mean mu sigma xi)
  (cond
    [(= xi 0) (- mu (* sigma gamma.0))]
    [(and (not (= xi 0)) (< xi 1))
     (/ (- (* mu xi) (- sigma) (* sigma (gammaf (- 1 xi)))) xi)]
    [else +inf.0]))

(define (min_stable_dist mu sigma xi)
  (let ([u (uniform 0 1)])
    (min_stable_quantile mu sigma xi u)))


#|
  Poisson process
|#
; From Mathematica PoissonProcess
(define (poisson_process_pdf mu t x)
  (if (>= 0)
      (/ (* (exp (* (- t) mu)) (expt (* t mu) x)) (factorial x))
      0))

; From https://en.wikipedia.org/wiki/Poisson_point_process
; "the probability of the random variable N(t) being equal to n is given by:"
; This is the PDF.
;; (define (poisson_process_count lambda_ t n)
;;  (* (/ (expt (* lambda_ t) n) (factorial n)) (exp (- (* lambda_ t)))))

(define (poisson_process_cdf mu t x)
  (if (>= 0)
      (gamma-regularized (+ 1 (floor x)) (* t mu))
      0))

; Reversing the CDF
; Note: This might be slow for q very near 1
(define (poisson_process_quantile mu t q)
  (for/first ([n (in-naturals)]
              #:when (>= (poisson_process_cdf mu t n) q))
    n))

; Note: This might be slow 
(define (poisson_process_dist mu t)
  (let ([u (uniform 0 1)])
    (poisson_process_quantile mu t u)))
              

(define (poisson_process_mean mu t)
  (* t mu))

(define (poisson_process_variance mu t)
  (* t mu))


#|
  Random walk process

  From Mathematica RandomWalkProcess
|#

; Handling non integer valus of k
(define (binomialf-float n k)
  (/ (gammaf (+ n 1))
     (* (gammaf (+ k 1))
        (gammaf (+ (- n k) 1)))))

(define (random_walk_process_pdf p t x)
  (if (and (>= (+ t x) 0) (>= (- t x) 0))
      (* 1/2
         (+ 1 (expt -1 (+ t x)))
         (expt (- 1 p) (/ (- t x) 2))
         (expt p (/ (+ t x) 2))
         (binomialf-float t (/ (+ t x) 2)))
      0))


; Mathematica:
; PDF[RandomWalkProcess[p][t], x]
; CDF[RandomWalkProcess[p][t], x]
(define (random_walk_process_cdf p t x)
  (let ([tx2 (/ (+ t x) 2)])
    (cond
      [(and (<= 0 tx2) (< tx2 t))
       ; Mathematica BetaRegularized[z,a,b] (cf the order (beta-inc a b z))
       (beta-inc (- t (floor tx2))
                 (+ 1 (floor tx2))
                 (- 1 p)  
                 #f #t)]
      [(>= tx2 t)
       1]
      [else 0])))

(define (random_walk_process_quantile p t q)
  (for/first ([x (range (- t) (add1 t))]
              #:when (>= (random_walk_process_cdf p t x) q))
              x))

(define (random_walk_process_dist p t)
  (let ([u (uniform 0 1)])
    (random_walk_process_quantile p t u)))

(define (random_walk_process_mean p t)
  (* (+ -1 (* 2 p)) t))  

(define (random_walk_process_variance p t)
  (* 4 (- 1 p) p t))


#|
  Binomial_process

  From Mathematica BinomialProcess
|#
(define (binomial_process_pdf p t x)
  (if (and (<= 0 x) (<= x t))
      (* (expt (- 1 p) (- t x))
         (expt p x)
         (binomialf t x))
  0
  ))

(define (binomial_process_cdf p t x)
  (cond
    [(and (<= 0 x) (< x t))
     ; BetaRegularlized[z,a,b]] (cf the order (beta-inc a b z))
     (beta-inc (- t (floor x))
               (+ 1 (floor x))
               (- 1 p)
               #f #t
              )
     ]
    [(>= x t)  1]
    [else 0]))
     
(define (binomial_process_quantile p t q)
  (for/first ([x (range (- t) (add1 t))]
              #:when (>= (binomial_process_cdf p t x) q))
              x))

(define (binomial_process_dist p t)
  (let ([u (uniform 0 1)])
    (binomial_process_quantile p t u)))

(define (binomial_process_mean p t)
  (* p t))

(define (binomial_process_variance p t)
  (* (- 1 p) p t))


#|
  
  Probability of an r-sized run for n trial with probability p

  From see https://math.stackexchange.com/questions/417762/probability-of-20-consecutive-success-in-100-runs
  """
  There's a pretty simple recurrence relation that yields the same result. Let f(n) be the 
  possibility of getting a string of at least r successes in n trials where the possibility of 
  success in one trial is p.

  The next value of f(n+1) is the possibility of starting a new string of r successes (p^r)
  preceded by a failure (1−p), but you don't want to double count any sequence that already 
  had a string of r successes. That double counting is removed by multiplying by (1−f(n+1−r−1)).

  So the recurrence relation is:

  f(n+1)=f(n)+(1−p)∗(p^r)∗(1−f(n−r)) for n > r
  f(r)                               p^r
                                     0 otherwise

  Substituting r = 20, and p = .9 yields the same answer as @awkward above. i.e. f(100) = 0.7752991959
  """

  And we get the same result:

  (t 155059839175900504924381436836342065083638297473182979981373175134990794132777951951/200000000000000000000000000000000000000000000000000000000000000000000000000000000000 0.7752991958795026)


  (Also see https://www.reddit.com/r/askmath/comments/rwy34y/if_i_flip_a_coin_100_times_what_is_the/
   which shows a different way for coins)

|#

#|
  Recurrence formula:
  f(n+1) = f(n)+(1−p)∗(p^r)∗(1−f(n−r))
  f(r)   = p^r
  f(_)   = 0
|#
(define (probability-of-run-size n0 p r)
  ; Memoize it
  (defmem (loop n)
      (cond
        [(or (< n 0) (< n r)) 0]
        [(= n r) (expt p r)]
        [else
         (let ([n1 (- n 1)])
           (+ (loop n1)
              (* (- 1 p)
                 (expt p r)
                 (- 1 (loop (- n1 r))))))]
      ))
  (loop n0)
  )


#|
  prob-n-heads-after-k-in-max-m-tosses

  Probability of getting n heads in a row after exact k tosses,
  with max m tosses.

|#

;
; Port of Python code from
; https://www.reddit.com/r/math/comments/4kj27s/probability_of_getting_n_heads_in_a_row_after_m/
; m: number of tosses
; n: number of heads in a row
; p: probability of success
;
; The last value in P is the probability of getting n heads in a row in m tosses
;
(defmem (prob-n-heads-after-k-in-max-m-tosses-list p m n)
  (let ([q (- 1 p)]
        [P (ones-list (add1 m) 0)])
    (set! P (list-set P n (expt p n)))
    (for ([i (range n m)])
      (set! P (list-set P (add1 i) (+ (list-ref P i)
                                      (* (- 1 (list-ref P (- i n)))
                                         q
                                         (expt p n))))))
    P)
  )

;
; PDF
; 
; Probability of getting n tosses (with probability p of getting heads)
; in a row in m tosses in the k'th toss,
;
; Note that the range of k is 0..m+1.
; where k=m+1 contains the rest of the probabilities of k=0..m
;
; Example
; For getting 3 heads in a row in max 21 tosses after just 3 tosses is 0.125.
; For getting 3 heads in a row in max 21 tosses after 21 tosses is 0.017113685607910156
; This is the weird part of this PDF:
; The probability of not getting 3 heads in a row in max 21 tosses is when k=21+1
;   (prob-n-heads-after-k-in-max-m-tosses-pdf 1/2 21 3 22)
;   0.19585800170898438
;
; The full probability of getting n tosses in a row in m tosses
; is given by the CDF (see below)
;   (prob-n-heads-after-k-in-max-m-tosses-cdf p m n m)
;
(defmem (prob-n-heads-after-k-in-max-m-tosses-pdf p m n k)
  (let* ([cdf (prob-n-heads-after-k-in-max-m-tosses-list p m n)]
         [s (sum cdf)]
         [diffs (cons 0 (differences cdf))])
    (if (> k m)
        (- 1 (sum diffs))
        (list-ref diffs k))
  ))

;
; CDF
; The cumulative probability of getting n heads in a row in max m tosses after k tosses
; is (prob-n-heads-after-k-in-max-m-tosses-cdf p m n k)
; The total probability of getting n heads in a row in max m tosses is when k=m:
;  (prob-n-heads-after-k-in-max-m-tosses-cdf p m n m)
; The weird part
; The total probability of not getting n heads in a row in max m tosses is when k=m+1
;  (prob-n-heads-after-k-in-max-m-tosses-cdf p m n (+ 1 m))
; 
(defmem (prob-n-heads-after-k-in-max-m-tosses-cdf p m n k)
  (let ([cdf (prob-n-heads-after-k-in-max-m-tosses-list p m n)])
    (if (> k m)
        1
        (list-ref cdf k)
        ))
  )

; Quantile
(define (prob-n-heads-after-k-in-max-m-tosses-quantile p m n q)
  (for/first ([k (range (+ 2 m))]
              #:when (>= (prob-n-heads-after-k-in-max-m-tosses-cdf p m n k) q))
    k)
  )

; The mean value of tosses needed to get n heads
(define (expected-tosses-needed-for-n-heads n)
  (* 2 (- (expt 2 n) 1)))

; Generalized version of (expected-tosses-needed-for-n-heads n)
; The mean value of tosses needed to get n successes when the
; probabilisty of success is p
(define (expected-tosses-needed-for-n-successes n p)
  (/ (- 1 (expt p n))
     (* (- 1 p) (expt p n))))




; Generating random number of k
(define (prob-n-heads-after-k-in-max-m-tosses-dist p m n)
  (let ([u (uniform 0 1)])
        (prob-n-heads-after-k-in-max-m-tosses-quantile p m n u)
  ))


#|
  Wiener process, a,k.a. Brownian motion

  From Mathematica's WienerProcess
|#
(define (wiener_process_pdf mu sigma t x)
  (gaussian_dist_pdf (* mu t) (* sigma (sqrt t)) x))

(define (wiener_process_cdf mu sigma t x)
  (gaussian_dist_cdf (* mu t) (* sigma (sqrt t)) x))

(define (wiener_process_quantile mu sigma t q)
  ; (gaussian_dist_quantile (* mu t) (* sigma (sqrt t)) q))
  (dist-inv-cdf (normal-dist (* (* 1.0 mu) t) (* sigma (sqrt t))) (* 1.0 q)))  


(define (wiener_process_dist mu sigma t)
  (let ([u (uniform 0 1)])
    (wiener_process_quantile mu sigma t u)))
  
(define (wiener_process_mean mu sigma t)
  (* t mu))

(define (wiener_process_variance mu sigma t)
  (* t (expt sigma 2)))


#|
  Discrete Markov process

  Inspired by Mathematica DiscreteMarkovProcess

|#
; Probability of being in state state after t steps
; Note: This is a proper PDF over the states, but not over the time.
(defmem (discrete_markov_process_pdf tm init t state)
  (defmem (loop n k)
    (cond
      [(= n 0) ; Base case: at step 0, return initial state probabilities
       (list-ref init k)]
      [else
       ;; Compute probability recursively, summing over previous states
       (for/sum ([j (in-range (length tm))])
         (* (loop (sub1 n) j) ; Prob. of reaching state j at previous step
            (list-ref (list-ref tm j) k)))]))
  (loop t state))

; Cumulative probability of being in state state after t steps
(defmem (discrete_markov_process_cdf tm init t state)
  (let ([state-probs (accum (for/list ([s (range (add1 state))])
                              (discrete_markov_process_pdf tm init t s)))])
    (list-ref state-probs state)))
            

; Which state have the probability q after step steps?
; Note: Currently Mathematica's CDF does not use the CDF, just use the 1/(num states) intervals
(defmem (discrete_markov_process_quantile tm init step q)
  (let ([a (for/list ([state (range (length tm))])
             (discrete_markov_process_cdf tm init step state))])
    (for/first ([state (length tm)]
                #:when (>= (list-ref a state) q))
      state)
    ))


; Generate random states for a discrete Markov process
(define (discrete_markov_process_dist tm init t)
  (let ([u (uniform 0 1)])
    (discrete_markov_process_quantile tm init t u)))


; Stationary probability.
; Consider this experimental...
;; (define (discrete_markov_process_stationary tm init state #:start-time [start-time 1] #:max-time [max-time 1000] #:precision [precision 1e-20])
;;   (let ([last-p 0.0]
;;         [last-diff 100.0])
;;     (for/first ([t (in-naturals start-time)]
;;                 #:do [(define this-p (discrete_markov_process_pdf tm init t state))
;;                       (define this-diff (abs (- last-p this-p)))
;;                       (define diff-diff (abs (- last-diff this-diff)))
;;                       (show2 "t" t "last-p" (* 1.0 last-p) "this-p" (* 1.0 this-p) "last-diff" (* 1.0 last-diff) "this-diff" (* 1.0 this-diff) "diff-diff" (* 1.0 diff-diff))
;;                       (set! last-p this-p)
;;                       (set! last-diff this-diff)
;;                       ]
;;                 #:when (or (<= last-diff precision)
;;                            (and (< last-diff 1) (= last-diff diff-diff))
;;                            (> t max-time))
;;                 )
;;       (when (> t max-time)
;;         (displayln (format "Could not find stationary for state ~a in <= ~a steps, precision ~a" state max-time precision)))
;;       (list this-p t)
;;       )))
(define (discrete_markov_process_stationary transition-matrix
                                            #:tolerance [tolerance 1e-10]
                                            #:max-iterations [max-iterations 1000]
                                            #:exact? [exact? #f])
  (define num-states (length transition-matrix))
  (define initial-dist (make-list num-states (/ (if exact? 1 1.0) num-states)))
  
  (define (apply-transition dist)
    (map (lambda (j)
           (apply + (map (λ (i) (* (list-ref dist i) (list-ref (list-ref transition-matrix i) j)))
                         (range num-states))))
         (range num-states)))
  
  (define (converged? dist1 dist2)
    (< (apply max (map abs (map - dist1 dist2))) tolerance))

  (defmem (iterate dist counter)
    (let ([next-dist (apply-transition dist)])
      (if (or (converged? dist next-dist) (>= counter max-iterations))
          (begin
            (when (>= counter max-iterations)
              (displayln (format "stationary did not converge in ~a iterations with tolerance ~a! Last convergence: ~a" counter tolerance (* 1.0 (apply max (map abs (map - dist next-dist)))))))
            next-dist
            )
          (iterate next-dist (add1 counter)))))
  
  (iterate initial-dist 0))

; Reduce small values to 0
(define (discrete_markov_process_stationary2 transition-matrix #:tolerance [tolerance 1e-10])
  (map (λ (x) (if (< (abs x) 1e-19) 0 x)) (discrete_markov_process_stationary transition-matrix #:tolerance tolerance)))


; Checks that all rows sums to (about) 1 for a proper matrix
(define (markov-check-matrix m #:tolerance [tolerance 1e-05])
  (for ([row m])
    (when (not (<= (abs (- tolerance (sum row)))))
      (displayln (format "Matrix is not correct! Error in row ~a sum:~a" row (sum row))))))


;
; Returns the first passage time for the to states
; Note: This uses iteration and might not converge. Increasing max-iter might fix that.
;
(define (discrete_markov_process_first_passage_time p to
                                                    #:tolerance [tolerance 10e-26]
                                                    #:max-iter [max-iter 10000]
                                                    #:exact? [exact? #f]
                                                    #:debug? [debug? #f])
  
  ; Return the max of pairwise differences
  (define (max-pairs l1 l2)
    (max-list (for/list ([i (vector-length l1)]) (abs (- (vector-ref l1 i) (vector-ref l2 i))))))

  (let* ([n (length p)] ; Number of states
         [converged? #f] ;  Identify absorbing states
         [converge-iter 0]
         [f (ones-vector n 0)] ;  Initialize first passage time estimates with 0 for non-target states
         [f-new (ones-vector n 0)]         
         [absorbing-states (flatten (index-of (diagonal p) 1))] ; Identify absorbing states
         )
    (cond
      ; Set passage time to zero if we're already at the target
      ; [(member to absorbing-states) (list 0)]
      
      [else
       ; Iteratively update the first passage times using dynamic programming
       (for ([iteration (in-naturals)]
             #:do [(set! f (list->vector (vector->list f-new)))] ; using (set! f f-new) gives an reference
             #:do [(for ([i (range n)])
                     (when (and (not (member i absorbing-states)) (not (= i to)))
                       (vector-set! f-new i
                                    (+ (if exact? 1 1.0) (sum (for/list ([j (range n)]) (* (list-ref (list-ref p i) j) (vector-ref f j)))))))
                     )]
             #:do [(when (< (max-pairs f-new f) tolerance) (set! converged? #t) (set! converge-iter iteration))]
             #:final (or converged? (>= iteration max-iter)))
         #t ; We have to have something here...
         )
       (when debug?
           (show2 "to" to "converged?" converged? "converge-iter" converge-iter)
           )
       (when (not converged?) (displayln (format "to ~a did not converge!" to)))
       (vector->list f-new)
       ]))
  )


#|
  Sum probability

  Distribution of summing discrete uniform numbers to a specific sum.

  Inspired by Fletcher Thompson "What’s the Probability Ten Dice Add Up To 12?"
  https://medium.com/puzzle-sphere/whats-the-probability-ten-dice-add-up-to-12-83f637205505

|#
; Number of ways to sum n integers from a to b to get s
(defmem (count-partitions-sum a b n s)
  (cond
    [(= n 1) (if (and (>= s a) (<= s b)) 1 0)]
    [(or (< s (* n a)) (> s (* n b))) 0 ]
    [else (for/sum ([x (range a (add1 b))])
            (count-partitions-sum a b (sub1 n) (- s x) ))]))

;
; Probability of getting the sum s for n random samples in the range a to b
;
(defmem (sum_prob_pdf a b n s)
  (if (or (< s (* a n)) (> s (* b n)))
      0
      (/ (count-partitions-sum a b n s) (expt (+ b (- a) 1) n))))

(defmem (sum_prob_cdf a b n s)
  (cond
    [(< s (* a n)) 0]
    [(> s (* b n)) 1]
    [else (for/sum ([i (range (* a n) (add1 s))])
            (sum_prob_pdf a b n i))])
  )

(define (sum_prob_quantile a b n q)
  (for/first ([i (in-range (* a n) (add1 (* b n)))]
              #:when (>= (sum_prob_cdf a b n i) q))
    i))

(define (sum_prob_mean a b n)
  (/ (* n (+ a b)) 2))

(define (sum_prob_variance a b n)
  (* n (/ (- (expt (+ b (- a) 1) 2) 1) 12)))
   
(define (sum_prob_dist a b n)
  (let ([u (uniform 0 1)])
    (sum_prob_quantile a b n u)))
