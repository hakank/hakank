#=
  From http://mc-stan.org/workshops/vanderbilt2016/carp-1.pdf
  Slide 5ff
  """
  Laplace's data on live births in Paris 1745-1770:
     sex        live births
     ----------------------
     female      241 945
     male        251 527    
  
  * Question 1 (Estimation):
    What is the birth rate of boys vs. girls?
  * Question 2 (Event Probability)
    Is a boy more likely to be born than a girl?

    Bayes (1763) set up the 'Bayesian' model  Laplace (1781, 1786) solved for the posterior
  ...
  (Answers:)
  * Q1: θis 99% certain to lie in(0.508,0.512)
  * Q2: Laplace 'morally certain' boys more prevalent
  """

  See Stan model ~/stan/laplace_births.stan (see slide 10)
  """
                    mean se_mean     sd   2.5%    25%    50%    75%  97.5%  n_eff   Rhat
  theta           0.51  2.4e-5 6.8e-4   0.51   0.51   0.51   0.51   0.51  793.0    1.0
  theta_gt_half    1.0     nan    0.0    1.0    1.0    1.0    1.0    1.0    nan    nan
  """

  Summary Statistics
     parameters          mean        std   naive_se      mcse        ess      rhat   ess_per_sec 
         Symbol       Float64    Float64    Float64   Float64    Float64   Float64       Float64 

          theta        0.5096     0.0007     0.0000    0.0001    80.1135    1.0098       42.9333
  theta_gt_half        1.0000     0.0000     0.0000    0.0000        NaN       NaN           NaN
      male_post   251491.8328   492.9858     4.9299   28.3048   162.6618    1.0060       87.1714

  Quantiles
     parameters          2.5%         25.0%         50.0%         75.0%         97.5% 
         Symbol       Float64       Float64       Float64       Float64       Float64 

          theta        0.5083        0.5092        0.5095        0.5101        0.5111
  theta_gt_half        1.0000        1.0000        1.0000        1.0000        1.0000
      male_post   250533.9750   251162.0000   251485.0000   251822.2500   252475.0750



  Cf ~/webppl/laplace_births.jl

=#

using Turing
include("jl_utils.jl")

@model function laplace_births(male=251527)
    female = 241945
    theta ~ Beta(2,2)
    # Note that this works i.e. male is both
    # in LHS and RHS.
    male ~ Binomial(male + female, theta)    
    
    theta_gt_half ~ Dirac(theta > 0.5)
    
    # Posterior predictive of male
    male_post ~ Binomial(male + female, theta)
    
end

model = laplace_births()

num_chains = 4
# chains = sample(model, Prior(), 100_000)
# chains = sample(model, MH(), 100_000)
# chains = sample(model, PG(15), 10_000)
chains = sample(model, SMC(), 10_000)
# chains = sample(model, IS(), 100_000)

display(chains)


