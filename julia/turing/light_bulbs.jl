#=

  From the WebPPL model (written months ago):
  """
  Today I changed the last of two bulbs which were supposed to hold for 
  2000h (they are Halogen bulbs). The first went out some day ago, 
  say 25h ago.

  What is the probability of this to happen, i.e. that both bulbs went out
  in a 25h period, if we assume that the time for light bulbs are an 
  Exponential distribution?
  """

  Mathematica code:
    Probability[Abs[a - b] <= c, {a, b} \[Distributed] 
      ProductDistribution[ExponentialDistribution[p], 
      ExponentialDistribution[p]]]

  1 - E^(-c p) (if c > 0)
  
  -> 0.0124222

  Here's the summary of the three models: exponential, gaussian, and poisson,
  where 1 is the probability that both bulbs went out in the range of 25h.
  I guess that the exponential model is the most realistic of these.

  Model 1: Exponential
  Distributions of variable p (num:0)
  0.00000 =>    9881  (0.988100)
  1.00000 =>     119  (0.011900)

  Model 2: Gaussian
  Distributions of variable p (num:0)
  0.00000 =>    8639  (0.863900)
  1.00000 =>    1361  (0.136100)

  Model 3: Poisson
  Distributions of variable p (num:0)
  0.00000 =>    6851  (0.685100)
  1.00000 =>    3149  (0.314900)

  Model 4: Weibull (experimental)
  Distributions of variable p (num:0)
  0.00000 =>    9880  (0.988000)
  1.00000 =>     120  (0.012000)

  Note: 2000h is 83.33 days. 
  1/0.011900 (from the exponential model) ~ 84.03: This happens once each 84'th day...


  Cf ~/webppl/light_bulbs.jl

=#

using Turing
include("jl_utils.jl")

# Exponential
@model function light_bulbs_exp()
    lambda = 2000
    b1 ~ Exponential(lambda)
    b2 ~ Exponential(lambda)
    # The differences in failures
    diff ~ Dirac(abs(b1-b2))
    # Probability that the difference between the events is <= 25
    p ~ Dirac(diff <= 25)
end

println("Model 1: Exponential")
model = light_bulbs_exp()

# chns = sample(model, Prior(), 100_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 100_000)

display(chns)
show_var_dist_pct(chns, :p)

# Gaussian distribution: 2000h, standard deviation 100h
@model function light_bulbs_gauss()
    mu = 2000;
    stdev = 100;
    b1 ~ Normal(mu,stdev)
    b2 ~ Normal(mu,stdev)
    diff ~ Dirac(abs(b1-b2))
    p ~ Dirac(diff <= 25)
end

println("Model 2: Gaussian")
model = light_bulbs_gauss()

# chns = sample(model, Prior(), 100_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 100_000)

display(chns)
show_var_dist_pct(chns, :p)



# Poisson distribution: 1/2000h
@model function light_bulbs_poisson()
    lambda = 2000
    b1 ~ Poisson(lambda)
    b2 ~ Poisson(lambda)
    diff ~ Dirac(abs(b1-b2))
    p ~ Dirac(diff <= 25)
end

println("Model 3: Poisson")
model = light_bulbs_poisson()

# chns = sample(model, Prior(), 100_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 100_000)

display(chns)
show_var_dist_pct(chns, :p)

# Weibul distribution
@model function light_bulbs_weibul()
    shape = 1
    scale = 2000
    b1 ~ Weibull(shape,scale)
    b2 ~ Weibull(shape,scale)
    diff ~ Dirac(abs(b1-b2))
    p ~ Dirac(diff <= 25)
end


println("Model 4: Weibull (experimental)")
model = light_bulbs_weibul()

# chns = sample(model, Prior(), 100_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 100_000)

display(chns)
show_var_dist_pct(chns, :p)
