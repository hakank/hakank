#=

  The BUGS Book
  Example 3.3.2 Surgery (continued): beta-binomial analysis using BUGS

  Cf ~/jags/bugs_book_3_3_2.R
  """
  model {
    theta ~ dbeta(a, b)
    y ~ dbin(theta, n)
    Y.pred ~ dbin(theta, n.pred)
    P.crit <- step(Y.pred - n.crit + 0.5)
  }

  Output: 
          Mean      SD  Naive SE Time-series SE
P.crit 0.41531 0.49278 0.0017422      0.0019429
Y.pred 1.49477 1.42409 0.0050349      0.0061002
theta  0.07453 0.04117 0.0001456      0.0002249
  """

  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

       theta    0.0748    0.0416     0.0004    0.0005   7624.8902    1.0000     3450.1765
      y_pred    1.4931    1.4276     0.0143    0.0144   9178.6177    1.0000     4153.2207
      p_crit    0.4177    0.4932     0.0049    0.0051   9070.0308    1.0001     4104.0863


  Cf ~/webppl/bugs_book_3_3_2.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_3_3_2(y=0)
    a = 3
    b = 27
    n = 10
    n_pred = 20
    n_crit = 2
    
    theta ~ Beta(a,b)
    y ~ Binomial(n,theta)
    y_pred ~ Binomial(n_pred,theta)
    p_crit ~ Dirac(y_pred + 0.5 > n_crit) #  toReal(y_pred - n_crit) + 0.5 > 0.0;

end

model = bugs_book_3_3_2()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(10_000), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
# chns = sample(model, HMC(0.1,5), 10_000)

display(chns)
# display(plot(chns))
