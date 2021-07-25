#=

  The BUGS Book

  Example 3.3.3 Trihalomethanes in tap water

  Cf ~/jags/bugs_book_3_3_3.R
  """
  model {
    for (i in 1:n) {
      y[i] ~ dnorm(mu, inv.sigma.squared)
    }
    mu ~ dnorm(gamma, inv.omega.squared)
    inv.omega.squared <- n0/sigma.squared
    inv.sigma.squared <- 1/sigma.squared
    y.pred ~ dnorm(mu, inv.sigma.squared)
    P.crit <- step(y.pred - y.crit)
  }

  Data: list(n=2, y=c(128, 132), gamma=120, n0=0.25,sigma.squared=25, y.crit=145)

  Output:
            Mean      SD  Naive SE Time-series SE
P.crit 3.213e-03 0.05659 0.0002001      0.0002034
mu     1.289e+02 3.33096 0.0117767      0.0117332
y.pred 1.289e+02 5.95716 0.0210618      0.0209726 
  """

  Summary Statistics
  parameters       mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol    Float64   Float64    Float64   Float64      Float64   Float64       Float64 

          mu   128.9489    3.3703     0.0337    0.0502    4816.4635    0.9999     2519.0709
      y_pred   128.9964    6.0306     0.0603    0.0779    7496.1334    1.0000     3920.5719
      p_crit     0.0046    0.0677     0.0007    0.0006   10096.6867    0.9999     5280.6939

  Quantiles
  parameters       2.5%      25.0%      50.0%      75.0%      97.5% 
      Symbol    Float64    Float64    Float64    Float64    Float64 

          mu   122.2672   126.7344   128.9249   131.2583   135.4088
      y_pred   117.2568   124.8223   128.9742   133.1282   140.7995
      p_crit     0.0000     0.0000     0.0000     0.0000     0.0000


  Cf ~/webppl/bugs_book_3_3_3.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_3_3_3(y=[128.0,132.0])
    n = 2
    gamma = 120
    n0 = 0.25
    sigma_squared = 25
    y_crit = 145.0
    
    omega_squared = sigma_squared/n0
    mu ~ Normal(gamma, sqrt(omega_squared))
    #  random Real inv_omega_squared ~ n0/sigma_squared;
    y ~ filldist(Normal(mu, sqrt(sigma_squared)),2)

    #  random Real inv_sigma_squared ~ 1/sigma_squared;
    y_pred ~ Normal(mu, sqrt(sigma_squared));
    p_crit ~ Dirac(y_pred > y_crit)
    
 end

model = bugs_book_3_3_3()

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
