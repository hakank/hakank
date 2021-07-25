#=
  A/B test simple

  From ~/stan/ab_test_simple.stan
  """
                  mean      se_mean         sd         2.5%          25%
  rateA       0.09061974 0.0012142080 0.06150084   0.01159885   0.04405893
  rateB       0.07498042 0.0009625318 0.05068273   0.01016666   0.03781876
  rate_diff  -0.01563931 0.0015525787 0.07993122  -0.18981938  -0.05958019
  lp__      -14.93856837 0.0297626673 1.09616975 -17.86292780 -15.35567844
  """

  This Turing model
  """
  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

       rateA    0.0934    0.0612     0.0003    0.0014   1715.9223    1.0023      142.5185
       rateB    0.0763    0.0494     0.0002    0.0011   1683.9340    1.0002      139.8616
        diff   -0.0171    0.0784     0.0004    0.0016   2026.4826    1.0015      168.3125
     diffGt0    0.4111    0.4920     0.0025    0.0137   1085.2272    1.0029       90.1352

  Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
      Symbol   Float64   Float64   Float64   Float64   Float64 

       rateA    0.0126    0.0489    0.0820    0.1255    0.2451
       rateB    0.0094    0.0402    0.0671    0.1034    0.1997
        diff   -0.1845   -0.0619   -0.0147    0.0322    0.1342
     diffGt0    0.0000    0.0000    0.0000    1.0000    1.0000

  Credible interval for diff with mass 0.9: (-0.139303..0.114952)
  """

  See ~/stan/ab_test_simple.R and ~/stan/ab_test_simple.stan
      ~/blog/ab_test_simple.blog
      ~/webppl/ab_test_simple.wppl
=#  

using Turing, StatsPlots, DataFrames
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)

include("jl_utils.jl")

@model function ab_test_simple(nA=20,nB=25)
    # nA = 16 # Number of sent mail
    # nB = 16

    rateA ~ Beta(1,1) # priors
    rateB ~ Beta(1,1)

    1 ~ Binomial(nA,rateA) # likelihood
    1 ~ Binomial(nB,rateB)

    diff ~ Dirac(rateB - rateA)
    diffGt0 ~ Dirac(diff > 0)
end

nA = 20 # number of trial (sent mail) for A    
nB = 25 # number of trial (sent mail) for B
model = ab_test_simple(nA,nB)

num_chains = 4
# chains = sample(model, Prior(), 1000)
# chains = sample(model, MH(), 40_000)

# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
chains = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

display(chains)
# display(plot(chains))

credible_interval(chains,"diff",0.9)
