#=

  The BUGS Book
  Example 3.5.1 Heart transplants: learning from data

  ~/jags/bugs_book_3_5_1.R
  """
  model {
    yT ~ dbin(pT, nT)
    pT ~ dunif(0, 1)
    for (i in 1:8) {
      sP[i] ~ dexp(theta)
    }
    theta ~ dgamma(0.001, 0.001)
    surv.t <- pT/theta # expected survival with transplant
    Is <- surv.t - 2
  }

  Data: list(yT=8, nT=10, sP=c(2,3,4,4,6,7,10,12))

  Output:
         Mean      SD  Naive SE Time-series SE
Is     3.1442 2.28250 0.0080699      0.0085034
pT     0.7500 0.12014 0.0004248      0.0005856
surv.t 5.1442 2.28250 0.0080699      0.0085034
theta  0.1667 0.05899 0.0002086      0.0002086
  """

  Note that most value are fixed/derived.

  Summary Statistics
  parameters      mean       std   naive_se      mcse       ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64   Float64   Float64       Float64 

       theta    0.1473    0.0000     0.0000    0.0000   20.5530    0.9999        6.1813
          pT    0.8623    0.0000     0.0000    0.0000   20.5530    0.9999        6.1813
          yT    8.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
      surv_t    5.8559    0.0000     0.0000    0.0000       NaN       NaN           NaN
          Is    3.8559    0.0000     0.0000    0.0000   20.5530    0.9999        6.1813



  Cf ~/webppl/bugs_book_3_5_1.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_3_5_1(sPs)
    n = length(sPs)
    nT = 10
    
    theta ~ Gamma(0.001,1/0.001)
    pT ~ Uniform(0,1)
    yT ~ Binomial(nT,pT)
    
    surv_t ~ Dirac(pT/theta) # expected survival with transplant
    Is ~ Dirac(surv_t - 2.0)
    
    true ~ Dirac(yT == 8)
    sPs ~ filldist(Exponential(theta),n)    

end

sPs = [2,3,4,4,6,7,10,12]
model = bugs_book_3_5_1(sPs)

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(10_000), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
chns = sample(model, HMC(0.1,10), 10_000)

display(chns)
# display(plot(chns))
