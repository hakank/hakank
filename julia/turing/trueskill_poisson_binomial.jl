#=

    This is a port of the SPPL model trueskill-poisson-binomial.pynb

    This model:
    * Prior() 
    Summary Statistics
  parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

      perfB1   79.3452    0.9921     0.0099    0.0102    9771.5110    0.9999   105070.0105
      skillA   99.9423    9.4506     0.0945    0.0981    9873.7861    0.9999   106169.7428
      perfA1   89.9122    9.0726     0.0907    0.0948   10013.5210    0.9999   107672.2688
      result    0.8594    0.3476     0.0035    0.0032   10370.4888    1.0000   111510.6318



    * With observation result == 0
    Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        perfB1   79.4940    0.8266     0.0083    0.0249   1315.1090    1.0011      171.1045
        skillA   86.2807    4.2628     0.0426    0.1226   1134.3992    0.9999      147.5929
        perfA1   75.9473    3.2281     0.0323    0.0936   1280.7372    1.0003      166.6325
        result    0.0003    0.0173     0.0002    0.0003   3133.8451    1.0002      407.7342



=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function trueskill_poisson_binomial() 
    perfB1 ~ truncated(Binomial(100, 0.9), 0,80)
    skillA ~ truncated(Poisson(100), 77,125)
    perfA1 ~ Binomial(skillA, 0.9)
        
    if perfA1 > perfB1
       result ~ Dirac(1)
    else
        result ~ Dirac(0)
    end
    
    # Observation
    true ~ Dirac(result == 0)
end 

model = trueskill_poisson_binomial()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5),  10_000)
# chns = sample(model, PG(5),  MCMCThreads(), 1_000, 4)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)
# display(plot(chns))

