#=

  From
  "CAV 2020 Tutorial: Probabilistic Programming: A Guide for Verificationists"
  https://www.youtube.com/watch?v=yz5uUf_03Ik&t=2657s
  Around @23:30

  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

          pr    0.3290    0.2330     0.0023    0.0036   4184.9815    0.9999      900.7709
           p    0.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
           x   14.0153    0.4955     0.0050    0.0062   5962.5178    0.9999     1283.3659


  cf ~/webppl/gaussian_mixture_model2.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function gaussian_mixture_model2(xval=14.5)
    pr ~ Uniform(0,1)
    p ~ flip(pr)

    # x ~ p ? Normal(10.0,1.0) : Normal(14.0,0.5)
    # x ~ p ? Normal(10.0,1.0) : Normal(14.0,1)  

    # Note: Both these approaches give incorrect solutions
    # x == 14.5 || begin Turing.@addlogprob! -Inf; return end
    # true ~ Dirac(x == 14.5)
    # true ~ Dirac(x == 8.5)
    # true ~ Dirac(x == 12.5)
    
    # This is the way, "observe externally"
    xval ~ p ? Normal(10.0,1.0) : Normal(14.0,0.5)

    # Post
    x_post ~ p ? Normal(10.0,1.0) : Normal(14.0,0.5)
    

end

model = gaussian_mixture_model2()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
# chns = sample(model, HMC(0.1,5), 10_000)
# chns = sample(model, Gibbs(HMC(0.1,5,:pr,:x),PG(5,:p)), 10_000)
# chns = sample(model, Gibbs(HMC(0.1,5,:pr,:x),MH(:p)), 10_000)
# chns = sample(model, Gibbs(NUTS(1000,0.65,:pr,:x),PG(15,:p)), 10_000)

display(chns)
# display(plot(chns))

