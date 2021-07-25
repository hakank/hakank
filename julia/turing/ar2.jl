#=
  AR(2) in Turing.jl

  From BLOG example/ar2.blog

  Note: Using 
     true ~ Dirac(x[2] == 1.0)
  takes too long for NUTS() and HMC(). Instead I hardwired x[2] as
       x[t] ~ Dirac(1.0) (for t == 2)

  This might be considered cheating...


  NUTS: (58s)
  """
  Summary Statistics
  parameters      mean       std   naive_se      mcse       ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64   Float64   Float64       Float64 

        x[1]    0.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
        x[2]    1.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
        x[3]   -1.2907    0.0000     0.0000    0.0000   20.5530    0.9999        0.3713
        x[4]    0.5053    0.0000     0.0000    0.0000   20.5530    0.9999        0.3713

  Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
      Symbol   Float64   Float64   Float64   Float64   Float64 

        x[1]    0.0000    0.0000    0.0000    0.0000    0.0000
        x[2]    1.0000    1.0000    1.0000    1.0000    1.0000
        x[3]   -1.2907   -1.2907   -1.2907   -1.2907   -1.2907
        x[4]    0.5053    0.5053    0.5053    0.5053    0.5053
   """

  HMC(0.01,5) (3,6s)
  """
  Summary Statistics
  parameters      mean       std   naive_se      mcse       ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64   Float64   Float64       Float64 

        x[1]    0.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
        x[2]    1.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
        x[3]    1.5492    0.0000     0.0000    0.0000   20.5530    0.9999        9.6674
        x[4]    0.1999    0.0000     0.0000    0.0000   20.5530    0.9999        9.6674

  Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
      Symbol   Float64   Float64   Float64   Float64   Float64 

        x[1]    0.0000    0.0000    0.0000    0.0000    0.0000
        x[2]    1.0000    1.0000    1.0000    1.0000    1.0000
        x[3]    1.5492    1.5492    1.5492    1.5492    1.5492
        x[4]    0.1999    0.1999    0.1999    0.1999    0.1999
   """


   Cf ~/blog/ar2.blog
      ~/webppl/ar2.wppl

=#

using Turing
include("jl_utils.jl")

@model function ar2()
    beta = [0.2,0.8]
    sigma = 0.1
    # sigma ~ Uniform(0.0,1.0)
    # beta1 ~ Uniform(0,1)
    # beta2 ~ Uniform(0,1)

    # n = length(ys)
    n = 4
    # Note: using tzeros give strange error. Use Vector{Real} instead!
    # tzeros(Float64,n)
    x = Vector{Real}(undef, n) 
    for t in 1:n
        if t == 1
            x[t] ~ Dirac(0.0)
        elseif t == 2
            # With this NUTS() take an awful long time
            # x[t] ~ Normal(0.0,sigma)
            # This is much faster but might be considered cheating...
            x[t] ~ Dirac(1.0)
        else
            x[t] ~ Normal(beta[1]*x[t-2] + beta[2]*x[t-1],sigma)
        end
    end

    # This takes too long!
    # true ~ Dirac(x[2] == 1.0)
    
end

ys = [0,1,2,3]
# model = ar2(ys)
model = ar2()
num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
# chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chs)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))

