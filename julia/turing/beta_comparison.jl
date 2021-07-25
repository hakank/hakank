#=
  Beta comparison

  From infer.net src/tests/Test/BlogTests.cs
  """
  a = Beta(11,500)[mean=0.02153], b = Beta(16,695)[mean=0.0225]
  aGreaterThanConstant = Bernoulli(0.9849)
  P(A > B) =  Bernoulli(0.4467)
  """

  (Yet another A/B test.)


  Summary Statistics
         parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
             Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

              aRate    0.0215    0.0065     0.0001    0.0002   1208.1553    1.0015       35.7887
              bRate    0.0238    0.0056     0.0001    0.0002   1433.9797    1.0000       42.4782
               aGtB    0.3834    0.4862     0.0049    0.0174    709.5007    0.9999       21.0173
            aGr0_01    0.9853    0.1204     0.0012    0.0031   1829.2015    1.0001       54.1857
  aSuccessCountPost   10.7714    4.5923     0.0459    0.0939   2137.7235    1.0006       63.3249
  bSuccessCountPost   16.5616    5.5983     0.0560    0.1184   2440.4334    1.0001       72.2920



  Cf ~/webppl/beta_comparison.wppl

=#

using Turing
include("jl_utils.jl")

@model function beta_comparison(aTrialCount=400,aSuccessCount=10,bTrialCount=700,bSuccessCount=15)
    aRate ~ Beta(1,10)
    bRate ~ Beta(1,10)

    aSuccessCount ~ Binomial(aTrialCount,aRate)
    bSuccessCount ~ Binomial(bTrialCount,bRate)

    aGtB ~ Dirac(aRate > bRate)
    aGt0_01 ~ Dirac(aRate > 0.01)

    aSuccessCountPost ~ Binomial(aTrialCount,aRate)
    bSuccessCountPost ~ Binomial(bTrialCount,bRate)    
end

aTrialCount=500
aSuccessCount=10
bTrialCount=700
bSuccessCount=16
model = beta_comparison(aTrialCount,aSuccessCount,bTrialCount,bSuccessCount)

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chs)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))



