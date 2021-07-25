#=
   Binomial Trial Count.

   From infer.net test/Tests/BlogTests.cs
   """
   // Example of inferring the size of a population.
   // Reference: A. Raftery, "Inference for the binomial N parameter: A hierarchical Bayes approach", Biometrika 1988
   // http://pluto.huji.ac.il/~galelidan/52558/Material/Raftery.pdf

   ....
   ExpectationPropagation
   impala
   theta = Beta(80.24,91.17)[mean=0.4681]
   N mode = 44, median = 45
   waterbuck
   theta = Beta(238,233.8)[mean=0.5044]
   N mode = 124, median = 125
   impala
   N mode = 37, median = 65
   waterbuck
   N mode = 122, median = 208
  
   VariationalMessagePassing
   impala
   theta = Beta(106,370.9)[mean=0.2223]
   N mode = 94, median = 94
   waterbuck
   theta = Beta(316,393)[mean=0.4457]
   N mode = 141, median = 141
   impala
   N mode = 37, median = 65
   waterbuck
   N mode = 122, median = 208
   """

   Model 1: Impala
   Summary Statistics
   parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

     beta[1]   43.9012   26.9240     0.2692    1.6380   232.9437    1.0011      110.3999
     beta[2]   59.2836   25.3251     0.2533    1.2813   328.1459    1.0000      155.5194
       theta    0.4091    0.1625     0.0016    0.0090   272.1842    1.0010      128.9972
           n   73.4049   87.6808     0.8768    4.6531   335.0041    1.0000      158.7697

   Model 2: Waterbuck
   Summary Statistics
   parameters       mean        std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol    Float64    Float64    Float64   Float64    Float64   Float64       Float64 

     beta[1]    42.1176    25.6791     0.2568    1.7376   152.0795    1.0034       90.3085
     beta[2]    63.5964    24.1648     0.2416    1.7421   106.0723    1.0363       62.9883
       theta     0.3738     0.1557     0.0016    0.0104   164.0217    1.0003       97.4000
           n   215.9076   134.5262     1.3453    9.2679   136.5959    1.0000       81.1139


   Cf ~/webppl/binomial_trial_count.wppl

=#

using Turing
using Plots, StatsPlots
include("jl_utils.jl")

@model function binomial_trial_count(data)
    maxN = 1000
    beta ~ filldist(Uniform(0.1,100),2)
    theta ~ Beta(beta[1],beta[2])
    
    n ~ Categorical(simplex([1/i for i in 1:maxN]))
    data ~ filldist(Binomial(n, theta), length(data))
    
end

impala = [15,20,21,23,26]
waterbuck = [53,57,66,67,72]
println("Model 1: Impala")
model = binomial_trial_count(impala)

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(5), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))

println("\n\nModel 2: Waterbuck")
model = binomial_trial_count(waterbuck)

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(5), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))

