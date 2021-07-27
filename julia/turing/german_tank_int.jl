#=
  German tank problem

  https://en.wikipedia.org/wiki/German_tank_problem

  See ~/stan/german_tank.py
      ~/blog/german_tank_int.blog
      ~/webppl/german_tank_int.wppl

=#

#=
The (mean) value is sensitive to nn (the range of possible integers)-
Here we test nn=10_000 and 1_000

Testing y=[10,256,202,97]

* nn = 10_000 using MH (4 chns * 10-000)
Summary Statistics
  parameters       mean        std   naive_se      mcse         ess      rhat
      Symbol    Float64    Float64    Float64   Float64     Float64   Float64

           n   387.3275   205.1037     0.5128    3.3054   2371.1663    1.0018

Quantiles
  parameters       2.5%      25.0%      50.0%      75.0%      97.5%
      Symbol    Float64    Float64    Float64    Float64    Float64

           n   259.0000   283.0000   325.0000   408.0000   901.0000

  2.728913 seconds (37.15 M allocations: 1.883 GiB, 11.90% gc time)

* nn = 1_000
Summary Statistics
  parameters       mean        std   naive_se      mcse          ess      rhat
      Symbol    Float64    Float64    Float64   Float64      Float64   Float64

           n   363.6256   124.4699     0.3112    0.9607   15281.6395    1.0002

Quantiles
  parameters       2.5%      25.0%      50.0%      75.0%      97.5%
      Symbol    Float64    Float64    Float64    Float64    Float64

           n   258.0000   280.0000   319.0000   398.0000   738.0000

  2.827802 seconds (36.46 M allocations: 1.883 GiB, 13.95% gc time)


=#


# using Memoization
using Turing, StatsPlots,DataFrames
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)

include("jl_utils.jl")

# "Theoretical" Bayesian mean
# https://en.wikipedia.org/wiki/German_tank_problem
function theoretical(a)
    m = maximum(a)
    len = length(a)
    return (m-1)*(len-1)/(len-2)
end


data = []
@model function german_tank_int(y,nn=10_000)
    n ~ DiscreteUniform(1,nn)
    for i in 1:length(y)
        y[i] ~ DiscreteUniform(1,n)
    end

end

y = [10,256,202,97]
# y = [1,60] # Mosteller's version
# nn = 10_000
nn = 1000
println("theoretical: $(theoretical(y))")
model = german_tank_int(y,nn)

num_chns = 4

# mean: 4969.6869 +/- 2869.6797
# chns = sample(model, Prior(), MCMCThreads(), 40_000, num_chns)

# mean 381.6883+/- 212.6124
chns = sample(model, MH(), 10_000)
# chns = sample(model, MH(), MCMCThreads(), 100_000, num_chns)

# Mean: 398.16 +/309.91
# chns = sample(model, PG(20), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, PG(20), 1_000)

# mean: 4989.9474 +/- 2873.9598
# chns = sample(model, IS(), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, IS(), 10_000)

# mean: 949.1512+/- 1261.4199
# chns = sample(model, SMC(), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, SMC(), 10_000)


# chns = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, NUTS(1000), 10_000)
# chns = sample(model, Gibbs(MH(:zlabels),NUTS(1000,0.65,:m,:b,:sigma)), MCMCThreads(), 40_000, num_chns)

display(chns)

# d = sort(make_hash(data))
# display(d)

show_var_dist_pct(chns, :n,10)

# Note: One have to treat IS special
# See https://github.com/TuringLang/Turing.jl/issues/1467
using StatsFuns
using LinearAlgebra
println("estimate using lp (for IS()):")
display(dot(chns[:n], softmax(chns[:lp]))) # 384.09706644203965
