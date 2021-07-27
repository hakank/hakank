#=
  German tank problem

  https://en.wikipedia.org/wiki/German_tank_problem

  This is a real (float) version of German tank problem

  See  ~/stan/german_tank.py
       ~/blog/german_tank_int.blog
       ~/webppl/german_tank.blog

  Cf german_tank_int.jl

  The theoretical answer is 382.5
  The advantage of this model using y as Uniform() is that we can use
  NN = 10000 and it's still fast (the full integer version is much slower).

  Interestingly, with NN = 10000 and N = randomInteger(NN), then
  enumerate calculates the value to exactly 383.0076434324275 (in 1.3s)
  which is almost exactly the same as the full integer version in german_tank_int2.wppl
  (which takes about 2min51s).

=#
# using Memoization
using Turing, StatsPlots, DataFrames
using ReverseDiff, Zygote, Tracker
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

# Note: Compared to  german_tank_in.jl both
# n and y are Uniform
@model function german_tank(y,nn=10_000)
    # n ~ DiscreteUniform(1,nn)
    n ~ Uniform(1,nn)
    # n ~ truncated(Normal(nn/2,nn/2),1,nn)
    for i in 1:length(y)
        y[i] ~ Uniform(1,n+1) # using Uniform instead of DiscreteUniform
    end

end

y = [10,256,202,97]
# y = [60] # Mosteller's version
nn = 10_000
println("theoretical: $(theoretical(y))")
model = german_tank(y,nn)

num_chns = 4


# n: 5005.3523   2889.8752
# chns = sample(model, Prior(), MCMCThreads(), 40_000, num_chns)

#
# chns = sample(model, MH(), 40_000)
# 382.1528 +/- 218.3875
chns = sample(model, MH(), MCMCThreads(), 40_000, num_chns)

# n:398.0311 +/- 321.7577  (it took 63s!)
# chns = sample(model, PG(20), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, PG(20), 1_000)

# n: 4999.0273+/-2889.7176
# chns = sample(model, IS(), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, IS(), 10_000)

# n: 956.9097 +/- 1271.8082
# chns = sample(model, SMC(), MCMCThreads(), 40_000, num_chns)
# chns = sample(model, SMC(), 10_000)



# n: 384.4288 +/- 203.1368
# chns = sample(model, NUTS(1000,0.65), 40_000)
# n: 384.5504 +/- 232.5935  (12.7s)
# chns = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chns)

# Error:
# LoadError: MethodError: no method matching phasepoint(::Random._GLOBAL_RNG, ::Array{Any,1},
# chns = sample(model, Gibbs(MH(:n),NUTS(1000,0.65,:y)), MCMCThreads(), 1_000, num_chns)
# chns = sample(model, Gibbs(MH(:n),NUTS(10,0.65,:y)), 1_000)

display(chns)

show_var_dist_pct(chns, :n,20)
