#=
  Sum of 2 dice.

  I asked how one should observe a sum of the two dice here:
  https://discourse.julialang.org/t/turing-observing-sum-of-two-random-dice/50486


=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function two_dice(ss)
    d1 ~ DiscreteUniform(1, 6)
    d2 ~ DiscreteUniform(1, 6)

    ss ~ Dirac(d1 + d2)

    return ss
end

# Note: Here we observe c=5 from outside
model = two_dice(4)

# chains = sample(model, MH(), 10000) # This don't work anymore! c is 1..5!
num_chains = 4 # 4
# This works: i.e. forces d1 and d2 to be 2
chains = sample(model, MH(), MCMCThreads(), 10000, num_chains)

# Both d1 and d2 have means 3.5
# chains = sample(model, IS(), MCMCThreads(), 1000, num_chains) # Nope

# Both d1 and d2 have means 3.5
# chains = sample(model, PG(20), 1000) # Nope

# # Both d1 and d2 have means 3.5
# chains = sample(model, SMC(1000), 1000) # Nope

# chains = sample(model, NUTS(), 1000) # Error
# chains = sample(model, HMC(0.1, 5), 1000) # Error
# chains = sample(model, HMCDA(0.15, 0.65), 1000) # ERROR

display(chains)
# display(plot(chains))
# display(histogram(chains))
# display(gelmandiag(chains))

show_var_dist_pct(chains, :d1)
show_var_dist_pct(chains, :d2)

println("\nhash d1+d2")
display(sort(make_hash([chains[:d1][i]+chains[:d2][i] for i in 1:length(chains)])))


gen = generated_quantities(model, chains)
println("After removing the 0s from gen")
# gen2 = [g for g in gen if g != 0]
show_var_dist_pct(gen, 120)
