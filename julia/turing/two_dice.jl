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

    s ~ Dirac(d1 + d2)
    true ~ Dirac(s == ss)
    
end

# Note: Here we observe c=5 from outside
model = two_dice(5)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, IS(), 10_000)
chns = sample(model, PG(20), 1_000) 
# chns = sample(model, SMC(1000), 1000)


display(chns)
# display(plot(chns))
# display(histogram(chns))
# display(gelmandiag(chns))

show_var_dist_pct(chns, :d1)
show_var_dist_pct(chns, :d2)
show_var_dist_pct(chns, :s)
