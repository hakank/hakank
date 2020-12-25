#=
  From https://people.duke.edu/~ccc14/sta-663/PyStan.html

  Cf ~/stan/coin_toss.py
"""
       mean se_mean     sd   2.5%    25%    50%    75%  97.5%  n_eff   Rhat
p      0.61  3.1e-3   0.05   0.52   0.58   0.61   0.64    0.7  235.0    1.0
lp__ -70.25    0.04   0.77 -72.18 -70.39 -69.94 -69.79 -69.74  303.0   1.01
"""

  See ~/blog/coin_toss.blog
      ~/psi/coin_toss.psi
      ~/webppl/coin_toss.wppl

=#


using Turing, StatsPlots, DataFrames
using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)
include("jl_utils.jl")


@model function coin_toss(yObs)
    n = 100 # number of tosses

    p ~ Beta(2,2)

    yObs ~ Binomial(n,p) # number of successes
    # y ~ Binomial(n,p) # number of successes
    # y ~ Dirac(yObs)


end

model = coin_toss(61)

num_chains = 1
# chains = sample(model, Prior(), MCMCThreads(), 1000, num_chains)

# chains = sample(model, MH(), 40_000)
chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, PG(20), 10_000)
# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains) # Nope: does not handle the observation
# chains = sample(model, IS(), 40_000)

# chains = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chains)
# chains = sample(model, Gibbs(MH(:zlabels),NUTS(1000,0.65,:m,:b,:sigma)), MCMCThreads(), 40_000, num_chains)

display(chains)


show_var_dist_pct(chains, :p,20)
show_var_dist_pct(chains, :yObs,20)
show_var_dist_pct(chains, :y,20)
