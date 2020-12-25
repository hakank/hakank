#=
# The BUGS Book
# Example 2.1.2, page 17:
# For
#   Y ~ Binomial(0.5, 8)
# we want to know Pr(Y <= 2)
#

  See ~/jags/bugs_book_2_1_2.jags
       Mean     SD Naive SE Time-series SE
  P2 0.1442 0.3513 0.001434       0.001434
  Y  4.0025 1.4177 0.005788       0.005839

  ~/webppl/bugs_book_2_1_2.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_2_1_2()

    y ~ Binomial(8,0.5)
    p2 ~ y <= 2 ? flip(1.0) : flip(0.0);

    return y, p2

end


model = bugs_book_2_1_2()

num_chains = 4
# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(15), MCMCThreads(), 10_000, num_chains)

chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)

display(chains)
# display(plot(chains))

gen = generated_quantities(model, chains)
show_var_dist_pct(gen, 120)
