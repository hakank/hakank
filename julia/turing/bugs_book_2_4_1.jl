#=
 The BUGS Book
 Example 2.4.1 Transformation

 Cf ~/jags/bugs_book_2_4_1.R
 model {
  Z ~ dnorm(0, 1)
  Y <- pow(2*Z + 1, 3)
  P10 <- step(Y - 10)
 }
       Mean      SD Naive SE Time-series SE
P10  0.2874  0.4526 0.001848       0.001839
Y   13.3067 39.9025 0.162901       0.162901
Z    0.0112  0.9993 0.004080       0.004025

  ~/webppl/bugs_book_2_4_2.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_2_4_1()

    z ~ Normal(0,1)
    y = (2.0*z + 1.0)^3.0
    p10 ~ y > 10.0 ? flip(1.0) : flip(0.0)

    return p10
end

model = bugs_book_2_4_1()

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
