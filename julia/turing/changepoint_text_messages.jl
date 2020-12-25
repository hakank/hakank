#=
  Changepoint detection of text messages counts in webppl.


  From http://nbviewer.jupyter.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter1_Introduction/Ch1_Introduction_PyMC3.ipynb
  """
  You are given a series of daily text-message counts from a user of your system.
  The data, plotted over time, appears in the chart below. You are curious to
  know if the user's text-messaging habits have changed over time, either
  gradually or suddenly. How can you model this? (This is in fact my own
  text-message data. Judge my popularity as you wish.)
  """

  Identify the changepoint in text message counts.

  Cf changepoint_coal_miners.jl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function change_point_text_messages(y)
    len = length(y)
    alpha = mean(y) # 1/0.581151832460733  # 1/mean(data)
    lambda_1 ~ Exponential(alpha)
    lambda_2 ~ Exponential(alpha)
    tau ~ DiscreteUniform(1,len)

    # Data is according to Poisson with two different lambdas,
    # one before the change point (tau) and one after the change point
    for i in 1:len
        if tau > i
            y[i] ~ Poisson(lambda_1)
        else
            y[i] ~ Poisson(lambda_2)
        end
    end

end

# http://nbviewer.jupyter.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter1_Introduction/Ch1_Introduction_PyMC3.ipynb
data =[13, 24, 8, 24, 7, 35, 14, 11, 15, 11, 22, 22, 11, 57, 11, 19, 29, 6,
        19, 12, 22, 12, 18, 72, 32, 9, 7, 13, 19, 23, 27, 20, 6, 17, 13, 10,
        14, 6, 16, 15, 7, 2, 15, 15, 19, 70, 49, 7, 53, 22, 21, 31, 19, 11,
        18, 20, 12, 35, 17, 23, 17, 4, 2, 31, 30, 13, 27, 0, 39, 37, 5, 14,
        13, 22];

# display(plot(data))
model = change_point_text_messages(data)

#=
println("Prior distribution")
chains_prior = sample(model, Prior(), MCMCThreads(), 1000, num_chains)
display(chains_prior)
display(plot(chains_prior))
=#

println("\nPosterior distribution")
num_chains = 4

# 2.06s tau 42+/- 8.37 rhat ~1.5
# chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
chains = sample(model, MH(), MCMCThreads(), 30_000, num_chains)

# 1.94s tau 37.5+/- 21.2 (rhat ok)
# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains,drop_warmup=true)

# chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)



## LoadError: MethodError: Cannot `convert` an object of type Float64 to an object of type Symbol
# chains = sample(model, Gibbs(NUTS(0.65,5,1,1,0,:lambda_1,:lambda_2), PG(20,:tau) ), 1000)
# chains = sample(model, Gibbs(HMC(0.1,5,:lambda_1,:lambda_2), PG(20,:tau) ), MCMCThreads(), 1000, num_chains)

# Error
# chains = sample(model, Gibbs(HMCDA(0.15,0.65,0.3,:lambda_1,:lambda_2), PG(20,:tau) ), MCMCThreads(), 1000, num_chains) # Error...

display(chains)
# plot(chains)
show_var_dist_pct(chains, :tau)
show_var_dist_pct(chains, :lambda_1,20)
show_var_dist_pct(chains, :lambda_2,20)
