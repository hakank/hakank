#=

  Identify the change point in the coal miners data.
  https://pymc-devs.github.io/pymc/tutorial.html
  """
  Consider the following dataset, which is a time series of recorded
  coal mining disasters in the UK from 1851 to 1962
  [R.G. Jarrett. A note on the intervals between coal mining disasters. Biometrika, 66:191–193, 1979.]
  """

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function change_point(y)
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

# Coal miners data from https://pymc-devs.github.io/pymc/tutorial.html
data = [4, 5, 4, 0, 1, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 1,
        4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3,
        0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0,
        0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2,
        0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1]

model = change_point(data)

#=
println("Prior distribution")
chns_prior = sample(model, Prior(), MCMCThreads(), 1000, num_chns)
display(chns_prior)
display(plot(chns_prior))
=#

println("\nPosterior distribution")
num_chns = 2

# 3.17s tau 40.32+/- 3.77 (rhat ~ 1.1!) 10_000 samples
# chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns)
# 3.17s tau 41.5249+/- 3.03 (rhat ~ 1.04!) 30_000 samples
# chns = sample(model, MH(), MCMCThreads(), 30_000, num_chns)

# Too long...
# chns = sample(model, Gibbs(MH(:tau),PG(20,:lamba_1,:lambda_2)), MCMCThreads(), 10000, num_chns)

# 1.2s tau 57.6+/- 32.1(!) (rhat ok) 1_000 samples
# 3.3s tau 55.98+/- 32.0(!) (rhat ok) 10_000 samples
# chns = sample(model, IS(), MCMCThreads(), 10_000, num_chns)

# 16.2s tau 42.2 +/- 8.5 (rhat 1.05..) 1000 samples
# chns = sample(model, PG(20), MCMCThreads(), 1000, num_chns)

# 2.37s tau 41.1+/- 2.62 (rhat > 2!)  with samples: 1_000
# 17.3s tai 41.28+/- 2.84 (rhat ok)  with samples: 10_000
# chns = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chns)

# 33.1s tau 40.87+/- 2.91 (rhat ~ 1.03)
# chns = sample(model, SMC(), MCMCThreads(), 10_000, num_chns)


# # LoadError: MethodError: Cannot `convert` an object of type Float64 to an object of type Symbol
# chns = sample(model, Gibbs(NUTS(0.65,5,:lambda_1,:lambda_2), PG(20,:tau) ), 1000)

# 19.34s tau 41.4+/-5.18 (rhat ok)
chns = sample(model, Gibbs(HMC(0.1,5,:lambda_1,:lambda_2), PG(20,:tau) ), MCMCThreads(), 1000, num_chns)

# Error
# chns = sample(model, Gibbs(HMCDA(0.15,0.65,0.3,:lambda_1,:lambda_2), PG(20,:tau) ), MCMCThreads(), 1000, num_chns) # Error...

display(chns)
# plot(chns)

show_var_dist_pct(chns, :tau)
show_var_dist_pct(chns, :lambda_1,20)
show_var_dist_pct(chns, :lambda_2,20)
