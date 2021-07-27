#=

  Gaussian mixture model.

  cf ~/blog/gaussian_mixture_model.blog
     ~/psi/gaussian_mixture_model.psi
     ~/webppl/gaussian_mixture_model.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function gaussian_mixture_model()
    x = [0.2,1.0,0.5,0.6]
    n = length(x)

    p ~ Beta(0.5, 1)
    a ~ Uniform(-1, 1)
    b ~ Uniform(-1, 1)

    z = tzeros(n)
    x = tzeros(n)
    for i in 1:n
        z[i] ~ Bernoulli(p)
        if z[i] == 1
            x[i] ~ Normal(a, 1.0)
        else
            x[i] ~ Normal(b, 1.0)
        end
    end

    a_is_greater_than_b ~ Dirac(a > b)

end

model = gaussian_mixture_model()

num_chns = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), MCMCThreads(), 100_000, num_chns)
# chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, MH(), 100_000)

# chns = sample(model, PG(15), MCMCThreads(), 10_000, num_chns)

# chns = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chns)
chns = sample(model, SMC(), 40_000)
# chns = sample(model, IS(), 10_000)

#
# chns = sample(model, Gibbs(HMC(0.1,5,:a,:b),PG(15,:p,:a_is_greater_than_b)), 10_000)
# chns = sample(model, Gibbs(NUTS(1000,0.65,:a,:b),PG(15,:p,:a_is_greater_than_b)), 10_000)


display(chns)
# display(plot(chns))

# show_var_dist_pct(chns,:a,20)
# show_var_dist_pct(chns,:b,20)
# show_var_dist_pct(chns,:p,20)

show_var_dist_pct(chns,:a_is_greater_than_b)
