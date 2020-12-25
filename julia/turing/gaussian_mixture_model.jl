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

    return a > b

end

model = gaussian_mixture_model()

num_chains = 4

# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), MCMCThreads(), 100_000, num_chains)
# chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, MH(), 10_000)

# chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)

chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, SMC(1000), 10_000)
# chains = sample(model, IS(), 10_000)
#
#chains = sample(model, Gibbs(HMC(0.1,5,:a,:b),PG(15,:p)), 10_000)
# chains = sample(model, Gibbs(NUTS(1000,0.65,:a,:b),PG(15,:p)), 10_000)
# chains = sample(model, Gibbs(HMC(0.1,5,:a,:b),SMC(1000,:p)), 10_000) # Nope

display(chains)
# display(plot(chains))

# show_var_dist_pct(chains,:a,20)
# show_var_dist_pct(chains,:b,20)
# show_var_dist_pct(chains,:p,20)

genq = generated_quantities(model, chains)
show_var_dist_pct(genq,20)
