#=
  http://www.cs.ubc.ca/~murphyk/Bayes/bnintro.html

  Cf ~/problog/sprinkler.pl
     ~/blog/sprinkler.blog
     ~/psi/sprinkler.psi
     ~/webppl/sprinkler.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function sprinkler()
    cloudy ~ flip(0.5)

    sprinkler ~ cloudy ? flip(0.1) : flip(0.5)

    rain ~ cloudy ? flip(0.8) : flip(0.2)

    wet_grass ~ (sprinkler==false && rain==false) ? flip(0.0) :
                (sprinkler==true && rain==false) ? flip(0.9) :
                (sprinkler==false && rain==true) ? flip(0.9) :
                (sprinkler==true && rain==true) ? flip(0.99) : flip(0.0)

    true ~ Dirac(wet_grass == true)
    # true ~ Dirac (wet_grass == false);

    return cloudy, sprinkler, rain, wet_grass
end

model = sprinkler()

num_chains = 4

# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, MH(), 10_000)
# chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(15), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, PG(20), 1_000)

chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, SMC(1000), 40_000)

# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, IS(), 10_000)


# chains = sample(model, NUTS(1000,0.65), 1_000)
# chains = sample(model, HMC(0.1,5), 1_000)
# chains = sample(model, Gibbs(MH(:gender),NUTS(1000,0.65,:height)), 1_000)
# chains = sample(model, Gibbs(MH(:gender),NUTS(10,0.65,:height)), 1_000)
# chains = sample(model, Gibbs(MH(:gender),HMC(0.1,5,:height)), 1_000)
# chains = sample(model, Gibbs(PG(10,:gender),HMC(0.1,5,:height)), 1_000)
# chains = sample(model, Gibbs(MH(:gender),NUTS(1_000,0.65,:height)), 1_000)

display(chains)
# display(plot(chains))

gen = generated_quantities(model, chains)
show_var_dist_pct(gen, 120)
