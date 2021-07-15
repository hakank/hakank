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
# chains = sample(model, PG(20), 1_000)
chains = sample(model, SMC(1000), 40_000)
# chains = sample(model, IS(), 10_000)


display(chains)
# display(plot(chains))

chains_params = Turing.MCMCChains.get_sections(chains, :parameters)
genq = generated_quantities(model, chains_params)
println("Probability of the combinations of cloudy, sprinkler, rain, wet_grass:")
show_var_dist_pct(genq, 120)
