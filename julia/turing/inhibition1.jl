#=
  https://dtai.cs.kuleuven.be/problog/tutorial/basic/10_inhibitioneffects.html
  """
  ...
  In some cases, however, if both causes are present this reduces the likelihood of the effect happening.
  For example when the presence of a second drug blocks the impact of the original drug. In such a case we
  can use negated heads. Depending on the chosen probabilities the effect can be lower than the joint occurance
  of c1 and c2 or be lower then either p1 or p2:
  """

  cf ~/blog/inhibition1.blog
     ~/webppl/inhibition1.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function inhibition1()
    c1 ~ flip(0.5)
    c2 ~ flip(0.5)

    e1 ~ c1 ? flip(0.3) : c2 ? flip(0.2) : flip(0.0)
    e2 ~ c2 ? flip(0.4) : c1 ? flip(0.2) : flip(0.0)
    ee ~ flip(0.5)
    ee ~ Dirac(e1 || e2)

    true ~ Dirac(c1)
    true ~ Dirac(c2)

    return ee

end

model = inhibition1()

num_chains = 4

# chains = sample(model, Prior(), 10_000)

# chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, MH(), 10_000)

# chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)
# chains = sample(model, PG(15), 1_000)

chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, SMC(1000), 10_000)

# chains = sample(model, IS(), 1_000)

#
display(chains)
show_var_dist_pct(chains,:c1,20)
show_var_dist_pct(chains,:c2,20)
show_var_dist_pct(chains,:e1,20)
show_var_dist_pct(chains,:e2,20)

println("\nee:")
genq = generated_quantities(model, chains)
show_var_dist_pct(genq,10)
