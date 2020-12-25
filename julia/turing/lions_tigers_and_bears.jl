#=
   From Allen Downey
   https://twitter.com/AllenDowney/status/1063460117029535746
   """
   Today's Bayesian problem of the week: Suppose we visit a wild animal preserve where we
   know that the only animals are lions and tigers and bears, but we don't know how
   many of each there are.

   During the tour, we see 3 lions, 2 tigers, and 1 bear. Assuming that every animal had an equal
   chance to appear in our sample, estimate the prevalence of each species.

   What is the probability that the next animal we see is a bear?
   """

   Also see: https://towardsdatascience.com/estimating-probabilities-with-bayesian-modeling-in-python-7144be007815


   Cf ~/blog/lions_tigers_and_bears.blog
      ~/webppl/lions_tigers_and_bears.wppl

    From https://towardsdatascience.com/estimating-probabilities-with-bayesian-modeling-in-python-7144be007815
    """
    Species: lions    Prevalence: 44.44%.
    Species: tigers   Prevalence: 33.33%.
    Species: bears    Prevalence: 22.22%.
    """

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function lions_tigers_and_bears()
    lion = 1
    tiger = 2
    bear = 3

    # Prior
    # probLion  ~ Beta(1,1)
    # probTiger ~ Beta(1,1)
    # probBear  ~ Beta(1,1)

    # The Dirichlet distribution ensures that the sum of probabilities is 1
    # i.e. we don't have to ensure this via some specific constraint.
    v = [3, 2, 1] # We saw three lions, two tigers, and one bear
    # v = [2,2,2]
    x ~ Dirichlet(v)

    # The probabilities to calculate ("aliased" for simplicity)
    probLion  = x[1]
    probTiger = x[2]
    probBear  = x[3]

    n = 7
    o = tzeros(n)
    for i in 1:n
        o[i] ~ Categorical([probLion,probTiger,probBear])
    end

    # It shouldn't matter in what order we see the different animals.
    true ~ Dirac(o[1] == lion)
    true ~ Dirac(o[2] == lion)
    true ~ Dirac(o[3] == lion)
    true ~ Dirac(o[4] == tiger)
    true ~ Dirac(o[5] == tiger)
    true ~ Dirac(o[6] == bear)

    # return o[7] == lion
    # return o[7] == tiger
    return o[7] == bear

end


model = lions_tigers_and_bears()
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
show_var_dist_pct(chains,:len,1000)


genq = generated_quantities(model, chains)
show_var_dist_pct(genq,1000)
