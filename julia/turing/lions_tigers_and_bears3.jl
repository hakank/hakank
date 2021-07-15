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
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)
include("jl_utils.jl")


@model function lions_tigers_and_bears()
    lion = 1
    tiger = 2
    bear = 3

    # Prior
    # We have a prior of alphas for the Dirichlet distribution.
    # We draw 6 times with the Multinomial distrib with the Dirichlet prior.
    # What is the probability of different combinations of the number of each animal?

    # alphas = [1/3, 1/3, 1/3] # Highest prob x: [3, 2, 1]       0.2920000000000001
    alphas = [3/6, 2/6, 1/6] # Highest prob x: [3, 2, 1]       0.4446000000000001
    # alphas = [2/6, 2/6, 2/6] # Highest prob x: [3, 2, 1]       0.26220000000000004
    # alphas = [1/6, 2/6, 3/6] # Highest prob x: [2, 2, 2]!      0.2613
    # alphas = [7/21, 7/21, 7/21] # Highest prob x: [2, 2, 2]       0.23359999999999997

    # Draw 6 animals
    n = 6
    # Using Multinomial seems to be much slower than using Dirichlet
    x ~ Multinomial(n,alphas)

    # The probabilities to calculate ("aliased" for simplicity)
    probLion  = x[1]
    probTiger = x[2]
    probBear  = x[3]

    n = 7
    o = tzeros(n)
    for i in 1:n
        o[i] ~ Categorical(simplex([probLion,probTiger,probBear]))
    end

    # It shouldn't matter in what order we see the different animals.
    true ~ Dirac(o[1] == lion)
    true ~ Dirac(o[2] == lion)
    true ~ Dirac(o[3] == lion)
    true ~ Dirac(o[4] == tiger)
    true ~ Dirac(o[5] == tiger)
    true ~ Dirac(o[6] == bear)

    probbear7 ~ Dirac(o[7] == bear)
    problion7 ~ Dirac(o[7] == lion)
    probtiger7 ~ Dirac(o[7] == tiger)


end


model = lions_tigers_and_bears()
num_chains = 4

# chains = sample(model, Prior(), 10_000)

# chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 1_000)
# chains = sample(model, SMC(1000), 10_000)
chains = sample(model, IS(), 10_000)

#
display(chains)

show_var_dist_pct(chains,:probbear7)
show_var_dist_pct(chains,:problion7)
show_var_dist_pct(chains,:probtiger7)
