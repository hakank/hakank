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
    From https://towardsdatascience.com/estimating-probabilities-with-bayesian-modeling-in-python-7144be007815
    """
    Species: lions    Prevalence: 44.44%.
    Species: tigers   Prevalence: 33.33%.
    Species: bears    Prevalence: 22.22%.
    """

   Distributions of variable animal7
   lion       =>    4526  (0.452600)
   tiger      =>    3251  (0.325100)
   bear       =>    2223  (0.222300)

   Cf ~/blog/lions_tigers_and_bears.blog
      ~/webppl/lions_tigers_and_bears.wppl


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
    probLion  ~ Dirac(x[1])
    probTiger ~ Dirac(x[2])
    probBear  ~ Dirac(x[3])

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

    animal7 ~ Dirac(o[7])
    
    # probbear7 ~ Dirac(o[7] == bear)
    # problion7 ~ Dirac(o[7] == lion)
    # probtiger7 ~ Dirac(o[7] == tiger)

end


model = lions_tigers_and_bears()
num_chains = 4

# chains = sample(model, Prior(), 10_000)

# chains = sample(model, MH(), 10_000)
chains = sample(model, PG(15), 10_000)
# chains = sample(model, SMC(1000), 10_000)
# chains = sample(model, IS(), 10_000)

#
display(chains)

show_var_dist_pct(chains,:animal7,["lion","tiger","bear"])
