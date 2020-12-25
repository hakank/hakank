#=
  Monty Hall problem

  From the PyMC3 model in the talk
  "Carlo Revolution is Open Source: Probabilistic Programming with PyMC3?Austin Rochford"
  https://www.safaribooksonline.com/videos/open-data-science/9780135432792/9780135432792-ODSC_11
  Around time 9:00


  Which mean that if we selected d1 it will be 1/3 chance of being the price door.
  Changing to d3 would - however - give a 2/3 change of getting the price.

  See ~/blog/monty_hall.blog
      ~/psi/monty_hall.psi
      ~/webppl/monty_hall.wppl

=#

# using Memoization
using Turing # , StatsPlots, DataFrames
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)
include("jl_utils.jl")


@model function monty_hall()
    # There are 3 doors.
    d1 = 1
    d2 = 2
    d3 = 3

    # The prize can be behind any door 1..3.
    prize = DiscreteUniform(1,3)

    # Which door will Monty open?
    # Assumption (WLOG): We always select door 1.
    prize ~ DiscreteUniform(1,3)
    open  ~ DiscreteUniform(1,3);
    open ~ (prize == d1) ? Categorical([0.0, 0.5,0.5]) :
            (prize == d2 ? Categorical([0.0, 0.0,1.0]) : Categorical([0.0, 1.0,0.0]))

    # We see that Monty opens door 2.
    true ~ Dirac(open == d2)

    # What are the probabilities that the price is behind
    # - door d1 (the one we selected, i.e don't switch)
    # - or door d3 (i.e. switch door)

end

model = monty_hall()
num_chains = 4

# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

chains = sample(model, MH(), 10_000)
# chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, PG(20), 1_000)

# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, IS(), 10_000)

# chains = sample(model, SMC(), 10_000)


# chains = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chains)
# chains = sample(model, Gibbs(MH(:zlabels),NUTS(1000,0.65,:m,:b,:sigma)), MCMCThreads(), 40_000, num_chains)

display(chains)

show_var_dist_pct(chains, :prize)
