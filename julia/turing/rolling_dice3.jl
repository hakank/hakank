#=
  From https://dtai.cs.kuleuven.be/problog/tutorial/basic/03_dice.html
  """
  We now consider yet another way to model dice, using fair ones only. This representation
  allows for convenient use of the results in arithmetic expressions, e.g., to add up the
  results from several dice. We query for the probabilities of the possible sums we can get
  from two dice given that the first number is even and the second odd.
  """

  See ~/blog/rolling_dice3.blog
      ~/psi/rolling_dice3.psi
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function rolling_dice3()

    # d1 ~ DiscreteUniform(1,6)
    # d2 ~ DiscreteUniform(1,6)
    num_dice = 2
    dice = tzeros(2)
    for d in 1:num_dice
        dice[d] ~ DiscreteUniform(1,6)
    end

    s = sum(dice)
    # s ~ Dirac(sum(dice))
    # Dirac(s == 7)

    # Nice!
    function odd(d)
        return d % 2 == 1
    end

    # Nicer!
    function even(d)
        return !odd(d)
    end

    true ~ Dirac(even(dice[1]))
    true ~ Dirac(odd(dice[2]))

    return s
end

model = rolling_dice3()

num_chains = 2

# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, MH(), 10_000)
chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(15), MCMCThreads(), 1000, num_chains)
# chains = sample(model, PG(15), 1_000)

# chains = sample(model, IS(), MCMCThreads(), 1000, num_chains)
# chains = sample(model, IS(), 10_000)

# chains = sample(model, SMC(), 10_000)

# chains = sample(model, HMC(0.1,10), 10_000) # nope

# chains = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chains)
# chains = sample(model, Gibbs(MH(:zlabels),NUTS(1000,0.65,:m,:b,:sigma)), MCMCThreads(), 40_000, num_chains)

display(chains)

show_var_dist_pct(chains,Symbol("dice[1]")) # Note: :"dice[1]" DON't work!
show_var_dist_pct(chains,Symbol("dice[2]"))
# show_var_dist_pct(chains,Symbol("s"))

println("\nd1 + d2 (s):")
show_var_dist_pct(generated_quantities(model,chains),20)
