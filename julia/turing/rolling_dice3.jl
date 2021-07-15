#=
  From https://dtai.cs.kuleuven.be/problog/tutorial/basic/03_dice.html
  """
  We now consider yet another way to model dice, using fair ones only. This representation
  allows for convenient use of the results in arithmetic expressions, e.g., to add up the
  results from several dice. We query for the probabilities of the possible sums we can get
  from two dice given that the first number is even and the second odd.
  """

   Distributions of variable dice[1] (num:0)
   4.00000 =>    3357  (0.335700)
   2.00000 =>    3343  (0.334300)
   6.00000 =>    3300  (0.330000)
   Distributions of variable dice[2] (num:0)
   5.00000 =>    3569  (0.356900)
   1.00000 =>    3251  (0.325100)
   3.00000 =>    3180  (0.318000)
   Distributions of variable s (num:0)
   7.00000 =>    3402  (0.340200)
   9.00000 =>    2239  (0.223900)
   5.00000 =>    2114  (0.211400)
   11.00000 =>    1160  (0.116000)
   3.00000 =>    1085  (0.108500)



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

    s ~ Dirac(sum(dice))

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

end

model = rolling_dice3()

num_chains = 2

# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, MH(), 10_000)
chains = sample(model, PG(15), 1_000)
# # chains = sample(model, IS(), 10_000)

chains = sample(model, SMC(), 10_000)

# chains = sample(model, HMC(0.1,10), 10_000) # nope

# chains = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chains)
# chains = sample(model, Gibbs(MH(:zlabels),NUTS(1000,0.65,:m,:b,:sigma)), MCMCThreads(), 40_000, num_chains)

display(chains)

show_var_dist_pct(chains,Symbol("dice[1]")) # Note: :"dice[1]" DON't work!
show_var_dist_pct(chains,Symbol("dice[2]"))
show_var_dist_pct(chains,:s)

