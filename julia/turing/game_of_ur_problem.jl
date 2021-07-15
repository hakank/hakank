#=
   https://www.allendowney.com/blog/2018/10/21/the-game-of-ur-problem/
   """
   Here’s a probability puzzle to ruin your week.

   In the Royal Game of Ur, players advance tokens along a track with 14 spaces.
   To determine how many spaces to advance, a player rolls 4 dice with 4 sides. Two corners
   on each die are marked; the other two are not. The total number of marked corners —
   which is 0, 1, 2, 3, or 4 — is the number of spaces to advance.

   For example, if the total on your first roll is 2, you could advance a token to space 2.
   If you roll a 3 on the next roll, you could advance the same token to space 5.

   Suppose you have a token on space 13. How many rolls did it take to get there?
   """

   See:
   https://www.allendowney.com/blog/lions-and-tigers-and-bears/

   Allen Downey's solution:
   http://nbviewer.jupyter.org/github/AllenDowney/ThinkBayes2/blob/master/solutions/game_of_ur_soln.ipynb?flush=true

   cf ~/blog/game_of_ur_problem.blog
      ~/webppl/game_of_ur_problem.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function game_of_ur_problem(s=13)
    numRolls ~ DiscreteUniform(3,20)

    zroll = tzeros(numRolls)
    for i in 1:numRolls
        zroll[i] ~ DiscreteUniform(4)
    end

    sumRoll ~ DiscreteUniform(0,4*numRolls)
    sumRoll ~ Dirac(sum(zroll))

    true ~ Dirac(sumRoll == s)

end


model = game_of_ur_problem(13)

num_chains = 4

# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

# Got weird errors for MH()
#   - BoundsError: attempt to access 8-element Array{Int64,1} at index [2:20]
#
# chains = sample(model, MH(), 10_000)
# chains = sample(model, MH(), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(15), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, PG(20), 1_000)

# chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, SMC(1000), 10_000)

# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
chains = sample(model, IS(), 10_000)


# chains = sample(model, NUTS(1000,0.65), 1_000)
# chains = sample(model, HMC(0.1,5), 1_000)
# chains = sample(model, Gibbs(MH(:gender),NUTS(1000,0.65,:height)), 1_000)
# chains = sample(model, Gibbs(MH(:gender),NUTS(10,0.65,:height)), 1_000)
# chains = sample(model, Gibbs(MH(:gender),HMC(0.1,5,:height)), 1_000)
# chains = sample(model, Gibbs(PG(10,:gender),HMC(0.1,5,:height)), 1_000)
# chains = sample(model, Gibbs(MH(:gender),NUTS(1_000,0.65,:height)), 1_000)

display(chains)
# display(plot(chains))

show_var_dist_pct(chains, :numRolls,20)
println()
show_var_dist_pct(chains, :sumRoll,20)
