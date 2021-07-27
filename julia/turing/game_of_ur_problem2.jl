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

@model function game_of_ur_problem2()

    function roll1(a)
        ss = sum(a)
        if ss == 13
            return  a
        elseif ss > 13
            return []
        else
            t = rand(DiscreteUniform(0,4))
            return roll1(vcat(a,t))
        end
    end

    # Simpler
    function roll(s,len)
        if s == 13
            return  len
        elseif s > 13
            return 0
        else
            t = rand(DiscreteUniform(0,4))
            return roll(s+t,len+1)
        end
    end
 
    len ~ Dirac(roll(0,0))
    # Skip length 0 events
    true ~ Dirac(len > 0)    

end


model = game_of_ur_problem2()

num_chns = 4

# chns = sample(model, Prior(), MCMCThreads(), 10_000, num_chns)

# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(20), 1_000)
chns = sample(model, SMC(1000), 40_000)
# chns = sample(model, IS(), 10_000)


display(chns)
# display(plot(chns))

println("Distribution of len")
show_var_dist_pct(chns, :len)
