#=
  From
  "Probabilistic logic programming and its applications"
  Luc De Raedt, Leuven
  https://www.youtube.com/watch?v=3lnVBqxjC88
  @ 3:44

  """
  Mike has a bag of marbles with 4 white, 8 blue, and
  6 red marbles. He pulls out one marble from the bag
  and it is red. What is the probability that the
  second marble he pulls out of the bag is white?

  The answer is 0.234931.
  """

  draw1: The probability of drawing white is the probability for 1.0
  Distributions of variable draw1
  blue       =>   47204  (0.472040)
  red        =>   29283  (0.292830)
  white      =>   23513  (0.235130)


  Cf ~/cplint/bag_of_marbles.pl
     ~/blog/bag_of_marbles.blog
     ~/psi/bag_of_marbles.psi
     ~/webppl/bag_of_marbles.wppl

  Also see bag_of_marbles2.jl for an alternative approach (using recursion).

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bag_of_marbles()

    white = 1
    blue  = 2
    red   = 3
    colors = [white,blue,red]

    start = [4,8,6];

    draw0 ~ Categorical(simplex([start[white],start[blue],start[red]]))


    if draw0 == white
        draw1 ~ Categorical(simplex([start[white]-1,start[blue],start[red]]))
    elseif draw0==blue
        draw1 ~ Categorical(simplex([start[white],start[blue]-1,start[red]]))
    else
        # red
        draw1 ~ Categorical([start[white],start[blue],start[red]-1]|>simplex);
    end

    true ~ Dirac(draw0 == red)

end

model = bag_of_marbles()
num_chains = 4

# chains = sample(model, Prior(), 100_000)
chains = sample(model, MH(), 100_000)

# chains = sample(model, PG(15), 10_000)
# chains = sample(model, SMC(), 10_000)

# Note: IS don't generate chains the same way as MH, PG, and SMC!
# chains = sample(model, IS(), 10_000)

display(chains)

show_var_dist_pct(chains,:draw0,["white","blue","red"])
println("\ndraw1: The probability of drawing white is the probability for 1.0")
show_var_dist_pct(chains,:draw1,["white","blue","red"])
