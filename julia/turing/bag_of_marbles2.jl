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
   Distributions of variable draw[1]
   red        =>  100000  (1.000000)

   Distributions of variable draw[2]
   blue       =>   47339  (0.473390)
   red        =>   29353  (0.293530)
   white      =>   23308  (0.233080)


  Cf ~/cplint/bag_of_marbles.pl
     ~/blog/bag_of_marbles.blog
     ~/psi/bag_of_marbles.psi
     ~/webppl/bag_of_marbles.wppl

  Cf bag_of_marbles.jl. This current model use a for loop.

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

#=
draw1: The probability of drawing white is the probability for 1.0

Distributions of variable draw[2] (num:0)
2.00000 =>  188281  (0.470703)
3.00000 =>  117185  (0.292963)
1.00000 =>   94534  (0.236335)
=#


@model function bag_of_marbles()

    white = 1
    blue  = 2
    red   = 3

    start = [4,8,6]

    draw = tzeros(2)
    for t in 1:2
        # We have to use sum(Int8[...]) since sum([]) throws an error
        draw[t] ~ Categorical(
                     simplex([
                            start[white] - sum([draw[i] == white ? 1 : 0 for i in 1:t-1]),
                            start[blue]  - sum([draw[i] == blue  ? 1 : 0 for i in 1:t-1]),
                            start[red]   - sum([draw[i] == red   ? 1 : 0 for i in 1:t-1])
                            ])
                            )
    end

    true ~ Dirac(draw[1] == red)

end

model = bag_of_marbles()
num_chns = 4

# chns = sample(model, Prior(), 100_000)
chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(),10_000)

display(chns)

println("\ndraw1: The probability of drawing white is the probability for 1.0")
show_var_dist_pct(chns,Symbol("draw[1]"),["white","blue","red"])
show_var_dist_pct(chns,Symbol("draw[2]"),["white","blue","red"])
