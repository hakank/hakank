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
                            start[white] - sum(Int8[draw[i]==white for i in 1:t-1]),
                            start[blue]  - sum(Int8[draw[i]==blue  for i in 1:t-1]),
                            start[red]   - sum(Int8[draw[i]==red   for i in 1:t-1])
                            ])
                            )
    end

    true ~ Dirac(draw[1] == red)

    return draw[2]==white

end

model = bag_of_marbles()
num_chains = 4

# HH has problem with this!
chains = sample(model, MH(), MCMCThreads(), 100_000, num_chains)
# chains = sample(model, MH(), 10_000)

# chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)
# chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, SMC(1000), 10_000)

# Note: IS don't generate chains the same way as MH, PG, and SMC!
# chains = sample(model, IS(), MCMCThreads(), 1000, num_chains)

display(chains)

show_var_dist_pct(chains,Symbol("draw[1]"))
println("\ndraw1: The probability of drawing white is the probability for 1.0")
show_var_dist_pct(chains,Symbol("draw[2]"))

# println("\ndraw1==white:")
# genq = generated_quantities(model,chains)
# show_var_dist_pct(genq,20)
# println("quantile:")
# println(quantile(vec(genq), [0.0, 0.025, 0.25, 0.5, 0.75, 0.975, 1.0]))
